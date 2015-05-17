/* Goals:
    - constant / value propagation
    - all VarDecl stmts have been replaced with Scope blocks and assignments
    - all control flow is replaced with goto and conditional goto
    - "hoisting" of var and named functions
    - almost all static semantics checks are here
    - inline most IIFEs (important for escape analysis)
    - the distinction between statements and expressions is now gone
 */

/* Statements not output by JEX - they are replaced with various gotos, labels,
   and the "@retval" variable.

     Break, Continue
     Return
     For ForIn ForOf DoWhile While
     Switch
 */
typedef union JEX JEX;

void jex_free (JEX *jex);

typedef enum
{
  JEX_CONSTANT_TYPE_STRING,
  JEX_CONSTANT_TYPE_NUMBER,
} JEX_ConstantType;

typedef struct {
  JEX_ConstantType type;
  union {
    JS_String *v_string;
    double v_number;
  } info;
} JEX_Constant;

typedef enum
{
  JEX_TYPE_GROUP,
  JEX_TYPE_GOTO,
  JEX_TYPE_GOTO_IF,
  JEX_TYPE_GOTO_TABLE,
  JEX_TYPE_LABEL,
  JEX_TYPE_WITH,
  JEX_TYPE_EMPTY,
  JEX_TYPE_TRY_CATCH,
  JEX_TYPE_THROW,
  JEX_TYPE_RETURN,
  JEX_TYPE_UNARY_OP,
  JEX_TYPE_BINARY_OP,
  JEX_TYPE_DOT,
  JEX_TYPE_INDEX,
  JEX_TYPE_INVOKE,
  JEX_TYPE_ASSIGN,
  JEX_TYPE_FUNCTION_VALUE,
  JEX_TYPE_ARROW_VALUE,
  JEX_TYPE_NUMBER_VALUE,
  JEX_TYPE_STRING_VALUE,
  JEX_TYPE_REGEX_VALUE,
  JEX_TYPE_TEMPLATE_VALUE,
  JEX_TYPE_ARRAY_VALUE,
  JEX_TYPE_OBJECT_VALUE,
  JEX_TYPE_LOCAL_VAR,
  JEX_TYPE_GLOBAL_VAR,
  JEX_TYPE_NEW
} JEX_Type;


typedef struct JEX_Base {
  JEX_Type type;
  JEX *prev_sibling;
  JEX *next_sibling;
} JEX_Base;

typedef struct JEX_Group {
  JEX_Base base;
  JEX *first_child;
  JEX *last_child;

  JS_Boolean is_scope;                  // scope for let/const variables

  JEX_Var *first_scope_var;
  JEX_Var *last_scope_var;

  JEX *optional_terminal;
} JEX_Group;

JEX *jex_group_new ();
void jex_group_add_child (JEX *group, JEX *child);
void jex_group_remove_child (JEX *group, JEX *child);

// Creates a terminal no-op
JEX *jex_group_get_terminal (JEX *group);

typedef struct JEX_Empty {
  JEX_Base base;
} JEX_Empty;

typedef struct JEX_Goto {
  JEX_Base base;
  JEX *target;
} JEX_Goto;

JEX *jex_goto_new ();
void jex_goto_set_target (JEX *gotojex, JEX *target);

typedef struct JEX_GotoIf {
  JEX_Base base;
  JEX_Var *condition;
  JS_Boolean goto_if_true;
  JEX *target;
} JEX_GotoIf;
JEX *jex_goto_if_new (JEX_Var *condition, JS_Boolean goto_if_true);

typedef struct JEX_GotoTableEntry {
  JEX_Constant value;
  JEX *target;
} JEX_GotoTableEntry;

typedef struct JEX_GotoTable {
  JEX_Base base;
  JEX_Var *switch_value;
  size_t n_entries;
  JEX_GotoTableEntry *entries;
} JEX_GotoTable;

typedef struct JEX_Label {
  JEX_Base base;
  JS_String *name;
} JEX_Label;

////typedef struct {
////  JAST_Base_Statement base;
////  JEX *expr;
////  JEX *body;
////} JEX_With;

typedef struct {
  JEX_Base base;
  JEX *body;
  JS_String *exception_var;
  JEX *catch_body;
  JEX *finally_body;
} JEX_TryCatch;

typedef struct {
  JEX_Base base;
  JEX *throw_expr;
} JEX_Throw;

typedef struct {
  JEX_Base base;
  JEX *expr;
} JEX_Return;

typedef struct
{
  JEX_Base base;
  JAST_UnaryOp_Type op;
  JEX *sub;
} JEX_UnaryOp;

typedef struct
{
  JEX_Base base;
  JAST_BinaryOp_Type op;
  JEX *subs[2];
} JEX_BinaryOp;

typedef struct
{
  JEX_Base base;
  JAST_Expr *container;
  JS_String *member_name;
} JEX_Dot;

typedef struct
{
  JEX_Base base;
  JAST_Expr *container;
  JAST_Expr *index;
} JEX_Index;

typedef struct
{
  JEX_Base base;
  JAST_Expr *function;
  size_t n_args;
  JAST_Expr **args;
} JEX_Invoke;

typedef struct 
{
  JEX_Base base;
  JAST_Expr *lhs, *rhs;
} JEX_Assign;

typedef struct
{
  JEX_Base base;
  JS_String *opt_name;
  unsigned n_args;
  JEX_Var **args;
  JEX *body;
} JEX_FunctionValue;

typedef struct
{
  JEX_Base base;
  size_t n_args;
  JS_String **args;
  JEX *body;         // either RETURN (implicit return); or COMPOUND
} JEX_ArrowValue;

typedef struct
{
  JEX_Base base;
  double value;
} JEX_NumberValue;

typedef struct
{
  JEX_Base base;
  JS_String *value;
} JEX_StringValue;

typedef struct
{
  JEX_Base base;
  struct JS_Regex *regex;
} JEX_RegexValue;

typedef struct 
{
  JAST_TemplatePiece_Type type;
  union {
    JS_String *string;
    JEX *expr;
  } info;
} JEX_TemplatePiece;

typedef struct
{
  JEX_Base base;
  size_t n_pieces;
  JEX_TemplatePiece *pieces;
} JEX_TemplateValue;

typedef struct
{
  JEX_Base base;
  unsigned n_values;
  JEX **values;
} JEX_ArrayValue;

typedef struct
{
  JS_String *key;
  JEX *computed_key;
  JEX *value;
} JEX_ObjectFieldValue;

typedef struct
{
  JEX_Base base;
  unsigned n_fields;
  JEX_ObjectFieldValue *fields;
} JEX_ObjectValue;

typedef struct
{
  JEX_Base base;
  JEX_Var *var;
} JEX_Identifier;

typedef struct
{
  JEX_Base base;
  JEX *ctor;
  unsigned n_args;
  JEX **args;
} JEX_New;

union JEX {
  JEX_Type type;
  JEX_Base base;

  JEX_Group v_group;
  JEX_Goto v_goto;
  JEX_GotoIf v_goto_if;
  JEX_GotoTable v_goto_table;
  JEX_Label v_label;
  JEX_With v_with;
  JEX_TryCatch v_try_catch;
  JEX_Throw v_throw;
  JEX_Return v_return;
  JEX_UnaryOp v_unary_op;
  JEX_BinaryOp v_binary_op;
  JEX_Dot v_dot;
  JEX_Index v_index;
  JEX_Invoke v_invoke;
  JEX_Assign v_assign;
  JEX_FunctionValue v_function_value;
  JEX_ArrowValue v_arrow_value;
  JEX_NumberValue v_number_value;
  JEX_StringValue v_string_value;
  JEX_RegexValue v_regex_value;
  JEX_TemplateValue v_template_value;
  JEX_ArrayValue v_array_value;
  JEX_ObjectValue v_object_value;
  JEX_Identifier v_identifier;
  JEX_New v_new;
};

JEX      *jex_compile_expr      (JEX_Context *context,
                                 JAST_Expr   *expr,
                                 JEX_Var    **var_out_opt);
JEX      *jex_compile_statement (JEX_Context *context,
                                 JAST_Statement *stmt);

// NOTE: By this phase, "var a = 1" has been hoisted into "let a" and "a = 1".
typedef enum {
  JEX_VAR_CONSTANT_VALUE,
  JEX_VAR_TMP,
  JEX_VAR_LET,
  JEX_VAR_CONST,
} JEX_Var_Type;
typedef struct JEX_Var_ConstantValue
{
  JEX_Var_Type type;
  JS_Value *value;
} JEX_Var_ConstantValue;

typedef struct JEX_Var_Scoped
{
  JEX_Var_Type type;
  JS_String *name;
  JEX *scope;
  JEX_Var *prev_in_scope;
  JEX_Var *next_in_scope;
} JEX_Var_Scoped;

typedef struct JEX_Var_Tmp
{
  JEX_Var_Type type;
} JEX_Var_Tmp;
  

typedef union JEX_Var {
  JEX_Var_Type type;
  JEX_Var_ConstantValue v_constant_value;
  JEX_Var_Tmp v_tmp;
  JEX_Var_Scoped v_let;
  JEX_Var_Scoped v_const;
} JEX_Var;

/* --- Constant Propagation Section --- */
JS_Boolean
jex_constprop_is_foldable_expr_type (JEX_Type type);

JS_Boolean
jex_constprop_get_value (JEX *expr, JEX_Constant *out);

JAST_Expr *
jex_constprop_fold_unary_op (JAST_UnaryOp_Type op, JEX *const_sub);

JAST_Expr *
jex_constprop_fold_binary_op (JAST_BinaryOp_Type op, JEX *const_sub1, JEX *const_sub2);

/* Examples: isNaN, parseInt */
JS_Boolean jex_constprop_is_global_foldable_func (JS_String func_name);

/* Example: Math.sin. */
JS_Boolean jex_constprop_is_ns_foldable_func (JS_String namespace_name,
                                              JS_String func_name);


/* --- Context manipulation --- */
/* For use by jex_masticate. */
void     jex_context_push_scope       (JEX_Context *context,
                                       JS_String   *opt_name,
                                       JEX         *continue_target,
                                       JEX         *break_target);
void     jex_context_pop_scope        (JEX_Context *context);
JEX_Var *jex_context_lookup_var       (JEX_Context *context,
                                       JS_String   *name);
JEX_Var *jex_context_alloc_var        (JEX_Context *context,
                                       JS_String   *name);


/* --- allocators --- */
JEX * jex_new_local_var      (JEX_Var      *var);
JEX * jex_new_global         (JS_String    *name);
JEX * jex_new_constant_take  (JEX_Constant *value);
