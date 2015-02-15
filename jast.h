
// Javascript Abstract Syntax Tree.
typedef union JAST_Statement JAST_Statement;


typedef struct JAST_FieldBindingPattern JAST_FieldBindingPattern;
typedef struct JAST_BindingPattern JAST_BindingPattern;

typedef enum
{
  JAST_BINDING_PATTERN_SIMPLE,
  JAST_BINDING_PATTERN_ARRAY,
  JAST_BINDING_PATTERN_OBJECT,
} JAST_BindingPatternType;

struct JAST_BindingPattern {
  JAST_BindingPatternType type;
  union {
    JS_String *simple;
    struct {
      size_t n_subs;
      JAST_BindingPattern **subs;    /* elements may be NULL */
    } array;
    struct {
      size_t n_fields;
      JAST_FieldBindingPattern *fields;    /* elements may be NULL */
    } object;
  } info;
  JAST_Expr *initializer;
};

struct JAST_FieldBindingPattern {
  JS_String *name;
  JAST_BindingPattern binding;
};

typedef struct {
  unsigned ref_count;
  /* string follows */
} JAST_Filename;

typedef struct {
  JAST_Filename *filename;
  unsigned line_number;
} JAST_Position;

typedef struct JAST_Base_Statement {
  JAST_StatementType type;
  JAST_Position position;
} JAST_Base_Statement;

typedef struct JAST_Compound_Statement {
  JAST_Base_Statement base;
  size_t n_children;
  JAST_Statement **children;
} JAST_Base_Statement;

typedef struct {

struct JAST_BindingPattern {
  JAST_BindingPatternType type;
  union {
    JS_String *simple;
    struct {
      size_t n_subs;
      JAST_BindingPattern **subs;    /* elements may be NULL */
    } array;
    struct {
      size_t n_fields;
      JAST_FieldBindingPattern *fields;    /* elements may be NULL */
    } object;
  } info;
};

struct JAST_FieldBindingPattern {
  JS_String *name;
  JAST_BindingPattern binding;
};

typedef struct {
  unsigned ref_count;
  /* string follows */
} JAST_Filename;

typedef struct {
  JAST_Filename *filename;
  unsigned line_number;
} JAST_Position;

typedef struct JAST_Base_Statement {
  JAST_StatementType type;
  JAST_Position position;
} JAST_Base_Statement;

typedef struct JAST_Compound_Statement {
  JAST_Base_Statement base;
  size_t n_children;
  JAST_Statement **children;
} JAST_Base_Statement;

typedef struct {
  JAST_Expr *expr;
  JAST_Statement *statement;
} JAST_ConditionalStatement_Clause;

typedef struct {
  JAST_Statement_Base base;
  size_t n_conditional_statements;
  JAST_ConditionalStatement_Clause *conditional_statements;
  JAST_Statement *else_statement;
} JAST_Statement_If;

typedef enum
{
  JAST_SWITCH_CLAUSE_CASE,
  JAST_SWITCH_CLAUSE_STATEMENT,
  JAST_SWITCH_CLAUSE_DEFAULT,
} JAST_Switch_ClauseType;
typedef enum
{
  JAST_Switch_ClauseType clause_type;
  union {
    JAST_Expr *case_value;
    JAST_Statement *statement;
  } info;
} JAST_Switch_Clause;

typedef struct {
  JAST_Statement_Base base;
  unsigned n_clauses;
  JAST_Switch_Clause *clauses;
} JAST_Statement_Switch;

typedef struct {
  JAST_Statement_Base base;
  JAST_Statement *initial;
  JAST_Expr *condition;
  JAST_Statement *advance;
  JAST_Statement *body;
} JAST_Statement_For;

typedef struct {
  JAST_Statement_Base base;
  JAST_String *variable;            /// better name for this variable?
  JAST_Expr *container;
  JAST_Statement *body;
} JAST_Statement_ForIn;

typedef struct {
  JAST_Statement_Base base;
  JAST_Expr *condition;
  JAST_Statement *body;
} JAST_Statement_While;

typedef struct {
  JAST_Statement_Base base;
  JAST_Statement *body;
  JAST_Expr *condition;
} JAST_Statement_DoWhile;

typedef struct {
  JAST_Statement_Base base;
  JAST_Expr *expr;
  JAST_Statement *body;
} JAST_Statement_With;

typedef struct {
  JAST_Statement_Base base;
  JAST_Statement *body;
  JAST_String *exception_var;
  JAST_Statement *catch_body;
  JAST_Statement *finally_body;
} JAST_Statement_TryCatch;

typedef struct {
  JAST_Statement_Base base;
  JAST_String *label;
} JAST_Statement_Label;

typedef struct {
  JAST_Statement_Base base;
  JAST_String *label;
} JAST_Statement_Goto;


union {
  JAST_StatementType type;
  JAST_Statement_Base base_statement;
  JAST_Statement_Compound compound_statement;
  JAST_Statement_If if_statement;
  JAST_Statement_Switch switch_statement;
  JAST_Statement_For for_statement;
  JAST_Statement_ForIn for_in_statement;
  JAST_Statement_While while_statement;
  JAST_Statement_DoWhile do_while_statement;
  JAST_Statement_With with_statement;
  JAST_Statement_TryCatch try_catch_statement;
  JAST_Statement_Throw throw_statement;
  JAST_Statement_Label label_statement;
  JAST_Statement_Expr expr_statement;
  JAST_Statement_Let let_statement;
  JAST_Statement_Var var_statement;
  JAST_Statement_Const const_statement;
} JAST_Statement;

typedef struct
{
  JAST_ExprType type;
} JAST_Expr_Base;

typedef enum
{
  JAST_UNARY_OP_PRE_INCR,
  JAST_UNARY_OP_POST_INCR,
  JAST_UNARY_OP_PRE_DECR,
  JAST_UNARY_OP_POST_DECR,
  JAST_UNARY_OP_NEGATE,
  JAST_UNARY_OP_PLUS,
  JAST_UNARY_OP_BITWISE_NOT,
  JAST_UNARY_OP_LOGICAL_NOT
} JAST_UnaryOp_Type;

typedef struct
{
  JAST_Expr_Base base;
  JAST_UnaryOp_Type op;
  JAST_Expr *sub;
} JAST_Expr_UnaryOp;

typedef enum
{
  JAST_BINARY_OP_ADD,
  JAST_BINARY_OP_SUBTRACT,
  JAST_BINARY_OP_MULTIPLY,
  JAST_BINARY_OP_DIVIDE,
  JAST_BINARY_OP_MOD,
  JAST_BINARY_OP_LOGICAL_OR,
  JAST_BINARY_OP_LOGICAL_AND,
  JAST_BINARY_OP_BITWISE_OR,
  JAST_BINARY_OP_BITWISE_AND,
  JAST_BINARY_OP_BITWISE_XOR,
} JAST_BinaryOp_Type;

typedef struct
{
  JAST_Expr_Base base;
  JAST_UnaryOp_Type op;
  JAST_Expr *subs[2];
} JAST_Expr_BinaryOp;

typedef struct
{
  JAST_Expr_Base base;
  JAST_Expr *container;
  JAST_String *member_name;
} JAST_Expr_Dot;

typedef struct
{
  JAST_Expr_Base base;
  JAST_Expr *function;
  size_t n_args;
  JAST_Expr **args;
} JAST_Expr_Invoke;

typedef struct
{
  JAST_Expr_Base base;
  unsigned n_terms;
  JAST_Expr **terms;
} JAST_Expr_Cond;

typedef struct
{
  JAST_String *name;
} JAST_FormalParam;

typedef struct
{
  JAST_Expr_Base base;
  JAST_String *opt_name;
  unsigned n_args;
  JAST_FormalParam *args;
  JAST_Statement *body;
} JAST_Expr_FunctionValue;

typedef struct
{
  JAST_Expr_Base base;
  unsigned n_values;
  JAST_Expr **values;
} JAST_Expr_ArrayValue;

union {
  JAST_ExprType type;
  JAST_Expr_UnaryOp unary_op_expr;
  JAST_Expr_BinaryOp binary_op_expr;
  JAST_Expr_Dot dot_expr;
  JAST_Expr_Bracket bracket_expr;
  JAST_Expr_Invoke invoke_expr;
  JAST_Expr_MethodInvoke invoke_expr;
  JAST_Expr_FunctionValue function_value_expr;
  JAST_Expr_ArrayValue array_value_expr;
  JAST_Expr_ObjectValue object_value_expr;
  JAST_Expr_Assign assign_expr;
  JAST_Expr_MemberAssign member_assign_expr;
  JAST_Expr_IndexAssign index_assign_expr;
  JAST_Expr_New new_expr;
} JAST_Expr;

JAST_Statement_Compound *
JAST_parse_file (const char *filename,
                 JAST_ParseError **error);

JAST_Statement_Compound *
JAST_parse_data (size_t            data_size,
                 const char       *data,
                 const char       *filename,
                 JAST_ParseError **error);
