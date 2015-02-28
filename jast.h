// Javascript Abstract Syntax Tree.

typedef union JAST_Statement JAST_Statement;
typedef union JAST_Expr JAST_Expr;

typedef struct JAST_FieldBindingPattern JAST_FieldBindingPattern;
typedef struct JAST_BindingPattern JAST_BindingPattern;

#include "js-string.h"

/* Compilation Utilities. */
typedef struct {
  JS_String *filename;
  unsigned line_number;
} JAST_Position;

typedef enum {
  JAST_PARSE_ERROR_BAD_CHARACTER,
  JAST_PARSE_ERROR_BAD_TOKEN,
  JAST_PARSE_ERROR_PREMATURE_EOF,
  JAST_PARSE_ERROR_SEMANTICS
} JAST_ParseError_Type;

typedef struct {
  JAST_ParseError_Type type;
  JAST_Position position;
  char *message;
} JAST_ParseError;
char *jast_parse_error_to_string (const JAST_ParseError *error);
void  jast_parse_error_free      (JAST_ParseError       *error);


/* --- binding patterns - like lvalues, but for variable declarations --- */

/* ECMAScript6 allows patterns on the "left-hand-side"
   of a declaration (as distinct from an LValue expression,
   like "x" in "x += 1").

   Sometimes this is called "destructuring" because it breaks
   object elements out of structures.  It does not throw exceptions,
   instead leaving unmatched patterns "undefined".

   These can always be unravelled in code - it's just syntactic sugar.
   For example:

      let {a:b=1, c, d:[e,f], g:{...}} = some_object;

   and it has semantics similar to the following, but better:

         a = some_object.b || 1;   /// not correct if some_object.b==0!!!
         c = some_object.c;
         d = some_object. XXX

   These are handled with "Binding Patterns".

   They are used in various places:

       (1) formal parameter lists, as in:
              function({{a,b,c}) {
                return a*b + c;
              }

       (2) variable declaration:
              let {a,b,c} = options;

 */

typedef enum
{
  JAST_BINDING_PATTERN_NONE,
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
      JAST_BindingPattern *subs;
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
  JAST_Expr *computed_name;
  JAST_BindingPattern binding;
};

/* --- Statements --- */
typedef enum {
  JAST_STATEMENT_COMPOUND,
  JAST_STATEMENT_IF,
  JAST_STATEMENT_SWITCH,
  JAST_STATEMENT_FOR,
  JAST_STATEMENT_FOR_IN,                /* includes "for...of..." */
  JAST_STATEMENT_WHILE,
  JAST_STATEMENT_DO_WHILE,
  JAST_STATEMENT_WITH,
  JAST_STATEMENT_VARIABLE_DECLARATIONS,
  JAST_STATEMENT_TRY_CATCH,
  JAST_STATEMENT_THROW,
  JAST_STATEMENT_LABEL,
  JAST_STATEMENT_BREAK,
  JAST_STATEMENT_CONTINUE,
  JAST_STATEMENT_RETURN,
  JAST_STATEMENT_EXPRESSION
} JAST_StatementType;

typedef struct JAST_Base_Statement {
  JAST_StatementType type;
  JAST_Position position;
} JAST_Base_Statement;

typedef struct JAST_Compound_Statement {
  JAST_Base_Statement base;
  size_t n_subs;
  JAST_Statement **subs;
} JAST_Compound_Statement;

typedef struct {
  JAST_Expr *expr;
  JAST_Statement *statement;
} JAST_ConditionalStatement_Clause;

typedef struct {
  JAST_Base_Statement base;
  size_t n_conditional_statements;
  JAST_ConditionalStatement_Clause *conditional_statements;
  JAST_Statement *else_statement;
} JAST_If_Statement;

typedef enum
{
  JAST_SWITCH_CLAUSE_CASE,
  JAST_SWITCH_CLAUSE_STATEMENT,
  JAST_SWITCH_CLAUSE_DEFAULT,
} JAST_Switch_Clause_Type;

typedef struct
{
  JAST_Switch_Clause_Type clause_type;
  union {
    JAST_Expr *case_value;
    JAST_Statement *statement;
  } info;
} JAST_Switch_Clause;

typedef struct {
  JAST_Base_Statement base;
  JAST_Expr *expr;
  unsigned n_clauses;
  JAST_Switch_Clause *clauses;
} JAST_Switch_Statement;

typedef struct {
  JAST_Base_Statement base;
  JAST_Statement *initial;
  JAST_Expr *condition;
  JAST_Expr *advance;
  JAST_Statement *body;
} JAST_For_Statement;

typedef struct {
  JAST_Base_Statement base;
  JAST_BindingPattern binding;
  JS_Boolean is_for_in;             /* if !for_in, it's a for-of loop */
  JAST_Expr *container;
  JAST_Statement *body;
} JAST_ForIn_Statement;

typedef struct {
  JAST_Base_Statement base;
  JAST_Expr *condition;
  JAST_Statement *body;
} JAST_While_Statement;

typedef struct {
  JAST_Base_Statement base;
  JAST_Statement *body;
  JAST_Expr *condition;
} JAST_DoWhile_Statement;

typedef struct {
  JAST_Base_Statement base;
  JAST_Expr *expr;
  JAST_Statement *body;
} JAST_With_Statement;

typedef struct {
  JAST_Base_Statement base;
  JAST_Statement *body;
  JS_String *exception_var;
  JAST_Statement *catch_body;
  JAST_Statement *finally_body;
} JAST_TryCatch_Statement;

typedef struct {
  JAST_Base_Statement base;
  JAST_Expr *throw_expr;
} JAST_Throw_Statement;

typedef struct {
  JAST_Base_Statement base;
  JS_String *label;   // optional
} JAST_Break_Statement;

typedef JAST_Break_Statement JAST_Continue_Statement;


typedef struct {
  JAST_Base_Statement base;
  JS_String *label;
} JAST_Label_Statement;

typedef struct {
  JAST_Base_Statement base;
  JS_String *label;
} JAST_Goto_Statement;

typedef enum
{
  JAST_VARIABLE_DECLARATION_NONE,
  JAST_VARIABLE_DECLARATION_VAR,
  JAST_VARIABLE_DECLARATION_LET,
  JAST_VARIABLE_DECLARATION_CONST,
} JAST_VariableDeclaration_Type;

typedef struct {
  JAST_Base_Statement base;
  JAST_VariableDeclaration_Type type;
  size_t n_vars;
  JAST_BindingPattern *vars;
} JAST_VariableDeclarations_Statement;

typedef struct {
  JAST_Base_Statement base;
  JAST_Expr *expr;
} JAST_Expression_Statement;

typedef struct {
  JAST_Base_Statement base;
  JAST_Expr *expr;
} JAST_Return_Statement;

union JAST_Statement {
  JAST_StatementType type;
  JAST_Base_Statement base_statement;
  JAST_Compound_Statement compound_statement;
  JAST_If_Statement if_statement;
  JAST_Switch_Statement switch_statement;
  JAST_For_Statement for_statement;
  JAST_ForIn_Statement for_in_statement;
  JAST_While_Statement while_statement;
  JAST_DoWhile_Statement do_while_statement;
  JAST_With_Statement with_statement;
  JAST_TryCatch_Statement try_catch_statement;
  JAST_Throw_Statement throw_statement;
  JAST_Label_Statement label_statement;
  JAST_Expression_Statement expr_statement;
  JAST_Break_Statement break_statement;
  JAST_Return_Statement return_statement;
  JAST_Continue_Statement continue_statement;
  JAST_VariableDeclarations_Statement vardecls_statement;
  JAST_Goto_Statement goto_statement;
};

void jast_statement_free (JAST_Statement *stmt);

/* --- Expressions --- */
typedef enum
{
  JAST_EXPR_UNARY_OP,
  JAST_EXPR_BINARY_OP,
  JAST_EXPR_COND,
  JAST_EXPR_DOT,
  JAST_EXPR_INDEX,
  JAST_EXPR_INVOKE,
  JAST_EXPR_FUNCTION_VALUE,
  JAST_EXPR_ARROW,
  JAST_EXPR_OBJECT_VALUE,
  JAST_EXPR_ARRAY_VALUE,
  JAST_EXPR_STRING_VALUE,
  JAST_EXPR_REGEX_VALUE,
  JAST_EXPR_TEMPLATE,
  JAST_EXPR_NUMBER_VALUE,
  JAST_EXPR_BOOLEAN_VALUE,
  JAST_EXPR_UNDEFINED_VALUE,
  JAST_EXPR_NULL_VALUE,
  JAST_EXPR_IDENTIFIER
} JAST_Expr_Type;
const char *jast_expr_type_name (JAST_Expr_Type type);
typedef struct
{
  JAST_Expr_Type type;
} JAST_Base_Expr;

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
  JAST_Base_Expr base;
  JAST_UnaryOp_Type op;
  JAST_Expr *sub;
} JAST_UnaryOp_Expr;

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

  JAST_BINARY_OP_COMMA,
  JAST_BINARY_OP_ASSIGN,
  JAST_BINARY_OP_ADD_ASSIGN,
  JAST_BINARY_OP_SUBTRACT_ASSIGN,
  JAST_BINARY_OP_MULTIPLY_ASSIGN,
  JAST_BINARY_OP_DIVIDE_ASSIGN,
  JAST_BINARY_OP_MOD_ASSIGN,
  JAST_BINARY_OP_BITWISE_OR_ASSIGN,
  JAST_BINARY_OP_BITWISE_AND_ASSIGN,
  JAST_BINARY_OP_BITWISE_XOR_ASSIGN,
  JAST_BINARY_OP_CMP_EXACT_EQ,
  JAST_BINARY_OP_CMP_NUM_EQ,
  JAST_BINARY_OP_CMP_EXACT_NE,
  JAST_BINARY_OP_CMP_NUM_NE,
  JAST_BINARY_OP_CMP_LT,
  JAST_BINARY_OP_CMP_LE,
  JAST_BINARY_OP_CMP_GT,
  JAST_BINARY_OP_CMP_GE,
} JAST_BinaryOp_Type;

typedef struct
{
  JAST_Base_Expr base;
  JAST_BinaryOp_Type op;
  JAST_Expr *subs[2];
} JAST_BinaryOp_Expr;

typedef struct
{
  JAST_Base_Expr base;
  JAST_Expr *container;
  JS_String *member_name;
} JAST_Dot_Expr;

typedef struct
{
  JAST_Base_Expr base;
  JAST_Expr *container;
  JAST_Expr *index;
} JAST_Index_Expr;

typedef struct
{
  JAST_Base_Expr base;
  JAST_Expr *function;
  size_t n_args;
  JAST_Expr **args;
} JAST_Invoke_Expr;

typedef struct
{
  JAST_Base_Expr base;
  unsigned n_terms;
  JAST_Expr **terms;
} JAST_Cond_Expr;

typedef struct 
{
  JAST_Base_Expr base;
  JAST_Expr *lhs, *rhs;
} JAST_Assign_Expr;

typedef struct
{
  // XXX: should be BindingPattern.
  JS_String *name;
} JAST_FormalParam;

typedef struct
{
  JAST_Base_Expr base;
  JS_String *opt_name;
  unsigned n_args;
  JAST_FormalParam *args;
  JAST_Statement *body;
} JAST_FunctionValue_Expr;

typedef struct
{
  JAST_Base_Expr base;
  size_t n_args;
  JS_String **args;
  JAST_Statement *body;         // either RETURN (implicit return); or COMPOUND
} JAST_Arrow_Expr;

typedef struct
{
  JAST_Base_Expr base;
  double value;
} JAST_NumberValue_Expr;

typedef struct
{
  JAST_Base_Expr base;
  JS_String *value;
} JAST_StringValue_Expr;

typedef struct
{
  JAST_Base_Expr base;
  struct JS_Regex *regex;
} JAST_RegexValue_Expr;

typedef enum
{
  JAST_TEMPLATE_PIECE_STRING,
  JAST_TEMPLATE_PIECE_EXPR
} JAST_TemplatePiece_Type;

typedef struct 
{
  JAST_TemplatePiece_Type type;
  union {
    JS_String *string;
    JAST_Expr *expr;
  } info;
} JAST_TemplatePiece;

typedef struct
{
  JAST_Base_Expr base;
  size_t n_pieces;
  JAST_TemplatePiece *pieces;
} JAST_Template_Expr;

typedef struct
{
  JAST_Base_Expr base;
  unsigned n_values;
  JAST_Expr **values;
} JAST_ArrayValue_Expr;

typedef struct
{
  JS_String *key;
  JAST_Expr *computed_key;
  JAST_Expr *value;
} JAST_ObjectFieldValue;

typedef struct
{
  JAST_Base_Expr base;
  unsigned n_fields;
  JAST_ObjectFieldValue *fields;
} JAST_ObjectValue_Expr;

typedef struct
{
  JAST_Base_Expr base;
  JS_String *symbol;
} JAST_Identifier_Expr;

typedef struct
{
  JAST_Base_Expr base;
  JAST_Expr *ctor;
  unsigned n_args;
  JAST_Expr **args;
} JAST_New_Expr;

union JAST_Expr {
  JAST_Expr_Type type;
  JAST_UnaryOp_Expr unary_op_expr;
  JAST_BinaryOp_Expr binary_op_expr;
  JAST_Dot_Expr dot_expr;
  JAST_Cond_Expr cond_expr;
  JAST_Index_Expr index_expr;
  JAST_Invoke_Expr invoke_expr;
  JAST_FunctionValue_Expr function_value_expr;
  JAST_Arrow_Expr arrow_expr;
  JAST_NumberValue_Expr number_value_expr;
  JAST_StringValue_Expr string_value_expr;
  JAST_RegexValue_Expr regex_value_expr;
  JAST_Template_Expr template_expr;
  JAST_ArrayValue_Expr array_value_expr;
  JAST_ObjectValue_Expr object_value_expr;
  JAST_Assign_Expr assign_expr;
  JAST_Identifier_Expr identifier_expr;
  JAST_New_Expr new_expr;
};

void jast_expr_free (JAST_Expr *expr);

JAST_Statement *
JAST_parse_file (const char *filename,
                 JAST_ParseError **error);

JAST_Statement *
JAST_parse_data (size_t            data_size,
                 const char       *data,
                 const char       *filename,
                 JAST_ParseError **error);
