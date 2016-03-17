
typedef struct _JSUS_Command JSUS_Command;
typedef struct _JSUS_Scope JSUS_Scope;

typedef enum {
  JSUS_VAR_CONSTANT,
  JSUS_VAR_VARIABLE,
  JSUS_VAR_TEMPORARY,
  JSUS_VAR_NONE,          // only used for "optional" variables
} JSUS_Var_Type;

typedef struct {
  JSUS_VAR_Type type;
  union {
    char *name;           // for constant / variable the actual name;
    int id;               // for temporary- the basename
  } info;
} JSUS_Var;

typedef enum {
  JSUS_COMMAND_SCOPE,
  JSUS_COMMAND_BINARY_OP,
  JSUS_COMMAND_UNARY_OP,
  JSUS_COMMAND_GOTO_IF,
  JSUS_COMMAND_GOTO,
  JSUS_COMMAND_NOP,
  JSUS_COMMAND_CREATE_FUNCTION,
  JSUS_COMMAND_INVOKE_FUNCTION,
} JSUS_Command_Type;

struct _JSUS_Command {
  JSUS_Command_Type type;
  JSUS_Command *prev;
  JSUS_Command *next;
};

struct _JSUS_Command_Scope {
  JSUS_Command base;
  JSUS_Command *first;
  JSUS_Command *last;
  unsigned n_vars;
  JSUS_Var *vars;
  unsigned _vars_alloced;
};

struct _JSUS_Command_BinaryOp {
  JSUS_Command base;

  /* "target = a binary_op b" --- "target" is optional */
  JSUS_Var target;
  JSUS_Var a;
  JAST_BinaryOp_Type binary_op;
  JSUS_Var b;
};

struct _JSUS_Command_UnaryOp {
  JSUS_Command base;

  /* "target = unary_op(a)" -- this is for both prefix and postfix ops */
  JSUS_Var target;
  JSUS_Var a;
  JAST_UnaryOp_Type unary_op;
};

struct _JSUS_Command_GotoIf {
  JSUS_Command base;
  JSUS_Command *target;
  JSUS_Var *var;
};

struct _JSUS_Command_CreateFunction {
  JSUS_Command base;
  JSUS_Var target;
  unsigned n_params;
  JSUS_Command_Scope *body;    /* first n_params vars in 'body' are parameters */
};

struct _JSUS_Command_InvokeFunction {
  JSUS_Command base;
  JSUS_Var function;
  unsigned n_params;
  JSUS_Var *params;
};

typedef enum {
  JSUS_VALUE_NUMBER,
  JSUS_VALUE_STRING,
  JSUS_VALUE_OBJECT,
  JSUS_VALUE_ARRAY,
  JSUS_VALUE_NULL,
  JSUS_VALUE_BOOLEAN,
  JSUS_VALUE_UNDEFINED,
} JSUS_Value_Type;

