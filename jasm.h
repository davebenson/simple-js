


typedef enum
{
  // 0=global 1=stack others are captured stack frames on heap
  unsigned scope_index;

  unsigned var_index;
} JASM_Insn_VarRef;

typedef struct 
{
  JS_String *name;              /* optional */
} JASM_ScopeVarSignature;
typedef struct 
{
  size_t scope_size;
  size_t n_vars;
  JASM_ScopeVarSignature *vars;
} JASM_ScopeSignature;

typedef struct
{
  JASM_ScopeSignature *sig;
  /* data follows */
} JASM_Scope;

typedef struct
{
  size_t stack_size;

  ... n_closure_scopes;
  ... closure_scopes;

  JASM_Insn *code;

} JASM_StaticFunction;

typedef struct
{
  
} JASM_Function;

JASM_Function *jasm_function_new_capture (unsigned n_scopes,
                                          JasmScope **scopes);
  

typedef enum
{
  JASM_INSN_CALL,
  JASM_INSN_CALL_C,
  JASM_INSN_PUSH_SCOPE,
  JASM_INSN_POP_SCOPE,
  JASM_INSN_BRANCH,
  JASM_INSN_SET_CONSTANT,
  JASM_INSN_ASSIGN,
} JASM_Insn_Type;

struct _JASM_Insn_Call
{
  JASM_Insn_Base base;

  JASM_Insn_VarRef function;
  unsigned n_args;
  JASM_Insn_VarRef *args;
  JASM_Insn_VarRef *this_var;   /* optional */
  JASM_Insn_VarRef rv;          /* optional */
  JASM_Insn *throw_branch;
};

struct _JASM_Insn_CallC
{
  JASM_Insn_Base base;

  JVInvocationResult (*call) (JV *this_var, size_t n_args, JV **args);
  unsigned n_args;
  JASM_Insn_VarRef *args;
  JASM_Insn_VarRef *this_var;   /* optional */
  JASM_Insn_VarRef rv;          /* optional */
  JASM_Insn *throw_branch;
};

struct _JASM_Insn_Branch
{
  JASM_Insn_Base base;

  JASM_Insn_VarRef condition;
  JASM_Insn *true_branch;
  /* base.next == false_branch */
};

struct _JASM_Insn_SetConstant
{
  JASM_Insn_Base base;

  JASM_Insn_VarRef dest;
  JV source;
};

struct _JASM_Insn_Assign
{
  JASM_Insn_Base base;

  JASM_Insn_VarRef dest;
  JASM_Insn_VarRef source;
};

