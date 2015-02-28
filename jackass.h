
void jackass_static_semantics_check (JAST_Statement *statement,
                                     JackassErrorHandler handler);


JackassRunnable *jackass_compile (JAST_Statement *statement);


/* Transform:  statements are transformed into a graph of basic blocks.

    (1) IIFEs are unravelled

        This is done by jackass_context_compile_expr()
        when it hits an invoke on a function expression.

        Any mechanism which allows the function to "escape" causes the
        unravelling to fail (resorting to unoptimized impl).

    (2) VARs are converted to LETs or CONSTs

    (3) scopes are explicitly allocated - essentially on entrance
        to a { }-block

    (4) in the form of LETs and CONSTs are tagged as stack-based (no escaping into closures)
        or scoped.  For each {}-block there is a "scope signature" that gives the vars
        allocated in that scope.  The scope also has a "depth".

    (5) each basic block has a list of conditional targets (the next blocks)
        as well as a pointer for the default next block.

 */

struct Jackass_Scope_Var
{
  const char *name;             // temp vars start with '#'
};

struct Jackass_Scope_Signature
{
  size_t n_vars;
  size_t vars_alloced;
  struct Jackass_Scope_Var *vars;
  ...
};

typedef struct
{
  Jackass_Instruction_Type type;
} Jackass_Base_Instruction;

typedef union Jackass_Instruction Jackass_Instruction;

union Jackass_Instruction
{
  Jackass_Instruction_Type type;
  Jackass_Base_Instruction base_insn;
  Jackass_AllocScope_Instruction alloc_scope_insn;
  Jackass_DropScope_Instruction drop_scope_insn;
  Jackass_CreateClosure_Instruction create_closure_insn;
  Jackass_CallFunction_Instruction call_function_insn;
  Jackass_GetGlobal_Instruction get_global_insn;
  Jackass_SetGlobal_Instruction set_global_insn;
  Jackass_UnsetGlobal_Instruction unset_global_insn;
  Jackass_GetMember_Instruction get_member_insn;
  Jackass_SetMember_Instruction set_member_insn;
  Jackass_UnsetMember_Instruction unset_member_insn;
  Jackass_GetIndexed_Instruction get_indexed_insn;
  Jackass_SetIndexed_Instruction set_indexed_insn;
  Jackass_UnsetIndexed_Instruction unset_indexed_insn;
};

typedef struct {
  ... condition
  Jackass_Block *target;
} Jackass_ConditionalGoto;

struct Jackass_Block
{
  size_t n_active_scopes;
  Jackass_Scope_Signature **active_scopes;

  Jackass_Instruction *first, *last;

  unsigned n_conditional_gotos;
  Jackass_ConditionalGoto *conditional_gotos;

  JackassBlock *next_block;
};


struct Jackass_LocalVarRef
{
  unsigned scope_index;
  unsigned var_index;
};

typedef enum
{
  JACKASS_LVALUE_LOCAL,
  JACKASS_LVALUE_INDEX,
  JACKASS_LVALUE_DOT,
  JACKASS_LVALUE_GLOBAL,
} Jackass_LValue_Type;
struct Jackass_LValue
{
  Jackass_LValue_Type type;
  union {
    Jackass_LocalVarRef local;
    struct {
      Jackass_LocalVarRef container, member;
    } index;
    struct {
      Jackass_LocalVarRef container;
      JS_String *member;
    } dot;
    JS_String *global;
  } info;
};
  
struct Jackass_Context
{
  ...
};
void       jackass_context_push_scope(Jackass_Context *);
void       jackass_context_pop_scope (Jackass_Context *);
void       jackass_context_push_function_def (Jackass_Context  *context,
                                              size_t            n_params,
                                              JAST_FormalParam *params);
void       jackass_context_pop_function_def  (Jackass_Context  *context);

JS_Boolean jackass_context_lookup_var(Jackass_Context *context,
                                      JS_String       *name,
                                      Jackass_LocalVarRef *var_out);
JS_Boolean jackass_context_alloc_var (Jackass_Context *context,
                                      JS_String       *optional_name,
                                      Jackass_LocalVarRef *var_out);
JS_Boolean jackass_context_compile_expr(Jackass_Context *context,
                                        JAST_Expr       *expr,
                                        Jackass_LocalVarRef *var_out);
JS_Boolean jackass_context_compile_expr(Jackass_Context *context,
                                        JAST_Statement  *statement);

Jackass_Block *jackass_context_get_position(Jackass_Context *context);
void           jackass_context_next_block (Jackass_Context *context);

void jackass_block_add_conditional (Jackass_Block *block,
                                    Jackass_Condition condition,
                                    Jackass_LocalVarRef var,
                                    Jackass_Block *target);

#define JACKASS_MAKE_LOCAL_VAR_REF(scope_id, var_id) \
  ((Jackass_LocalVarRef) {scope_id,var_id})

void jackass_context_add_member_get  (Jackass_Context     *context,
                                    Jackass_LocalVarRef  container,
                                    JS_String           *member,
                                    Jackass_LocalVarRef  target);


void       jackass_context_add_call    (Jackass_Context     *context,
                                        Jackass_LocalVarRef  func,
                                        Jackass_LocalVarRef *opt_this_arg,
                                        unsigned             n_args,
                                        Jackass_LocalVarRef *args,
                                        Jackass_LocalVarRef  opt_rv_var);
typedef enum
{
  JACKASS_BUILTIN_UNARY_NEGATE,
  JACKASS_BUILTIN_UNARY_PLUS,
  JACKASS_BUILTIN_UNARY_LOGICAL_NOT,
  JACKASS_BUILTIN_UNARY_BITWISE_NOT,
  JACKASS_BUILTIN_UNARY_SUCCESSOR,     // x -> x + 1 --- underlies ++
  JACKASS_BUILTIN_UNARY_PREDECESSOR,   // x -> x - 1 --- underlies --

  JACKASS_BUILTIN_BINARY_ADD,
  JACKASS_BUILTIN_BINARY_SUBTRACT,
  JACKASS_BUILTIN_BINARY_MULTIPLY,
  JACKASS_BUILTIN_BINARY_DIVIDE,
  JACKASS_BUILTIN_BINARY_MOD,
  JACKASS_BUILTIN_BINARY_BITWISE_OR,
  JACKASS_BUILTIN_BINARY_BITWISE_AND,
  JACKASS_BUILTIN_BINARY_BITWISE_XOR,
  JACKASS_BUILTIN_BINARY_CMP_EXACT_EQ,
  JACKASS_BUILTIN_BINARY_CMP_NUM_EQ,
  JACKASS_BUILTIN_BINARY_CMP_EXACT_NE,
  JACKASS_BUILTIN_BINARY_CMP_NUM_NE,
  JACKASS_BUILTIN_BINARY_CMP_LT,
  JACKASS_BUILTIN_BINARY_CMP_LE,
  JACKASS_BUILTIN_BINARY_CMP_GT,
  JACKASS_BUILTIN_BINARY_CMP_GE,
} Jackass_BuiltinFunction;

void       jackass_context_add_builtin_call(Jackass_Context     *context,
                                        Jackass_BuiltinFunction func,
                                        unsigned             n_args,
                                        Jackass_LocalVarRef *args,
                                        Jackass_LocalVarRef  opt_rv_var);

