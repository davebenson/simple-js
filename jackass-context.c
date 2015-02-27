
#define JACKASS_LOCAL_VAR_SCOPE_MAGIC_BUILTIN_FUNCTION ((unsigned)-1000)


/*                      === Unary Functions ===                        */
static Jackass_BuiltinFunction
unary_op_to_builtin_function (JAST_UnaryOp_Type type)
{
  Jackass_LocalVarRef rv = { JACKASS_LOCAL_VAR_SCOPE_MAGIC_BUILTIN_FUNCTION, 0 };
  switch (type)
    {
      case JAST_UNARY_OP_NEGATE:
        return JACKASS_BUILTIN_UNARY_NEGATE;
      case JAST_UNARY_OP_PLUS:
        return JACKASS_BUILTIN_UNARY_PLUS;
      case JAST_UNARY_OP_BITWISE_NOT:
        return JACKASS_BUILTIN_UNARY_BITWISE_NOT;
      case JAST_UNARY_OP_LOGICAL_NOT:
        return JACKASS_BUILTIN_UNARY_LOGICAL_NOT;
      default:
        assert(0);
        return -1;
    }
}

static JS_Boolean
unary_op_requires_lvalue (JAST_UnaryOp_Type type)
{
  return type == JAST_UNARY_OP_PRE_INCR
      || type == JAST_UNARY_OP_POST_INCR
      || type == JAST_UNARY_OP_PRE_DECR
      || type == JAST_UNARY_OP_POST_DECR;
}

static void
classify_lvalue_unary_op (JAST_UnaryOp_Type type,
                          JS_Boolean       *is_pre_out,
                          Jackass_BuiltinFunction *func_out)
{
  switch (type)
    {
      case JAST_UNARY_OP_PRE_INCR:
        *is_pre_out = JS_TRUE;
        *func_out = JACKASS_BUILTIN_UNARY_INCR;
        break;
      case JAST_UNARY_OP_POST_INCR:
        *is_pre_out = JS_FALSE;
        *func_out = JACKASS_BUILTIN_UNARY_INCR;
        break;
      case JAST_UNARY_OP_PRE_DECR:
        *is_pre_out = JS_TRUE;
        *func_out = JACKASS_BUILTIN_UNARY_DECR;
        break;
      case JAST_UNARY_OP_POST_DECR:
        *is_pre_out = JS_FALSE;
        *func_out = JACKASS_BUILTIN_UNARY_DECR;
        break;
      default:
        assert(0);
    }
}

static JS_Boolean
compile_lvalue_unary_expr (Jackass_Context *context,
                           JAST_Expr       *expr,
                           Jackass_LocalVarRef *var_out)
{
  Jackass_LValue lvalue;
  if (!jackass_context_compile_lvalue (context, expr->unary_op_expr.sub, &lvalue))
    return JS_FALSE;
  JS_Boolean is_pre;
  Jackass_LocalVarRef func;
  classify_lvalue_unary_op (expr->unary_op_expr.op, &is_pre, &func);
  if (var_out == NULL)
    {
      Jackass_LocalVarRef tmp;
      jackass_context_compile_lvalue_get (context, &tmp);
      Jackass_LocalVarRef tmp2;
      jackass_context_alloc_var (context, NULL, &tmp2);
      jackass_context_add_call (context, func, 1, &tmp, &tmp2)
      jackass_context_compile_lvalue_set (context, tmp2);
      jackass_context_drop_var (context, tmp);
      jackass_context_drop_var (context, tmp2);
    }
  else if (is_pre)
    {
      Jackass_LocalVarRef tmp;
      jackass_context_compile_lvalue_get (context, &tmp);
      jackass_context_alloc_var (context, NULL, var_out);
      jackass_context_add_call (context, func, 1, &tmp, var_out)
      jackass_context_compile_lvalue_set (context, *var_out);
      jackass_context_drop_var (context, tmp);
    }
  else
    {
      Jackass_LocalVarRef tmp;
      jackass_context_compile_lvalue_get (context, var_out);
      jackass_context_add_call (context, func, 1, var_out, &tmp)
      jackass_context_compile_lvalue_set (context, lvalue, tmp);
      jackass_context_compile_lvalue_clear (context, lvalue);
      jackass_context_drop_var (context, tmp);
    }
}

/* LValue-RValue operators, namely
      +=    -=    /=    %=   *=
      <<=  >>=   >>>=
      &=   |=    ^=
 */
static Jackass_BuiltinFunction
binary_assign_to_builtin_func (JAST_BinaryOp_Type type)
{
  switch (type)
    {
#define WRITE_CASE(opname) \
      case JAST_BINARY_OP_##opname##_ASSIGN: return JACKASS_BUILTIN_BINARY_##opname
      WRITE_CASE(ADD);
      WRITE_CASE(SUBTRACT);
      WRITE_CASE(MULTIPLY);
      WRITE_CASE(DIVIDE);
      WRITE_CASE(MOD);
      WRITE_CASE(BITWISE_OR);
      WRITE_CASE(BITWISE_AND);
      WRITE_CASE(BITWISE_XOR);
#undef WRITE_CASE
      default:
        assert(0);
    }
  return -1;
}

static JS_Boolean
compile_lrvalue_binary_operators (Jackass_Context *context,
                                  Jackass_BuiltinFunction builtin_func,
                                  JAST_Expr       *e0,
                                  JAST_Expr       *e1,
                                  Jackass_LocalVarRef *var_out)
{
  Jackass_LValue lvalue;
  if (!jackass_context_compile_lvalue (context, e0, &lvalue))
    return JS_FALSE;
  Jackass_LocalVarRef args[2];
  if (!jackass_context_compile_expr (context, e1, &args[1]))
    return JS_FALSE;
  jackass_context_compile_lvalue_get (context, &args[0]);
  Jackass_LocalVarRef rv;
  jackass_context_alloc_var (context, NULL, &rv);
  jackass_context_add_builtin_call (context, builtin_func, 2, args, &rv);
  jackass_context_compile_lvalue_set (context, lvalue, rv);
  jackass_context_compile_lvalue_clear (context, lvalue);

  jackass_context_drop_var (context, &args[0]);
  jackass_context_drop_var (context, &args[1]);
  if (var_out)
    *var_out = rv;
  else
    jackass_context_drop_var (context, &rv);
  return JS_TRUE;
}

static JS_Boolean
compile_assign (Jackass_Context *context,
                JAST_BinaryOp_Expr *expr,
                Jackass_LocalVarRef *var_out)
{
  Jackass_LValue lvalue;
  if (!jackass_context_compile_lvalue (context, expr->binary_op_expr.subs[0], &lvalue))
    return JS_FALSE;
  Jackass_LocalVarRef rvalue;
  if (!jackass_context_compile_expr (context, expr->binary_op_expr.subs[1], &rvalue))
    return JS_FALSE;
  jackass_context_compile_lvalue_set (context, lvalue, rvalue);
  if (var_out)
    *var_out = rvalue;
  else
    jackass_context_drop_var (rvalue);
  jackass_context_compile_lvalue_clear (context, lvalue);
  return JS_TRUE;
}

// implements short-circuit
//
// Evaluate e0.
//    * If eval_second_condition is met, evaluate e1 and return it.
//    * Otherwise, return result from e0.
//
static JS_Boolean
compile_logical     (Jackass_Context *context,
                     Jackass_Condition eval_second_condition,
                     JAST_Expr       *e0,
                     JAST_Expr       *e1,
                     Jackass_LocalVarRef *var_out)
{
  Jackass_LocalVarRef out[2];
  if (var_out)
    jackass_context_alloc_var (context, NULL, var_out);
  if (!jackass_context_compile_expr (context, e0, &out[0]))
    return JS_FALSE;

  Jackass_Block *old_block = jackass_context_current_block (context);
  jackass_context_next_block (context);
  Jackass_Block *out0_rv_block = NULL;
  if (var_out)
    {
      out0_rv_block = jackass_context_current_block (context);
      jackass_context_add_assign (context, out[0], *var_out);
      jackass_context_next_block (context);
    }
  Jackass_Block *out1_rv_block = jackass_context_current_block (context);
  if (!jackass_context_compile_expr (context, e1, &out[1]))
    return JS_FALSE;
  if (var_out)
    jackass_context_add_assign (context, out[0], *var_out);
  jackass_context_next_block (context);

  jackass_block_add_conditional (old_block, out[0], eval_second_condition, out1_rv_block);
  if (out0_rv_block)
    out0_rv_block->next_block = jackass_context_current_block (context);
  else
    old_block->next_block = jackass_context_current_block (context);

  return JS_TRUE;
}

static JS_Boolean
compile_normal_binary_operators (Jackass_Context *context,
                                 Jackass_BuiltinFunction func,
                                 JAST_Expr       *e0,
                                 JAST_Expr       *e1,
                                 Jackass_LocalVarRef *var_out)
{
  Jackass_LocalVarRef args[2];
  if (!jackass_context_compile_expr (context, e0, &args[0])
   || !jackass_context_compile_expr (context, e1, &args[1]))
    return JS_FALSE;
  if (var_out)
    {
      jackass_context_alloc_var (context, var_out);
      jackass_context_add_builtin_call (context, func, 2, args, var_out);
    }
  jackass_context_drop_var (context, args[0]);
  jackass_context_drop_var (context, args[1]);
  return JS_TRUE;
}

static JS_Boolean
compile_cond (Jackass_Context     *context,
              JAST_Cond_Expr      *cond,
              Jackass_LocalVarRef *out)
{
  size_t i;
  Jackass_Block *prev_test_block = NULL;
  Jackass_LocalVarRef prev_test_var;
  unsigned n_value_gotos = cond->n_terms / 2;
  Jackass_Block **come_from_blocks = alloca (sizeof (Jackass_Block *) * n_value_gotos);
  if (out != NULL)
    jackass_context_alloc_var (context, out);
  for (i = 0; i < cond->n_terms; i += 2)
    {
      Jackass_LocalVarRef tmp;
      Jackass_Block *b = jackass_context_current_block (context);
      if (!jackass_context_compile_expr (context, cond->terms[i], &tmp))
        return JS_FALSE;
      if (prev_test_block)
        {
          jackass_block_add_conditional (prev_test_block, tmp, JACKASS_CONDITION_IS_FALSEY, b);
        }
      if (i + 1 == cond->n_terms)
        {
          if (out)
            jackass_context_add_assign (context, tmp, *out);
        }
      else
        {
          prev_test_block = jackass_context_current_block (context);
          jackass_context_next_block (context);
          if (!jackass_context_compile_expr (context, cond->terms[i+1], out ? &tmp : NULL))
            return JS_FALSE;
          if (out)
            jackass_context_add_assign (context, tmp, *out);
          come_from_blocks[i/2] = jackass_context_current_block (context);
          jackass_context_next_block (context);
        }
    }
  jackass_context_next_block (context);
  for (size_t i = 0; i < cond->n_terms / 2; i++)
    come_from_blocks[i]->next_block = jackass_context_current_block (context);
}

static JS_Boolean
can_inline_as_iife (Jackass_Context *context,
                    JAST_FunctionValue_Expr *f)
{
...
}

// Immediately-Invoked Function Expression
// We "inline" it.  Basically:
//    (1) evaluate the arguments as temporaries.
//    (2) create a new scope.
//    (3) allocate the parameters as scope variables,
//        initialized with temporaries.
//    (4) evaluate function body.
//    (5) pop scope
static JS_Boolean
inline_iife  (Jackass_Context *context,
              JAST_Invoke_Expr *expr,
              Jackass_LocalVarRef *var_out)
{
  Jackass_LocalVarRef *vars = alloca (sizeof (Jackass_LocalVarRef) * expr->n_args);
  for (size_t i = 0; i < expr->n_args; i++)
    if (!jackass_context_compile_expr (context, expr->args[i], vars + i))
      return JS_FALSE;

  jackass_context_push_scope (context);

  JAST_FunctionValue_Expr *func_expr = &expr->function->function_value_expr;
  Jackass_LocalVarRef *params = alloca (sizeof (Jackass_LocalVarRef) * expr->n_args);
  for (size_t i = 0; i < expr->n_args; i++)
    jackass_context_alloc_var (context, func_expr->args[i].name, params + i);
  for (size_t i = 0; i < expr->n_args; i++)
    jackass_context_add_assign (context, vars[i], params[i]);

  if (!jackass_context_compile_statement (context, func_expr->body))
    return JS_FALSE;

  jackass_context_pop_scope (context);

  for (size_t i = 0; i < expr->n_args; i++)
    jackass_context_drop_var (context, vars + i);

  return JS_TRUE;
}

static JS_Boolean
compile_generic_invoke (Jackass_Context *context,
                        Jackass_LocalVarRef fct,
                        Jackass_LocalVarRef *opt_this,
                        JAST_Invoke_Expr *expr,
                        Jackass_LocalVarRef *opt_rv)
{
  // eval arguments
  Jackass_LocalVarRef *args = alloca (sizeof (Jackass_LocalVarRef) * expr->n_args);
  for (size_t i = 0; i < expr->n_args; i++)
    if (!jackass_context_compile_expr (context, expr->args[i], args + i))
      return JS_FALSE;

  // add call
  jackass_context_add_call (context, fct, opt_this, n_args, args, opt_rv);

  // drop variables
  for (size_t i = 0; i < expr->n_args; i++)
    jackass_context_drop_var (context, args[i]);
}

JS_Boolean jackass_context_compile_expr(Jackass_Context *context,
                                        JAST_Expr       *expr,
                                        Jackass_LocalVarRef *var_out)
{
  switch (expr)
    {
    case JAST_EXPR_UNARY_OP:
      {
        if (unary_op_requires_lvalue (expr->unary_op_expr.op))
          {
            return compile_lvalue_unary_expr (context, expr, var_out);
          }
        else
          {
            Jackass_LocalVarRef subvar;
            jackass_context_alloc_var (context, NULL, &subvar);
            if (!jackass_context_compile_expr (context, expr->unary_op_expr.sub, &subvar))
              return JS_FALSE;
            jackass_context_alloc_var (context, NULL, var_out);
            jackass_context_add_call (context,
                                      unary_op_to_builtin_function (expr->unary_op_expr.op),
                                      1, &subvar, &var_out);
            jackass_context_drop_var (context, subvar);
          }
        return JS_TRUE;
      }
    case JAST_EXPR_BINARY_OP:
      {
        JAST_Expr *e0 = expr->binary_op_expr.subs[0];
        JAST_Expr *e1 = expr->binary_op_expr.subs[1];
        switch (expr->binary_op_expr.op)
          {
            // Special cased to implement lazy eval
            case JAST_BINARY_OP_LOGICAL_AND:
              return compile_logical (context, JACKASS_CONDITION_IS_TRUTHY, e0, e1, var_out);

            case JAST_BINARY_OP_LOGICAL_OR:
              return compile_logical (context, JACKASS_CONDITION_IS_FALSEY, e0, e1, var_out);

            // A pure assignment can be optimized in ways that say, +=, cannot.
            case JAST_BINARY_OP_ASSIGN:
              {
                Jackass_LValue lvalue;
                Jackass_LocalVarRef rvalue;
                if (!jackass_context_compile_lvalue (context, e0, &lvalue)
                 || !jackass_context_compile_expr (context, e1, &rvalue))
                  return JS_FALSE;
                jackass_context_compile_lvalue_set (context, lvalue, rvalue);
                jackass_context_compile_lvalue_clear (context, lvalue);
                if (var_out)
                  *var_out = rvalue;
                else
                  jackass_context_drop_var (context, rvalue);
                return JS_TRUE;
              }

            // Comma.  Simple double evaluation.
            case JAST_BINARY_OP_COMMA:
              if (!jackass_context_compile_expr (context, e0, NULL))
                return JS_FALSE;
              return jackass_context_compile_expr (context, e1, var_out);

            // Assignment operators are also special - their LHS is an lvalue (both read and write)
            case JAST_BINARY_OP_ADD_ASSIGN:
            case JAST_BINARY_OP_SUBTRACT_ASSIGN:
            case JAST_BINARY_OP_MULTIPLY_ASSIGN:
            case JAST_BINARY_OP_DIVIDE_ASSIGN:
            case JAST_BINARY_OP_MOD_ASSIGN:
            case JAST_BINARY_OP_BITWISE_OR_ASSIGN:
            case JAST_BINARY_OP_BITWISE_AND_ASSIGN:
            case JAST_BINARY_OP_BITWISE_XOR_ASSIGN:
              return compile_lrvalue_binary_operators (
                       context,
                       binary_assign_op_to_builtin_function (expr->binary_op_expr.op),
                       e0, e1,
                       var_out
                     );

            default:
              return compile_normal_binary_operators (
                       context,
                       binary_op_to_builtin_function (expr->binary_op_expr.op),
                       e0, e1,
                       var_out
                     );
          }
      }

    case JAST_EXPR_COND:
      return compile_cond (context, &expr->cond_expr, var_out);

    case JAST_EXPR_DOT:
      {
        Jackass_LocalVarRef c;
        if (!jackass_context_compile_expr (context, expr->dot_expr.container, &c))
          return JS_FALSE;
        if (var_out)
          {
            jackass_context_alloc_var (context, NULL, var_out);
            jackass_block_add_member_get (context, c, expr->dot_expr.member_name, var_out);
          }
        jackass_context_drop_var (context, c);
        return JS_TRUE;
      }

    case JAST_EXPR_INDEX:
      {
        Jackass_LocalVarRef c, i;
        if (!jackass_context_compile_expr (context, expr->index_expr.container, &c)
         || !jackass_context_compile_expr (context, expr->index_expr.index, &i))
          return JS_FALSE;
        if (var_out)
          {
            jackass_context_alloc_var (context, NULL, var_out);
            jackass_context_add_get_indexed (context, c, i, var_out);
          }
        jackass_context_drop_var (context, c);
        jackass_context_drop_var (context, i);
        return JS_TRUE;
      }

    case JAST_EXPR_INVOKE:
      {
        Jackass_LocalVarRef this_obj, fct;
        Jackass_LocalVarRef *this_ptr = NULL;
        JAST_Expr *func_expr = expr->invoke_expr.function;
        if (func_expr->type == JAST_EXPR_DOT)
          {
            // evaluate container
            if (!jackass_context_compile_expr (context, func_expr->dot_expr.container, &this_obj))
              return JS_FALSE;
            this_ptr = &this_obj;

            // get member function
            jackass_context_add_member_get (context, &this_obj, func_expr->dot_expr.member_name, &fct);
          }
        else if (func_expr->type == JAST_EXPR_INDEX)
          {
            // evaluate container
            Jackass_LocalVarRef index;
            jackass_context_alloc_var (context, NULL, &index);
            if (!jackass_context_compile_expr (context, func_expr->index_expr.container, &this_obj)
             || !jackass_context_compile_expr (context, func_expr->index_expr.index, &index))
              return JS_FALSE;
            this_ptr = &this_obj;

            // evaluate index
            jackass_context_add_index_get (context, this_obj, index, &fct);
            jackass_context_drop_var (context, index);
          }
        else if (func_expr->type == JAST_EXPR_FUNCTION_VALUE
            &&   can_inline_as_iife (&func_expr->function_value_expr))
          {
            return inline_iife (context, &expr->invoke_expr, var_out);
          }
        else
          {
            // simple, context-free invocation
            if (!jackass_context_compile_expr (context, func_expr, &fct))
              return JS_FALSE;
          }

        compile_generic_invoke (context, fct, this_ptr, &expr->invoke_expr, var_out);
        break;
      }

    case JAST_EXPR_FUNCTION_VALUE:
      {
        JAST_FunctionValue_Expr *fe = &expr->function_value_expr;
        jackass_context_push_function_def (context, fe->n_args, fe->args);
        jackass_context_compile_statement (context, fe->body);
        jackass_context_pop_function_def (context, var_out);
        break;
      }

    case JAST_EXPR_ARROW:
      {
        ...
        break;
      }

    case JAST_EXPR_OBJECT_VALUE:
      {
        ...
        break;
      }

    case JAST_EXPR_ARRAY_VALUE:
      {
        ...
        break;
      }

    case JAST_EXPR_STRING_VALUE:
      {
        ...
        break;
      }

    case JAST_EXPR_REGEX_VALUE:
      {
        ...
        break;
      }

    case JAST_EXPR_TEMPLATE:
      {
        ...
        break;
      }

    case JAST_EXPR_NUMBER_VALUE:
      {
        ...
        break;
      }

    case JAST_EXPR_BOOLEAN_VALUE:
      {
        ...
        break;
      }

    case JAST_EXPR_UNDEFINED_VALUE:
      {
        ...
        break;
      }

    case JAST_EXPR_NULL_VALUE:
      {
        ...
        break;
      }

    case JAST_EXPR_IDENTIFIER:
      {
        ...
        break;
      }
    }
}

JS_Boolean jackass_context_compile_statement(Jackass_Context *context,
                                             JAST_Statement  *stmt)
{
  switch (stmt->type)
    {
      case JAST_STATEMENT_COMPOUND:
        ...
      case JAST_STATEMENT_IF:
        ...
      case JAST_STATEMENT_SWITCH:
        ...
      case JAST_STATEMENT_FOR:
        ...
      case JAST_STATEMENT_FOR_IN:
        ...
      case JAST_STATEMENT_WHILE:
        ...
      case JAST_STATEMENT_DO_WHILE:
        ...
      case JAST_STATEMENT_WITH:
        ...
      case JAST_STATEMENT_VARIABLE_DECLARATIONS:
        ...
      case JAST_STATEMENT_TRY_CATCH:
        ...
      case JAST_STATEMENT_THROW:
        ...
      case JAST_STATEMENT_LABEL:
        ...
      case JAST_STATEMENT_BREAK:
        ...
      case JAST_STATEMENT_CONTINUE:
        ...
      case JAST_STATEMENT_RETURN:
        ...
      case JAST_STATEMENT_EXPRESSION:
        ...
    }
}
