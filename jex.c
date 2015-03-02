/* Goals:
 *  - constant / value propagation
 *  - all VarDecl stmts have just one variable
 *  - no 'var' - reduce to 'let' everywhere
 *  - "hoisting" - see ???
 *
 * Later?
 *  - IIFE inlining
 */

static inline JS_Boolean
is_constant (JAST_Expr *e)
{
  return jex_constprop_is_foldable_expr_type (e->type);
}

static JS_Boolean
is_pure_unary_op (JAST_UnaryOp_Type type)
{
  switch (type)
    {
      case JAST_UNARY_OP_PRE_INCR:
      case JAST_UNARY_OP_POST_INCR:
      case JAST_UNARY_OP_PRE_DECR:
      case JAST_UNARY_OP_POST_DECR:
        return JS_FALSE;
      case JAST_UNARY_OP_NEGATE:
      case JAST_UNARY_OP_PLUS:
      case JAST_UNARY_OP_BITWISE_NOT:
      case JAST_UNARY_OP_LOGICAL_NOT:
        return JS_TRUE;
    }
}

static JAST_Expr *
process_unary_op (Jex_Context *context,
                  JAST_UnaryOp_Expr *expr)
{
  JAST_Expr *sub = jex_context_expr (context, expr->sub);
  if (sub == NULL)
    return NULL;
  if (is_constant (sub)
   && is_pure_unary_op (expr->op)
   && context->do_constant_folding)
    return jex_constprop_fold_unary_op (expr->op, sub);
  else
    return jast_expr_new_unary_op (sub->op, sub);
}

static JAST_Expr *
process_cond (Jex_Context *context,
              JAST_Cond_Expr *expr)
{
  assert(expr->n_terms % 2 == 1);
  unsigned n_cond = expr->n_terms / 2;
  JAST_Expr **terms_out = ..;
  size_t n_terms_out = 0;
  JAST_Expr *e;
  for (size_t i = 0; i < n_cond; i++)
    {
      e = jex_context_expr (context, expr->terms[2*i]);
      if (!e)
        return NULL;
      if (is_constant (e))
        {
          if (jex_constprop_evaluates_true (e))
            {
              jast_expr_free (e);
              e = jex_context_expr (context, expr->terms[2*i+1]);
              if (!e)
                return NULL;
              terms_out[n_terms_out++] = e;
              goto done;
            }
          else
            {
              // simple omit cond+value
              jast_expr_free (e);
            }
        }
      else
        {
          terms_out[n_terms_out++] = e;
          e = jex_context_expr (context, expr->terms[2*i+1]);
          if (!e)
            return NULL;
          terms_out[n_terms_out++] = e;
        }
    }
  e = jex_context_expr (context, expr->terms[2*i+1]);
  if (!e)
    return NULL;
  terms_out[n_terms_out++] = e;

  if (n_terms_out == 1)
    return terms_out[0];
          
  return jast_expr_new_cond (n_terms_out, terms_out);
}

JAST_Expr *jex_context_expr (Jex_Context *context,
                             JAST_Expr   *expr)
{
  switch (expr->type)
    {
      case JAST_EXPR_UNARY_OP:
        return process_unary_op (context, &expr->unary_op_expr);

      case JAST_EXPR_BINARY_OP:
        return process_binary_op (context, &expr->binary_op_expr);

      case JAST_EXPR_COND:
        return process_cond (context, &expr->cond_expr);

      case JAST_EXPR_DOT:
        {
          JAST_Expr *c = jex_context_expr (context, expr->dot_expr.container);
          if (!c)
            return NULL;
          return jast_expr_new_dot (c, expr->dot_expr.member_name);
        }

      case JAST_EXPR_INDEX:
        {
          JAST_Expr *c = jex_context_expr (context, expr->index_expr.container);
          if (!c)
            return NULL;
          JAST_Expr *i = jex_context_expr (context, expr->index_expr.index);
          if (!i)
            return NULL;
          return jast_expr_new_index (c, i);
        }

      case JAST_EXPR_INVOKE:
        {
          JAST_Expr *f = jast_context_expr (context, expr->invoke_expr.function);
          if (!f)
            return NULL;
          size_t N = expr->invoke_expr.n_args;
          JAST_Expr *in_args = expr->invoke_expr.args;
          JAST_Expr *args = alloca (sizeof (JAST_Expr *) * N);
          JS_Boolean all_args_constant = JS_TRUE;
          for (size_t i = 0; i < N; i++)
            {
              args[i] = jast_context_expr (context, in_args[i]);
              if (!args[i])
                {
                  for (size_t j = 0; j < i; j++)
                    jast_expr_free (args[j]);
                  jast_expr_free (f);
                  return NULL;
                }
              if (!all_args_constant && !is_constant (args[i]))
                all_args_constant = JS_FALSE;
            }
          if (all_args_constant)
            {
              JAST_Expr *c = jex_constprop_try_fold_invocation (f, N, args);
              if (c)
                return c;
            }
          return jast_expr_new_invoke (f, n_args, args);
        }

      case JAST_EXPR_FUNCTION_VALUE:
        ...

      case JAST_EXPR_ARROW:
        ...

      case JAST_EXPR_TEMPLATE:
        ... fold subexprs; remove 0 length + fold contiguous substrings; 

      case JAST_EXPR_OBJECT_VALUE:
        ... fold subexprs

      case JAST_EXPR_ARRAY_VALUE:
        ... fold subexprs

      case JAST_EXPR_STRING_VALUE:
      case JAST_EXPR_REGEX_VALUE:
      case JAST_EXPR_NUMBER_VALUE:
      case JAST_EXPR_BOOLEAN_VALUE:
      case JAST_EXPR_UNDEFINED_VALUE:
      case JAST_EXPR_NULL_VALUE:
        return jast_expr_copy (expr);

      case JAST_EXPR_IDENTIFIER:
        ...
    }
}

JAST_Statement *jex_context_translate (Jex_Context *context,
                                       JAST_Statement *stmt)
{
...
}
  
