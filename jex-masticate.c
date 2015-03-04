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

static JAST_Expr *
process_template (Jex_Context *context,
                  JAST_Template_Expr *e)
{
  size_t N = e->n_pieces;
  JAST_TemplatePiece *pieces = alloca (sizeof (JAST_TemplatePiece) * N);
  size_t n_out = 0;
  for (size_t i = 0; i < N; i++)
    {
      if (e->pieces[i]->type == JAST_TEMPLATE_PIECE_STRING)
        {
          if (pieces->info.string->length == 0)
            continue;
          if (n_out > 0 && pieces[n_out-1].type == JAST_TEMPLATE_PIECE_STRING)
            {
              JS_String *old_s = pieces[n_out-1].info.string;
              JS_String *new_s = js_strings_concat (old_s, s);
              pieces[n_out-1].info.string = new_s;
              js_string_unref (old_s);
              js_string_unref (s);
            }
          else
            {
              pieces[n_out++] = e->pieces[i];
              js_string_ref (pieces[n_out-1].info.string);
            }
        }
      else
        {
          assert(e->pieces[i].type == JAST_TEMPLATE_PIECE_EXPR);
          JAST_Expr *sub = jex_masticate_expr (context, e->pieces[i].info.expr);
          if (sub == NULL)
            {
              return NULL;
            }
          else if (is_constant (sub))
            {
              JS_String *s = jex_constprop_expr_to_string (sub);
              if (n_out > 0 && pieces[n_out-1].type == JAST_TEMPLATE_PIECE_STRING)
                {
                  JS_String *old_s = pieces[n_out-1].info.string;
                  JS_String *new_s = js_strings_concat (old_s, s);
                  pieces[n_out-1].info.string = new_s;
                  js_string_unref (old_s);
                  js_string_unref (s);
                }
              else
                {
                  pieces[n_out].type = JAST_TEMPLATE_PIECE_STRING;
                  pieces[n_out].info.string = s;
                  n_out++;
                }
            }
          else
            {
              pieces[n_out].type = JAST_TEMPLATE_PIECE_EXPR;
              pieces[n_out].info.expr = sub;
              n_out++;
            }
        }
    }
  if (n_out == 0)
    return jast_expr_new_string_value (js_string_new_empty ());
  if (n_out == 1 && pieces[0].type == JAST_TEMPLATE_PIECE_STRING)
    return jast_expr_new_string_value (pieces[0].info.string);
  return jast_expr_new_template (n_out, pieces);
}


JAST_Expr *jex_masticate_expr (Jex_Context *context,
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
        {
          JAST_FunctionValue_Expr *e = &expr->function_value_expr;
          jex_context_nest_function (context, e->n_args, e->args);
          JAST_Statement *stmt = jex_masticate_statement (context, stmt);
          if (!stmt)
            return NULL;
          JAST_Expr *rv = jex_context_pop_nested_function (context, stmt, JS_TRUE);
          return rv;
        }

      case JAST_EXPR_ARROW:
        {
          JAST_FunctionValue_Expr *e = &expr->function_value_expr;
          jex_context_nest_function (context, e->n_args, e->args);
          JAST_Statement *stmt = jex_masticate_statement (context, stmt);
          if (!stmt)
            return NULL;
          JAST_Expr *rv = jex_context_pop_nested_function (context, stmt, JS_FALSE);
          return rv;
        }

      case JAST_EXPR_TEMPLATE:
        return process_template (context, &expr->template_expr);
        
      case JAST_EXPR_OBJECT_VALUE:
        return process_object_value (context, &expr->object_value_expr);

      case JAST_EXPR_ARRAY_VALUE:
        return process_array_value (context, &expr->array_value_expr);

      case JAST_EXPR_STRING_VALUE:
      case JAST_EXPR_REGEX_VALUE:
      case JAST_EXPR_NUMBER_VALUE:
      case JAST_EXPR_BOOLEAN_VALUE:
      case JAST_EXPR_UNDEFINED_VALUE:
      case JAST_EXPR_NULL_VALUE:
        return jast_expr_copy (expr);

      case JAST_EXPR_IDENTIFIER:
        {
          Jex_Var *v = jex_context_lookup_var (...);
          if (v)
            {
              ... note that the variable is captured, if we are in a nested function
            }
          else
            {
              ... must be a global
            }
        }
    }
}

JAST_Statement *jex_masticate_statement (Jex_Context *context,
                                         JAST_Statement *stmt)
{
...
}
