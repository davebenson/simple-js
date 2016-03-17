#include "jex.h"
#include "deb-array.h"



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
  JEX_Var subvar;
  JEX *sub = jex_compile_expr (context, expr->sub, &subvar);
  if (sub == NULL)
    return NULL;
  if (is_constant (context, jex, subvar)
   && is_pure_unary_op (expr->op)
   && context->do_constant_folding)
    return jex_constprop_fold_unary_op (expr->op, sub);
  else
    return jex_new_unary_op (sub->op, sub);
}

static JEX *
process_cond (Jex_Context *context,
              JAST_Cond_Expr *expr)
{
  assert(expr->n_terms % 2 == 1);
  unsigned n_cond = expr->n_terms / 2;
  size_t n_terms_out = 0;
  JAST_Expr *e;
  JEX *rv = jex_group_new ();
  for (size_t i = 0; i < n_cond; i++)
    {
      e = jex_context_expr (context, expr->terms[2*i]);
      if (!e)
        goto error_cleanup;
      if (is_constant (e))
        {
          if (jex_constprop_evaluates_true (e))
            {
              jast_expr_free (e);
              e = jex_context_expr (context, expr->terms[2*i+1]);
              if (!e)
                goto error_cleanup;
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
            goto error_cleanup;
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

error_cleanup:
  jex_free (rv);
  return NULL;
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


JEX *
jex_compile_expr (Jex_Context *context,
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
        {
          JEX_Constant cvalue;
          if (!jex_constprop_get_value (expr, &cvalue))
            assert(NO);
          return jex_new_constant (expr, cvalue);
        }

      case JAST_EXPR_IDENTIFIER:
        {
          Jex_Var *v = jex_context_lookup_var (context, expr->identifier_expr.name);
          if (v != NULL)
            {
              return jex_new_local_var (v);
            }
          else
            {
              return jex_new_global (expr->identifier_expr.name);
            }
        }
    }
}

static JEX *
process_compound_statement (Jex_Context *context,
                            JAST_Compound_Statement *stmt)
{
  JEX *rv = jex_group_new ();
  for (size_t i = 0; i < stmt->n_subs; i++)
    {
      JEX *sub = jex_compile_statement (context, stmt->subs[i]);
      if (sub == NULL)
        {
          return NULL;
        }
      jex_group_add_child (rv, sub);
    }
  return rv;
}

// Pseudo-code of IF implementation
//   {
//    goto_if !condition1, label1
//    body1
//    goto last_label;
//   label1:
//    goto_if !condition2, label2
//    body2
//    goto last_label;
//   label2:
//    else_body
//   last_label:
//   }
static JEX *
process_if_statement (Jex_Context *context, JAST_If_Statement *stmt)
{
  JEX *rv = jex_group_new ();
  JEX *last_if_not = NULL;
  for (size_t i = 0; i < stmt->n_conditional_statements; i++)
    {
      JEX_Var condvar;
      JEX *cond = jex_compile_expr (context, stmt->conditional_statements[i].condition, &condvar);
      if (is_constant (condvar))
        {
          JAST_Statement *body;
          JS_Boolean is_true = jex_constprop_evaluates_true (condvar);
          if (is_true)
            {
              body = stmt->conditional_statements[i].body;
            }
          else
            {
              body = stmt->else_body;
            }
          if (body != NULL)
            {
              JEX *b = jex_compile_statement (context, body, NULL);
              jex_group_add_child (rv, b);
              if (last_if_not)
                {
                  jex_goto_set_target (last_if_not, b);
                  last_if_not = NULL;
                }
            }
          goto done_with_else;
        }

      // Normal case: non-constant condition.
      jex_group_add_child (rv, cond);
      if (last_if_not != NULL)
        {
          jex_goto_set_target (last_if_not, cond);
          last_if_not = NULL;
        }

      //  (normal case: emit goto_if)
      last_if_not = jex_new_goto_if (condvar, JS_FALSE);
      jex_group_add_child (rv, last_if_not);

      //  (normal case: emit body for this conditianal- for when cond is true)
      JEX *body = jex_compile_statement (context, stmt->conditional_statements[i].body);
      jex_group_add_child (rv, body);

      //  (normal case: emit jump to end- for when cond is true)
      JEX *goto_end = jex_goto_new ();
      jex_goto_set_target (goto_end, jex_group_get_terminal (rv));
    }
  if (stmt->else_body != NULL)
    {
      JEX *eb = jex_compile_statement (context, stmt->else_body);
      if (last_if_not)
        jex_goto_set_target (last_if_not, eb);
      jex_group_add_child (rv, eb);
    }
  else
    {
      if (last_if_not)
        jex_goto_set_target (last_if_not, jex_group_get_terminal (rv));
    }

  return rv;
}

static JEX *
process_switch_statement (Jex_Context *context, JAST_Switch_Statement *stmt)
{
  JEX *rv = jex_group_new ();

  // Compile main expression
  JEX_Var mainexprvar;
  JEX *mainexpr = jex_compile_expr (context, stmt->expr, &mainexprvar);
  jex_group_add_child (rv, mainexpr);

  // Create jump-table to populate.
  DEB_ARRAY_STRUCT(JEX_GotoTableEntry) cur_entries = DEB_ARRAY_INITIALIZER(JEX_GotoTableEntry);

  JEX *last_table;
  
#define RESET_LAST_TABLE() \
  do{ \
    last_table = jex_goto_table_new (); \
    last_table->v_goto_table.switch_value = mainexprvar; \
    jex_group_add_child (rv, last_table); \
  }while(0)

#define MAYBE_FLUSH_GOTO_TABLE() \
  do{ \
    if (cur_entries.n > 0) \
      { \
        last_table->v_goto_table.n_entries = cur_entries.n; \
        last_table->v_goto_table.n_entries = DEB_ARRAY_MAKE_COPY (JEX_GotoTableEntry, cur_entries); \
        cur_entries.n = 0; \
      } \
  }while(0)

  RESET_LAST_TABLE();

  for (size_t ci = 0; ci < stmt->n_clauses; ci++)
    {
      switch (stmt->clauses[ci].clause_type)
        {
        case JAST_SWITCH_CLAUSE_CASE:
          JEX_Var *case_var;
          JEX *case_expr = jex_compile_expr (context, stmt->clauses[ci].info.case_value, &case_var);
          if (case_expr->type == JEX_TYPE_EMPTY && case_var->type == JEX_VAR_CONSTANT_VALUE)
            {
              // if constant...
              JEX_GotoTableEntry entry = {
                case_var->v_constant_value.value,
                NULL
              };
              DEB_ARRAY_APPEND(cur_entries, JEX_GotoTableEntry, entry);
            }
          else 
            {
              MAYBE_FLUSH_GOTO_TABLE();

              jex_group_add_child (rv, case_expr);

              //    goto if not equal --- target is next jump table
              JEX_Var *rv = jex_context_alloc_var (context, NULL);
              JEX *binop = jex_new_binop (JEX_BINARY_OP_EXACT_EQUALS, rv, case_var, mainexprvar);
              jex_group_add_child (rv, binop);
              JEX *goto_ne = jex_new_goto_if (...);
              jex_group_add_child (rv, goto_ne);

              //    create new jump table base on same var
              RESET_LAST_TABLE();
              jex_goto_set_target (goto_ne, last_table);
            }
          break;

        case JAST_SWITCH_CLAUSE_STATEMENT:
          assert (ci > 0);
          ...

        case JAST_SWITCH_CLAUSE_DEFAULT:
          ...
        }
    }
  MAYBE_FLUSH_GOTO_TABLE();

#undef RESET_LAST_TABLE
#undef MAYBE_FLUSH_GOTO_TABLE

  // would really be nice to filter goto-table's
  // with 0 elements at this point.
  ...
}

//{
//  init
// label1:
//  goto_if !condition, label3
//  body
// label2:    // used for continue statements
//  advance
//  goto label1
// label3:    // also used for break statements
//}
static JEX *
process_for_statement (Jex_Context *context, JAST_For_Statement *stmt)
{
  JEX *rv = jex_group_new ();
  if (stmt->initial != NULL)
    {
      JEX *tmp = jex_compile_statement (context, stmt->initial);
      jex_group_add_child (rv, tmp);
    }
  JEX *top_of_loop = NULL;
  JEX_Var *cvar = NULL;
  if (stmt->condition != NULL)
    {
      JEX *tmp = jex_compile_expr (context, stmt->initial, &cvar);
      jex_group_add_child (rv, tmp);
      if (top_of_loop == NULL)
        top_of_loop = tmp;
    }
  if (cvar != NULL)
    {
      // Append GOTO if false.
      JEX *tmp = jex_goto_if_new (cvar, JS_FALSE);
      jex_goto_set_target (tmp, jex_group_get_terminal (rv));
      jex_group_add_child (rv, tmp);
      if (top_of_loop == NULL)
        top_of_loop = tmp;
    }
  if (stmt->body != NULL)
    {
      jex_context_push_scope (context, stmt->label, ...continue, ...break);
      JEX *tmp = jex_compile_statement (context, stmt->body);
      jex_group_add_child (rv, tmp);
      if (top_of_loop == NULL)
        top_of_loop = tmp;
      jex_context_pop_scope (context);
    }
  if (stmt->advance != NULL)
    {
      JEX *tmp = jex_compile_statement (context, stmt->advance);
      jex_group_add_child (rv, tmp);
      if (top_of_loop == NULL)
        top_of_loop = tmp;
    }

  // Append GOTO top of loop.
  JEX *goto_top = jex_goto_new ();
  jex_group_add_child (rv, goto_top);
  if (top_of_loop == NULL)
    top_of_loop = goto_top;

  // Set target to top of loop.
  goto_top->v_goto.target = top_of_loop;

  return rv;
}

static JEX *
process_do_while_statement (Jex_Context *context, JAST_DoWhile_Statement *stmt)
{
  JEX *rv = jex_group_new ();
  JEX *body_comp = jex_compile_statement (context, stmt->body);
  jex_group_add_child (rv, body_comp);
  JEX *condition_expr = jex_compile_expr (context, stmt->condition, &cvar);
  jex_group_add_child (rv, condition_expr);
  JEX *goto_ex = jex_goto_if_new (cvar, JS_TRUE);
  jex_group_add_child (rv, goto_ex);
  jex_goto_set_target (goto_ex, body_comp);
  return rv
}

static JEX *
process_while_statement (Jex_Context *context, JAST_While_Statement *stmt)
{
  JEX *rv = jex_group_new ();
  JEX *condition_expr = jex_compile_expr (context, stmt->condition, &cvar);
  jex_group_add_child (rv, condition_expr);
  JEX *goto_ex = jex_goto_if_new (cvar, JS_FALSE);
  jex_group_add_child (rv, goto_ex);
  JEX *body_comp = jex_compile_statement (context, stmt->body);
  jex_group_add_child (rv, body_comp);
  jex_goto_set_target (goto_ex, jex_group_get_terminal (rv));
  return rv
}

JEX *
jex_compile_statement (Jex_Context *context,
                         JAST_Statement *stmt)
{
  switch (stmt->type)
    {
      case JAST_STATEMENT_COMPOUND:
        return process_compound_statement (context, &stmt->compound_statement);

      case JAST_STATEMENT_IF:
        // emit labels and conditional jumps
        return process_if_statement (context, &stmt->if_statement);

      case JAST_STATEMENT_SWITCH:
        return process_switch_statement (context, &stmt->switch_statement);

      case JAST_STATEMENT_FOR:
        return process_for_statement (context, &stmt->for_statement);

      case JAST_STATEMENT_FOR_IN:                /* includes "for...of..." */
        ...

      case JAST_STATEMENT_WHILE:
        return process_while_statement (context, &stmt->while_statement);

      case JAST_STATEMENT_DO_WHILE:
        return process_do_while_statement (context, &stmt->do_while_statement);

      case JAST_STATEMENT_WITH:
        ...

      case JAST_STATEMENT_VARIABLE_DECLARATIONS:
        jex_new_local_var
        ...
        
      case JAST_STATEMENT_TRY_CATCH:
        return process_try_catch_statement (context, &stmt->try_cache_statement);

      case JAST_STATEMENT_THROW:
        return process_throw_statement (context, &stmt->throw_statement);

      case JAST_STATEMENT_LABEL:
        return jex_new_label (...);

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
