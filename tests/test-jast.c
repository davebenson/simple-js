#include "../jast.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

static void
assert_string_is_literal (JS_String *s, const char *str_expr, const char *literal, unsigned line_no)
{
  if (strcmp (literal, (char*) (s+1)) != 0) {
    fprintf(stderr, "FAIL string=literal: string='%s'; literal='%s'; %s, line %u\n",
        str_expr, literal, __FILE__, line_no);
    abort();
  }
}
#define assert_string_is_literal(str, literal) \
  assert_string_is_literal(str, #str, literal, __LINE__)

static void
assert_is_identifier_expr(JAST_Expr *e, const char *e_str, const char *id, unsigned line_no)
{
  if (e->type != JAST_EXPR_IDENTIFIER) {
    fprintf(stderr, "FAIL is_id: not an id expression expr='%s'; id='%s'; %s, line %u\n",
        e_str, id, __FILE__, line_no);
    abort();
  }
  if (strcmp (id, JS_STRING_GET_STR (e->identifier_expr.symbol)) != 0) {
    fprintf(stderr, "FAIL is_id: id expression expr (%s) ='%s' does not match expected id='%s'; %s, line %u\n",
        e_str, JS_STRING_GET_STR (e->identifier_expr.symbol), id, __FILE__, line_no);
    abort();
  }
}
#define assert_is_identifier_expr(e, id) \
  assert_is_identifier_expr(e, #e, id, __LINE__)

static void
assert_is_number_value(JAST_Expr *e, const char *e_str, double n, unsigned line_no)
{
  if (e->type != JAST_EXPR_NUMBER_VALUE) {
    fprintf(stderr, "FAIL is_number: not an number expression='%s'; %s, line %u\n",
        e_str, __FILE__, line_no);
    abort();
  }
  if (e->number_value_expr.value != n) {
    fprintf(stderr, "FAIL is_number: number expression expr (%s) = %f does not match expected %f; %s, line %u\n",
        e_str, e->number_value_expr.value, n, __FILE__, line_no);
    abort();
  }
}
#define assert_is_number_value(e, n) \
  assert_is_number_value(e, #e, n, __LINE__)

static void
assert_is_string_value(JAST_Expr *e, const char *e_str, const char *expected_str, unsigned line_no)
{
  if (e->type != JAST_EXPR_STRING_VALUE) {
    fprintf(stderr, "FAIL is_string: not an string expr='%s'; expected='%s'; %s, line %u\n",
        e_str, expected_str, __FILE__, line_no);
    abort();
  }
  if (strcmp (expected_str, JS_STRING_GET_STR (e->string_value_expr.value)) != 0) {
    fprintf(stderr, "FAIL is_string: string expression expr (%s) ='%s' does not match expected='%s'; %s, line %u\n",
        e_str, JS_STRING_GET_STR (e->string_value_expr.value), expected_str, __FILE__, line_no);
    abort();
  }
}
#define assert_is_string_value(e, id) \
  assert_is_string_value(e, #e, id, __LINE__)

static JAST_Statement *
parse_string (const char *str)
{
  JAST_ParseError *error = NULL;
  JAST_Statement *rv = JAST_parse_data (strlen (str), str, "file.js", &error);
  if (!rv) {
    fprintf(stderr, "error parsing: %s\n", jast_parse_error_to_string(error));
    exit(1);
  }
  return rv;
}

int main()
{
  JAST_Statement *stmt;
  JAST_Statement *tmp, *sub;
  JAST_Expr *ex, *arg, *sub_ex;

  stmt = parse_string ("");
  assert (stmt->type == JAST_STATEMENT_COMPOUND);
  assert (stmt->compound_statement.n_subs == 0);
  jast_statement_free (stmt);

  stmt = parse_string ("let v = 4;");
  assert (stmt->type == JAST_STATEMENT_COMPOUND);
  assert (stmt->compound_statement.n_subs == 1);
  tmp = stmt->compound_statement.subs[0];
  assert(tmp->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  assert(tmp->vardecls_statement.n_vars == 1);
  JAST_BindingPattern *bp = &tmp->vardecls_statement.vars[0];
  assert(bp->type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(bp->info.simple, "v");
  assert(bp->initializer != NULL);
  assert(bp->initializer->type == JAST_EXPR_NUMBER_VALUE);
  assert(bp->initializer->number_value_expr.value == 4.);
  jast_statement_free (stmt);

  stmt = parse_string ("for (let i = 0; i < 10; i++) { console.log('hi'); }");
  assert (stmt->type == JAST_STATEMENT_COMPOUND);
  assert (stmt->compound_statement.n_subs == 1);
  tmp = stmt->compound_statement.subs[0];
  assert(tmp->type == JAST_STATEMENT_FOR);
  assert(tmp->for_statement.initial->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  assert(tmp->for_statement.initial->vardecls_statement.type == JAST_VARIABLE_DECLARATION_LET);
  assert(tmp->for_statement.initial->vardecls_statement.n_vars == 1);
  assert(tmp->for_statement.initial->vardecls_statement.vars[0].type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(tmp->for_statement.initial->vardecls_statement.vars[0].info.simple, "i");
  assert(tmp->for_statement.initial->vardecls_statement.vars[0].initializer->type == JAST_EXPR_NUMBER_VALUE);
  assert(tmp->for_statement.initial->vardecls_statement.vars[0].initializer->number_value_expr.value == 0.);
  assert(tmp->for_statement.condition->type == JAST_EXPR_BINARY_OP);
  assert(tmp->for_statement.condition->binary_op_expr.subs[0]->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(tmp->for_statement.condition->binary_op_expr.subs[0]->identifier_expr.symbol,"i");
  assert(tmp->for_statement.condition->binary_op_expr.op == JAST_BINARY_OP_CMP_LT);
  assert(tmp->for_statement.condition->binary_op_expr.subs[1]->type == JAST_EXPR_NUMBER_VALUE);
  assert(tmp->for_statement.condition->binary_op_expr.subs[1]->number_value_expr.value == 10);
  assert(tmp->for_statement.advance);
  assert(tmp->for_statement.advance->type == JAST_STATEMENT_EXPRESSION);
  assert(tmp->for_statement.advance->expr_statement.expr->type == JAST_EXPR_UNARY_OP);
  assert(tmp->for_statement.advance->expr_statement.expr->unary_op_expr.op == JAST_UNARY_OP_POST_INCR);
  assert(tmp->for_statement.body->type == JAST_STATEMENT_COMPOUND);
  assert(tmp->for_statement.body->compound_statement.n_subs == 1);
  assert(tmp->for_statement.body->compound_statement.subs[0]->type == JAST_STATEMENT_EXPRESSION);
  ex = tmp->for_statement.body->compound_statement.subs[0]->expr_statement.expr;
  assert(ex->type == JAST_EXPR_INVOKE);
  assert(ex->invoke_expr.function->type == JAST_EXPR_DOT);
  assert(ex->invoke_expr.function->dot_expr.container->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(ex->invoke_expr.function->dot_expr.container->identifier_expr.symbol, "console");
  assert_string_is_literal(ex->invoke_expr.function->dot_expr.member_name, "log");
  assert(ex->invoke_expr.n_args == 1);
  assert(ex->invoke_expr.args[0]->type == JAST_EXPR_STRING_VALUE);
  assert_string_is_literal(ex->invoke_expr.args[0]->string_value_expr.value, "hi");
  jast_statement_free (stmt);
  

  stmt = parse_string ("someFunction(a * b + c / d);");
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  assert(stmt->compound_statement.subs[0]->type == JAST_STATEMENT_EXPRESSION);
  ex = stmt->compound_statement.subs[0]->expr_statement.expr;
  assert(ex->type == JAST_EXPR_INVOKE);
  assert(ex->invoke_expr.function->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(ex->invoke_expr.function->identifier_expr.symbol, "someFunction");
  assert(ex->invoke_expr.n_args == 1);
  arg = ex->invoke_expr.args[0];
  assert(arg->type == JAST_EXPR_BINARY_OP);
  printf("binary_op=%d\n",arg->binary_op_expr.op);
  assert(arg->binary_op_expr.op == JAST_BINARY_OP_ADD);
  assert(arg->binary_op_expr.subs[0]->type == JAST_EXPR_BINARY_OP);
  assert(arg->binary_op_expr.subs[0]->binary_op_expr.op == JAST_BINARY_OP_MULTIPLY);
  assert(arg->binary_op_expr.subs[0]->binary_op_expr.subs[0]->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(arg->binary_op_expr.subs[0]->binary_op_expr.subs[0]->identifier_expr.symbol, "a");
  assert(arg->binary_op_expr.subs[0]->binary_op_expr.subs[1]->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(arg->binary_op_expr.subs[0]->binary_op_expr.subs[1]->identifier_expr.symbol, "b");
  assert(arg->binary_op_expr.subs[1]->type == JAST_EXPR_BINARY_OP);
  assert(arg->binary_op_expr.subs[1]->binary_op_expr.op == JAST_BINARY_OP_DIVIDE);
  assert(arg->binary_op_expr.subs[1]->binary_op_expr.subs[0]->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(arg->binary_op_expr.subs[1]->binary_op_expr.subs[0]->identifier_expr.symbol, "c");
  assert(arg->binary_op_expr.subs[1]->binary_op_expr.subs[1]->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(arg->binary_op_expr.subs[1]->binary_op_expr.subs[1]->identifier_expr.symbol, "d");
  jast_statement_free (stmt);

  stmt = parse_string ("for (let mem in cont) { console.log('mem=' + mem); }");
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  tmp = stmt->compound_statement.subs[0];
  assert(tmp->type == JAST_STATEMENT_FOR_IN);
  assert(tmp->for_in_statement.binding.type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(tmp->for_in_statement.binding.info.simple, "mem");
  assert(tmp->for_in_statement.container->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(tmp->for_in_statement.container->identifier_expr.symbol, "cont");
  assert(tmp->for_in_statement.body->type == JAST_STATEMENT_COMPOUND);
  assert(tmp->for_in_statement.body->compound_statement.n_subs == 1);
  assert(tmp->for_in_statement.body->compound_statement.subs[0]->type == JAST_STATEMENT_EXPRESSION);
  ex = tmp->for_in_statement.body->compound_statement.subs[0]->expr_statement.expr;
  assert(ex->type == JAST_EXPR_INVOKE);

  stmt = parse_string ("aaaaa ? bbbb : 42;");
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  tmp = stmt->compound_statement.subs[0];
  assert(tmp->type == JAST_STATEMENT_EXPRESSION);
  ex = tmp->expr_statement.expr;
  assert(ex->type == JAST_EXPR_COND);
  assert(ex->cond_expr.n_terms == 3);
  assert(ex->cond_expr.terms[0]->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(ex->cond_expr.terms[0]->identifier_expr.symbol, "aaaaa");
  assert(ex->cond_expr.terms[1]->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(ex->cond_expr.terms[1]->identifier_expr.symbol, "bbbb");
  assert(ex->cond_expr.terms[2]->type == JAST_EXPR_NUMBER_VALUE);
  assert(ex->cond_expr.terms[2]->number_value_expr.value == 42.);

  stmt = parse_string ("if (a < 42) n+=3; else if (b !== 'hi') q /= 2; else r *= 4 ^ n;");
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  tmp = stmt->compound_statement.subs[0];
  assert(tmp->type == JAST_STATEMENT_IF);
  assert(tmp->if_statement.n_conditional_statements == 2);

  /* First conditional */
  ex = tmp->if_statement.conditional_statements[0].expr;
  sub = tmp->if_statement.conditional_statements[0].statement;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_CMP_LT);
  assert(ex->binary_op_expr.subs[0]->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(ex->binary_op_expr.subs[0]->identifier_expr.symbol, "a");
  assert(ex->binary_op_expr.subs[1]->type == JAST_EXPR_NUMBER_VALUE);
  assert(ex->binary_op_expr.subs[1]->number_value_expr.value == 42);
  assert (sub->type == JAST_STATEMENT_EXPRESSION);
  sub_ex = sub->expr_statement.expr;
  assert(sub_ex->type == JAST_EXPR_BINARY_OP);
  assert(sub_ex->binary_op_expr.op == JAST_BINARY_OP_ADD_ASSIGN);
  assert(sub_ex->binary_op_expr.subs[0]->type == JAST_EXPR_IDENTIFIER);
  assert_string_is_literal(sub_ex->binary_op_expr.subs[0]->identifier_expr.symbol, "n");
  assert(sub_ex->binary_op_expr.subs[1]->type == JAST_EXPR_NUMBER_VALUE);
  assert(sub_ex->binary_op_expr.subs[1]->number_value_expr.value == 3.);

  /* Second conditional */
  ex = tmp->if_statement.conditional_statements[1].expr;
  sub = tmp->if_statement.conditional_statements[1].statement;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_CMP_EXACT_NE);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0], "b");
  assert_is_string_value(ex->binary_op_expr.subs[1], "hi");
  assert(sub->type == JAST_STATEMENT_EXPRESSION);
  sub_ex = sub->expr_statement.expr;
  assert(sub_ex->type == JAST_EXPR_BINARY_OP);
  assert(sub_ex->binary_op_expr.op == JAST_BINARY_OP_DIVIDE_ASSIGN);
  assert_is_identifier_expr(sub_ex->binary_op_expr.subs[0], "q");
  assert_is_number_value(sub_ex->binary_op_expr.subs[1], 2.);

  /* else */
  sub = tmp->if_statement.else_statement;
  assert(sub->type == JAST_STATEMENT_EXPRESSION);
  ex = sub->expr_statement.expr;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0], "r");
  assert(ex->binary_op_expr.subs[1]->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.subs[1]->binary_op_expr.op == JAST_BINARY_OP_BITWISE_XOR);
  assert_is_number_value(ex->binary_op_expr.subs[1]->binary_op_expr.subs[0], 4);
  assert_is_identifier_expr(ex->binary_op_expr.subs[1]->binary_op_expr.subs[1], "n");

  return 0;
}

