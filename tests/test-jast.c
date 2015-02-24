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
  fprintf(stderr, "parsing: %s\n",str);
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
  jast_statement_free (stmt);


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
  jast_statement_free (stmt);

  stmt = parse_string ("do { a = !a; } while ((is_prime(a)));");
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  assert(stmt->compound_statement.subs[0]->type == JAST_STATEMENT_DO_WHILE);
  sub = stmt->compound_statement.subs[0]->do_while_statement.body;
  assert (sub->type == JAST_STATEMENT_COMPOUND);
  assert (sub->compound_statement.n_subs == 1);
  assert (sub->compound_statement.subs[0]->type == JAST_STATEMENT_EXPRESSION);
  sub_ex = sub->compound_statement.subs[0]->expr_statement.expr;
  assert(sub_ex->type == JAST_EXPR_BINARY_OP);
  assert_is_identifier_expr(sub_ex->binary_op_expr.subs[0], "a");
  assert(sub_ex->binary_op_expr.subs[1]->type == JAST_EXPR_UNARY_OP);
  assert(sub_ex->binary_op_expr.subs[1]->unary_op_expr.op == JAST_UNARY_OP_LOGICAL_NOT);
  assert_is_identifier_expr(sub_ex->binary_op_expr.subs[1]->unary_op_expr.sub, "a");
  ex = stmt->compound_statement.subs[0]->do_while_statement.condition;
  assert(ex->type == JAST_EXPR_INVOKE);
  assert_is_identifier_expr(ex->invoke_expr.function, "is_prime");
  assert(ex->invoke_expr.n_args == 1);
  assert_is_identifier_expr(ex->invoke_expr.args[0], "a");
  jast_statement_free(stmt);

                               /* clause  0       1      2       3      4      5       6 */
  stmt = parse_string ("switch (foo()) { case 1: a+= 3; break; case 3: a/=2; default: a+=1; }");
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_SWITCH);
  ex = sub->switch_statement.expr;
  assert(ex);
  assert(ex->type == JAST_EXPR_INVOKE);
  assert(ex->invoke_expr.n_args == 0);
  assert_is_identifier_expr(ex->invoke_expr.function, "foo");
  assert(sub->switch_statement.n_clauses == 7);
  JAST_Switch_Clause *sclauses = sub->switch_statement.clauses;
  assert(sclauses[0].clause_type == JAST_SWITCH_CLAUSE_CASE);
  assert_is_number_value(sclauses[0].info.case_value, 1);
  assert(sclauses[1].clause_type == JAST_SWITCH_CLAUSE_STATEMENT);
  assert(sclauses[1].info.statement->type == JAST_STATEMENT_EXPRESSION);
  ex = sclauses[1].info.statement->expr_statement.expr;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_ADD_ASSIGN);
  assert_is_identifier_expr (ex->binary_op_expr.subs[0], "a");
  assert_is_number_value (ex->binary_op_expr.subs[1], 3);
  assert(sclauses[2].clause_type == JAST_SWITCH_CLAUSE_STATEMENT);
  assert(sclauses[2].info.statement->type == JAST_STATEMENT_BREAK);
  assert(sclauses[2].info.statement->break_statement.label == NULL);
  assert(sclauses[3].clause_type == JAST_SWITCH_CLAUSE_CASE);
  assert_is_number_value(sclauses[3].info.case_value, 3);
  assert(sclauses[4].clause_type == JAST_SWITCH_CLAUSE_STATEMENT);
  assert(sclauses[4].info.statement->type == JAST_STATEMENT_EXPRESSION);
  ex = sclauses[4].info.statement->expr_statement.expr;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_DIVIDE_ASSIGN);
  assert_is_identifier_expr (ex->binary_op_expr.subs[0], "a");
  assert_is_number_value (ex->binary_op_expr.subs[1], 2);
  assert(sclauses[5].clause_type == JAST_SWITCH_CLAUSE_DEFAULT);
  assert(sclauses[6].clause_type == JAST_SWITCH_CLAUSE_STATEMENT);
  assert(sclauses[6].info.statement->type == JAST_STATEMENT_EXPRESSION);
  ex = sclauses[6].info.statement->expr_statement.expr;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_ADD_ASSIGN);
  assert_is_identifier_expr (ex->binary_op_expr.subs[0], "a");
  assert_is_number_value (ex->binary_op_expr.subs[1], 1);
  jast_statement_free (stmt);

  // object binding patterns 
  {
  stmt = parse_string ("let {a,b:b,c:{d=1,e:e='2'}} = options;");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  assert(sub->vardecls_statement.n_vars == 1);
  JAST_BindingPattern *bp = &sub->vardecls_statement.vars[0];
  assert(bp->type == JAST_BINDING_PATTERN_OBJECT);
  assert(bp->info.object.n_fields == 3);
  JAST_FieldBindingPattern *fields = bp->info.object.fields;
  assert_string_is_literal(fields[0].name, "a");
  assert_string_is_literal(fields[1].name, "b");
  assert(fields[1].binding.type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(fields[1].binding.info.simple, "b");
  assert_string_is_literal(fields[2].name, "c");
  assert(fields[2].binding.type == JAST_BINDING_PATTERN_OBJECT);
  JAST_BindingPattern *sub_bp = &fields[2].binding;
  assert(sub_bp->info.object.n_fields == 2);
  assert_string_is_literal(sub_bp->info.object.fields[0].name, "d");
  assert_string_is_literal(sub_bp->info.object.fields[1].name, "e");
  assert(bp->initializer);
  assert_is_identifier_expr (bp->initializer, "options");
  jast_statement_free(stmt);
  }

  {
  stmt = parse_string("let [a,,c,d=3] = foo;");
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  sub = stmt->compound_statement.subs[0];
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  assert(sub->vardecls_statement.type == JAST_VARIABLE_DECLARATION_LET);
  assert(sub->vardecls_statement.n_vars == 1);
  JAST_BindingPattern *vars = sub->vardecls_statement.vars;
  assert(vars[0].type == JAST_BINDING_PATTERN_ARRAY);
  assert(vars[0].info.array.n_subs == 4);
  assert(vars[0].info.array.subs[0].type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(vars[0].info.array.subs[0].info.simple, "a");
  assert(vars[0].info.array.subs[0].initializer == NULL);
  assert(vars[0].info.array.subs[1].type == JAST_BINDING_PATTERN_NONE);
  assert(vars[0].info.array.subs[1].initializer == NULL);
  assert(vars[0].info.array.subs[2].type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(vars[0].info.array.subs[2].info.simple, "c");
  assert(vars[0].info.array.subs[2].initializer == NULL);
  assert(vars[0].info.array.subs[3].type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(vars[0].info.array.subs[3].info.simple, "d");
  assert_is_number_value(vars[0].info.array.subs[3].initializer, 3);
  assert_is_identifier_expr(vars[0].initializer, "foo");
  jast_statement_free(stmt);
  }

  {
  stmt = parse_string ("break a;");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_BREAK);
  assert_string_is_literal(sub->break_statement.label, "a");
  jast_statement_free(stmt);
  }
  {
  stmt = parse_string ("continue c;");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_CONTINUE);
  assert_string_is_literal(sub->continue_statement.label, "c");
  jast_statement_free(stmt);
  }

  {
  stmt = parse_string ("const a = { a: 14, b: 2 * 3, \"c\": foo(), 'd': goo(), [\"e\"]: hoo()}, b;");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  assert(sub->vardecls_statement.type == JAST_VARIABLE_DECLARATION_CONST);
  assert(sub->vardecls_statement.n_vars == 2);
  JAST_BindingPattern *vars = sub->vardecls_statement.vars;
  assert(vars[0].type == JAST_BINDING_PATTERN_SIMPLE);
  ex = vars[0].initializer;
  assert(ex->type == JAST_EXPR_OBJECT_VALUE);
  assert(ex->object_value_expr.n_fields == 5);
  JAST_ObjectFieldValue *fields = ex->object_value_expr.fields;

  assert_string_is_literal(fields[0].key, "a");
  assert_is_number_value(fields[0].value, 14.);
  assert_string_is_literal(fields[1].key, "b");
  assert(fields[1].value->type == JAST_EXPR_BINARY_OP);
  assert(fields[1].value->binary_op_expr.op == JAST_BINARY_OP_MULTIPLY);
  assert_is_number_value(fields[1].value->binary_op_expr.subs[0], 2);
  assert_is_number_value(fields[1].value->binary_op_expr.subs[1], 3);
  assert_string_is_literal(fields[2].key, "c");
  assert(fields[2].value->type == JAST_EXPR_INVOKE);
  assert_is_identifier_expr(fields[2].value->invoke_expr.function, "foo");
  assert_string_is_literal(fields[3].key, "d");
  assert(fields[3].value->type == JAST_EXPR_INVOKE);
  assert_is_identifier_expr(fields[3].value->invoke_expr.function, "goo");
  assert(fields[4].computed_key->type == JAST_EXPR_STRING_VALUE);
  assert_is_string_value (fields[4].computed_key, "e");
  assert(fields[4].value->type == JAST_EXPR_INVOKE);
  assert_is_identifier_expr(fields[4].value->invoke_expr.function, "hoo");

  assert(vars[1].type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(vars[1].info.simple, "b");
  assert(vars[1].initializer == NULL);

  jast_statement_free (stmt);
  }

  {
  stmt = parse_string ("c *= b++ + --a;");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_EXPRESSION);
  sub_ex = sub->expr_statement.expr;
  assert(sub_ex->type == JAST_EXPR_BINARY_OP);
  assert(sub_ex->binary_op_expr.op == JAST_BINARY_OP_MULTIPLY_ASSIGN);
  assert_is_identifier_expr (sub_ex->binary_op_expr.subs[0], "c");
  JAST_Expr *rhs = sub_ex->binary_op_expr.subs[1];
  assert(rhs->type == JAST_EXPR_BINARY_OP);
  assert(rhs->binary_op_expr.op == JAST_BINARY_OP_ADD);
  ex = rhs->binary_op_expr.subs[0];
  assert(ex->type == JAST_EXPR_UNARY_OP);
  assert(ex->unary_op_expr.op == JAST_UNARY_OP_POST_INCR);
  assert_is_identifier_expr(ex->unary_op_expr.sub, "b");
  ex = rhs->binary_op_expr.subs[1];
  assert(ex->type == JAST_EXPR_UNARY_OP);
  assert(ex->unary_op_expr.op == JAST_UNARY_OP_PRE_DECR);
  assert_is_identifier_expr(ex->unary_op_expr.sub, "a");
  jast_statement_free (stmt);
  }

  {
  stmt = parse_string ("c *= b++ % --a;");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_EXPRESSION);
  sub_ex = sub->expr_statement.expr;
  assert(sub_ex->type == JAST_EXPR_BINARY_OP);
  assert(sub_ex->binary_op_expr.op == JAST_BINARY_OP_MULTIPLY_ASSIGN);
  assert_is_identifier_expr (sub_ex->binary_op_expr.subs[0], "c");
  JAST_Expr *rhs = sub_ex->binary_op_expr.subs[1];
  assert(rhs->type == JAST_EXPR_BINARY_OP);
  assert(rhs->binary_op_expr.op == JAST_BINARY_OP_MOD);
  ex = rhs->binary_op_expr.subs[0];
  assert(ex->type == JAST_EXPR_UNARY_OP);
  assert(ex->unary_op_expr.op == JAST_UNARY_OP_POST_INCR);
  assert_is_identifier_expr(ex->unary_op_expr.sub, "b");
  ex = rhs->binary_op_expr.subs[1];
  assert(ex->type == JAST_EXPR_UNARY_OP);
  assert(ex->unary_op_expr.op == JAST_UNARY_OP_PRE_DECR);
  assert_is_identifier_expr(ex->unary_op_expr.sub, "a");
  jast_statement_free (stmt);
  }

  // TODO: prefix/postfix parsing torture tests
  // TODO: object and array literals
  // TODO: template strings
  // TODO: object syntax
  // TODO: arrows

  return 0;
}

