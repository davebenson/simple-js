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

#define ESC "\33"
#define VT100_UNDERSCORE ESC "[4m"
#define VT100_DEFAULT_FONT ESC "("
#define VT100_ALT_FONT ESC ")"
#define VT100_DIM ESC "[2m"
#define VT100_RED ESC "[31m"
#define VT100_GREEN ESC "[32m"
#define VT100_YELLOW ESC "[33m"
#define VT100_BLUE ESC "[34m"
#define VT100_MAGENTA ESC "[35m"
#define VT100_CYAN ESC "[36m"
#define VT100_WHITE ESC "[37m"
#define VT100_RESET ESC "[0m"


static void
clean_print (const char *str)
{
  while (*str)
    {
      if (*str == '\n')
        fputs(VT100_DIM "\\n" VT100_RESET, stderr);
      else
        fputc(*str, stderr);
      str++;
    }
}

static JAST_Statement *
parse_string (const char *str)
{
  JAST_ParseError *error = NULL;
  fprintf(stderr, "parsing: "); clean_print (str); fprintf(stderr, "\n");
  JAST_Statement *rv = JAST_parse_data (strlen (str), str, "file.js", &error);
  if (!rv) {
    fprintf(stderr, "error parsing: %s\n", jast_parse_error_to_string(error));
    exit(1);
  }
  return rv;
}

static void
test_parse_failure(const char *str, int line_no)
{
  JAST_ParseError *error = NULL;
  fprintf(stderr, "fail parse: "); clean_print (str); fprintf(stderr, "\n");
  JAST_Statement *rv = JAST_parse_data (strlen (str), str, "file.js", &error);
  if (rv)
    {
      fprintf(stderr,
              "parse of %s succeeded, was supposed to fail %s, line %u\n",
              str, __FILE__, line_no);
      exit (1);
    }
  jast_parse_error_free (error);
}
#define test_parse_failure(str) test_parse_failure(str, __LINE__)

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
  assert(tmp->for_statement.advance->type == JAST_EXPR_UNARY_OP);
  assert(tmp->for_statement.advance->unary_op_expr.op == JAST_UNARY_OP_POST_INCR);
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
  stmt = parse_string ("const a = { b, foo() { return 42; }, __proto__: XYZ };");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  assert(sub->vardecls_statement.type == JAST_VARIABLE_DECLARATION_CONST);
  assert(sub->vardecls_statement.n_vars == 1);
  JAST_BindingPattern *vars = sub->vardecls_statement.vars;
  assert(vars[0].type == JAST_BINDING_PATTERN_SIMPLE);
  ex = vars[0].initializer;
  assert(ex->type == JAST_EXPR_OBJECT_VALUE);
  assert(ex->object_value_expr.n_fields == 3);
  JAST_ObjectFieldValue *fields = ex->object_value_expr.fields;
  assert_string_is_literal (fields[0].key, "b");
  assert(fields[0].computed_key == NULL);
  assert_is_identifier_expr (fields[0].value, "b");
  assert_string_is_literal (fields[1].key, "foo");
  assert(fields[1].computed_key == NULL);
  assert(fields[1].value->type == JAST_EXPR_FUNCTION_VALUE);
  assert_string_is_literal (fields[2].key, "__proto__");
  assert(fields[2].computed_key == NULL);
  assert_is_identifier_expr(fields[2].value, "XYZ");
  //...
  jast_statement_free (stmt);
  }

  {
  stmt = parse_string ("const a = [ 42, c, { }, ];");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  assert(sub->vardecls_statement.type == JAST_VARIABLE_DECLARATION_CONST);
  assert(sub->vardecls_statement.n_vars == 1);
  assert(sub->vardecls_statement.vars[0].type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(sub->vardecls_statement.vars[0].info.simple, "a");
  assert (sub->vardecls_statement.vars[0].initializer->type == JAST_EXPR_ARRAY_VALUE);
  JAST_ArrayValue_Expr *ave = &sub->vardecls_statement.vars[0].initializer->array_value_expr;
  assert(ave->n_values == 3);
  assert_is_number_value (ave->values[0], 42);
  assert_is_identifier_expr (ave->values[1], "c");
  assert(ave->values[2]->type == JAST_EXPR_OBJECT_VALUE);
  assert(ave->values[2]->object_value_expr.n_fields == 0);
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

  {
  stmt = parse_string ("console.log(`test ${test2} of template ${templ+2} class`);");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_EXPRESSION);
  sub_ex = sub->expr_statement.expr;
  assert(sub_ex->type == JAST_EXPR_INVOKE);
  assert(sub_ex->invoke_expr.n_args == 1);
  ex = sub_ex->invoke_expr.args[0];
  assert(ex->type == JAST_EXPR_TEMPLATE);
  assert(ex->template_expr.n_pieces == 5);
  assert(ex->template_expr.pieces[0].type == JAST_TEMPLATE_PIECE_STRING);
  assert_string_is_literal(ex->template_expr.pieces[0].info.string, "test ");
  assert(ex->template_expr.pieces[1].type == JAST_TEMPLATE_PIECE_EXPR);
  assert_is_identifier_expr(ex->template_expr.pieces[1].info.expr, "test2");
  assert(ex->template_expr.pieces[2].type == JAST_TEMPLATE_PIECE_STRING);
  assert_string_is_literal(ex->template_expr.pieces[2].info.string, " of template ");
  assert(ex->template_expr.pieces[3].type == JAST_TEMPLATE_PIECE_EXPR);
  assert(ex->template_expr.pieces[3].info.expr->type == JAST_EXPR_BINARY_OP);
  assert(ex->template_expr.pieces[3].info.expr->binary_op_expr.op == JAST_BINARY_OP_ADD);
  JAST_Expr **subs = ex->template_expr.pieces[3].info.expr->binary_op_expr.subs;
  assert_is_identifier_expr(subs[0], "templ");
  assert_is_number_value(subs[1], 2);
  assert(ex->template_expr.pieces[4].type == JAST_TEMPLATE_PIECE_STRING);
  assert_string_is_literal(ex->template_expr.pieces[4].info.string, " class");
  jast_statement_free (stmt);
  }

  {
  stmt = parse_string ("let empty = () => {};");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  assert(sub->vardecls_statement.vars[0].type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(sub->vardecls_statement.vars[0].info.simple, "empty");
  sub_ex = sub->vardecls_statement.vars[0].initializer;
  assert(sub_ex->type == JAST_EXPR_ARROW);
  assert(sub_ex->arrow_expr.n_args == 0);
  assert(sub_ex->arrow_expr.body->type == JAST_STATEMENT_COMPOUND);
  assert(sub_ex->arrow_expr.body->compound_statement.n_subs == 0);
  jast_statement_free (stmt);
  }
  {
  stmt = parse_string ("let square = x => x * x;");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  sub_ex = sub->vardecls_statement.vars[0].initializer;
  assert(sub_ex->type == JAST_EXPR_ARROW);
  assert(sub_ex->arrow_expr.n_args == 1);
  assert_string_is_literal(sub_ex->arrow_expr.args[0], "x");
  assert(sub_ex->arrow_expr.body->type == JAST_STATEMENT_RETURN);
  ex = sub_ex->arrow_expr.body->return_statement.expr;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_MULTIPLY);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0], "x");
  assert_is_identifier_expr(ex->binary_op_expr.subs[1], "x");
  jast_statement_free (stmt);
  }
  {
  stmt = parse_string ("let key_maker = val => ({key: val});");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  sub_ex = sub->vardecls_statement.vars[0].initializer;
  assert(sub_ex->type == JAST_EXPR_ARROW);
  assert(sub_ex->arrow_expr.n_args == 1);
  assert_string_is_literal(sub_ex->arrow_expr.args[0], "val");
  assert(sub_ex->arrow_expr.body->type == JAST_STATEMENT_RETURN);
  ex = sub_ex->arrow_expr.body->return_statement.expr;
  assert(ex->type == JAST_EXPR_OBJECT_VALUE);
  assert(ex->object_value_expr.n_fields == 1);
  assert_string_is_literal(ex->object_value_expr.fields[0].key, "key");
  assert_is_identifier_expr (ex->object_value_expr.fields[0].value, "val");
  jast_statement_free(stmt);
  }
  {
  stmt = parse_string ("let odds = evens.map(v => v + 1);");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  sub_ex = sub->vardecls_statement.vars[0].initializer;
  assert(sub_ex->type == JAST_EXPR_INVOKE);
  assert(sub_ex->invoke_expr.n_args == 1);
  ex = sub_ex->invoke_expr.args[0];
  assert(ex->type == JAST_EXPR_ARROW);
  assert(ex->arrow_expr.n_args == 1);
  assert_string_is_literal(ex->arrow_expr.args[0], "v");
  assert(ex->arrow_expr.body->type == JAST_STATEMENT_RETURN);
  JAST_Expr *rvex = ex->arrow_expr.body->return_statement.expr;
  assert(rvex->type == JAST_EXPR_BINARY_OP);
  assert(rvex->binary_op_expr.op == JAST_BINARY_OP_ADD);
  assert_is_identifier_expr(rvex->binary_op_expr.subs[0], "v");
  assert_is_number_value(rvex->binary_op_expr.subs[1], 1);
  jast_statement_free(stmt);
  }
  {
  stmt = parse_string ("let mag = (x,y) => x*x + y*y;");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  sub_ex = sub->vardecls_statement.vars[0].initializer;
  assert(sub_ex->type == JAST_EXPR_ARROW);
  assert(sub_ex->arrow_expr.n_args == 2);
  assert(sub_ex->arrow_expr.body->type == JAST_STATEMENT_RETURN);
  ex = sub_ex->arrow_expr.body->return_statement.expr;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_ADD);
  assert(ex->binary_op_expr.subs[0]->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.subs[0]->binary_op_expr.op == JAST_BINARY_OP_MULTIPLY);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0]->binary_op_expr.subs[0], "x");
  assert_is_identifier_expr(ex->binary_op_expr.subs[0]->binary_op_expr.subs[1], "x");
  assert(ex->binary_op_expr.subs[1]->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.subs[1]->binary_op_expr.op == JAST_BINARY_OP_MULTIPLY);
  assert_is_identifier_expr(ex->binary_op_expr.subs[1]->binary_op_expr.subs[0], "y");
  assert_is_identifier_expr(ex->binary_op_expr.subs[1]->binary_op_expr.subs[1], "y");
  jast_statement_free (stmt);
  }

  {
  stmt = parse_string("const r = /foo[ab]*\\//s;");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  assert(sub->vardecls_statement.type == JAST_VARIABLE_DECLARATION_CONST);
  assert(sub->vardecls_statement.n_vars == 1);
  assert(sub->vardecls_statement.vars[0].type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(sub->vardecls_statement.vars[0].info.simple, "r");
  assert(sub->vardecls_statement.vars[0].initializer->type == JAST_EXPR_REGEX_VALUE);
  jast_statement_free (stmt);
  }

  {
  stmt = parse_string("const r = function (literally) { return literally + 2; };");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  assert(sub->vardecls_statement.type == JAST_VARIABLE_DECLARATION_CONST);
  assert(sub->vardecls_statement.n_vars == 1);
  assert(sub->vardecls_statement.vars[0].type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(sub->vardecls_statement.vars[0].info.simple, "r");
  assert(sub->vardecls_statement.vars[0].initializer->type == JAST_EXPR_FUNCTION_VALUE);
  JAST_FunctionValue_Expr *f = &sub->vardecls_statement.vars[0].initializer->function_value_expr;
  assert(f->n_args == 1);
  assert_string_is_literal(f->args[0].name, "literally");
  assert(f->body->type == JAST_STATEMENT_COMPOUND);
  assert(f->body->compound_statement.n_subs == 1);
  assert(f->body->compound_statement.subs[0]->type == JAST_STATEMENT_RETURN);
  ex = f->body->compound_statement.subs[0]->return_statement.expr;
  assert (ex->type == JAST_EXPR_BINARY_OP);
  assert (ex->binary_op_expr.op == JAST_BINARY_OP_ADD);
  assert_is_identifier_expr (ex->binary_op_expr.subs[0], "literally");
  assert_is_number_value (ex->binary_op_expr.subs[1], 2.);
  jast_statement_free (stmt);
  }

  {
  stmt = parse_string("with (some_expr) a += b;");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_WITH);
  assert_is_identifier_expr(sub->with_statement.expr, "some_expr");
  assert(sub->with_statement.body->type == JAST_STATEMENT_EXPRESSION);
  ex = sub->with_statement.body->expr_statement.expr;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_ADD_ASSIGN);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0], "a");
  assert_is_identifier_expr(ex->binary_op_expr.subs[1], "b");
  jast_statement_free (stmt);
  }

  {
  stmt = parse_string("let a = [0b10, 0b11, 0B100, 0o7, 0o10, 0O11, 0xaa];");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  ex = sub->vardecls_statement.vars[0].initializer;
  assert(ex->type == JAST_EXPR_ARRAY_VALUE);
  assert(ex->array_value_expr.n_values == 7);
  JAST_Expr **values = ex->array_value_expr.values;
  assert_is_number_value (values[0], 2.);
  assert_is_number_value (values[1], 3.);
  assert_is_number_value (values[2], 4.);
  assert_is_number_value (values[3], 7.);
  assert_is_number_value (values[4], 8.);
  assert_is_number_value (values[5], 9.);
  assert_is_number_value (values[6], 170.);
  jast_statement_free (stmt);
  }

  //
  // Automatic Semicolon Insertion. Section 11.9.
  //
  // Should test every rule that
  //    (1) includes [no LineTerminator here]
  //    (2) includes semicolon
  //
  // TODO for(i = 0\ni < 10\ni++)
  // TODO {return}
  // TODO {i++}
  // TODO foo()
  //
  {
  stmt = parse_string("continue\na;");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 2);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_CONTINUE);
  assert(sub->continue_statement.label == NULL);
  sub = stmt->compound_statement.subs[1];
  assert(sub->type == JAST_STATEMENT_EXPRESSION);
  ex = sub->expr_statement.expr;
  assert_is_identifier_expr(ex, "a");
  jast_statement_free (stmt);
  }
  {
  stmt = parse_string("a\n++\nb");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 2);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_EXPRESSION);
  ex = sub->expr_statement.expr;
  assert_is_identifier_expr(ex, "a");
  sub = stmt->compound_statement.subs[1];
  assert(sub->type == JAST_STATEMENT_EXPRESSION);
  ex = sub->expr_statement.expr;
  assert(ex->type == JAST_EXPR_UNARY_OP);
  assert(ex->unary_op_expr.op == JAST_UNARY_OP_PRE_INCR);
  assert_is_identifier_expr(ex->unary_op_expr.sub, "b");
  jast_statement_free (stmt);
  }
  {
  stmt = parse_string("a=b\nB=A");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 2);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_EXPRESSION);
  ex = sub->expr_statement.expr;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_ASSIGN);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0], "a");
  assert_is_identifier_expr(ex->binary_op_expr.subs[1], "b");
  sub = stmt->compound_statement.subs[1];
  assert(sub->type == JAST_STATEMENT_EXPRESSION);
  ex = sub->expr_statement.expr;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_ASSIGN);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0], "B");
  assert_is_identifier_expr(ex->binary_op_expr.subs[1], "A");
  jast_statement_free (stmt);
  }

  {
  test_parse_failure("{ 1 2 } 3");
  }
  {
  stmt = parse_string("{ 1\n2 } 3");
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 2);
  sub = stmt->compound_statement.subs[0];
  assert(sub->type == JAST_STATEMENT_COMPOUND);
  assert(sub->compound_statement.n_subs == 2);
  assert(sub->compound_statement.subs[0]->type == JAST_STATEMENT_EXPRESSION);
  assert_is_number_value (sub->compound_statement.subs[0]->expr_statement.expr, 1);
  assert(sub->compound_statement.subs[1]->type == JAST_STATEMENT_EXPRESSION);
  assert_is_number_value (sub->compound_statement.subs[1]->expr_statement.expr, 2);
  sub = stmt->compound_statement.subs[1];
  assert(sub->type == JAST_STATEMENT_EXPRESSION);
  assert_is_number_value (sub->expr_statement.expr, 3);
  jast_statement_free (stmt);
  }

  {
  test_parse_failure("for (a; b\n)");
  }
  {
  test_parse_failure("if (a > b)\nelse c = d");
  }
  {
  stmt = parse_string("a = b + c\n(d / e).print()");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert (sub->type == JAST_STATEMENT_EXPRESSION);
  assert (sub->expr_statement.expr->type == JAST_EXPR_BINARY_OP);
  assert (sub->expr_statement.expr->binary_op_expr.op == JAST_BINARY_OP_ASSIGN);
  assert_is_identifier_expr (sub->expr_statement.expr->binary_op_expr.subs[0], "a");
  ex = sub->expr_statement.expr->binary_op_expr.subs[1];
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_ADD);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0], "b");
  ex = ex->binary_op_expr.subs[1];   /* c(d/e).print() */
  assert(ex->type == JAST_EXPR_INVOKE);
  assert(ex->invoke_expr.n_args == 0);
  assert(ex->invoke_expr.function->type == JAST_EXPR_DOT);
  assert_string_is_literal(ex->invoke_expr.function->dot_expr.member_name, "print");
  ex = ex->invoke_expr.function->dot_expr.container;    /* c(d/e) */
  assert(ex->type == JAST_EXPR_INVOKE);
  assert_is_identifier_expr(ex->invoke_expr.function, "c");
  assert(ex->invoke_expr.n_args == 1);
  ex = ex->invoke_expr.args[0];
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_DIVIDE);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0], "d");
  assert_is_identifier_expr(ex->binary_op_expr.subs[1], "e");
  jast_statement_free (stmt);
  }

  // assignment is right-assoc
  {
  stmt = parse_string("a = b = c");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert (sub->type == JAST_STATEMENT_EXPRESSION);
  ex = sub->expr_statement.expr;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_ASSIGN);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0], "a");
  ex = ex->binary_op_expr.subs[1];
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_ASSIGN);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0], "b");
  assert_is_identifier_expr(ex->binary_op_expr.subs[1], "c");
  }
  // addition (and everything except assignment) is left-assoc
  {
  stmt = parse_string("a + b - c");
  assert(stmt);
  assert(stmt->type == JAST_STATEMENT_COMPOUND);
  assert(stmt->compound_statement.n_subs == 1);
  sub = stmt->compound_statement.subs[0];
  assert (sub->type == JAST_STATEMENT_EXPRESSION);
  ex = sub->expr_statement.expr;
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_SUBTRACT);
  assert_is_identifier_expr(ex->binary_op_expr.subs[1], "c");
  ex = ex->binary_op_expr.subs[0];
  assert(ex->type == JAST_EXPR_BINARY_OP);
  assert(ex->binary_op_expr.op == JAST_BINARY_OP_ADD);
  assert_is_identifier_expr(ex->binary_op_expr.subs[0], "a");
  assert_is_identifier_expr(ex->binary_op_expr.subs[1], "b");
  }


  // TODO: module system
  // TODO: generator

  return 0;
}

