#include "../jast.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

static void
assert_string_is_literal (JS_String *s, const char *literal)
{
  assert(strcmp (literal, (char*) (s+1)) == 0);
}

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

  stmt = parse_string ("");
  assert (stmt->type == JAST_STATEMENT_COMPOUND);
  assert (stmt->compound_statement.n_subs == 0);
  jast_statement_free ((JAST_Statement *) stmt);

  stmt = parse_string ("let v = 4;");
  assert (stmt->type == JAST_STATEMENT_COMPOUND);
  assert (stmt->compound_statement.n_subs == 1);
  JAST_Statement *tmp;
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


  return 0;
}

