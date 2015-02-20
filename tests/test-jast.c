
static void
assert_string_is_literal (JS_String *s, const char *literal)
{
  assert(strcmp (literal, (char*) (s+1)) == 0);
}

static JAST_Statement_Compound *
parse_string (const char *str)
{
  JAST_ParseError *error = NULL;
  JAST_Statement_Compound *rv = JAST_parse_data (strlen (str), str, "file.js", &error);
  if (!rv) {
    fprintf(stderr, "error parsing: %s\n", jast_parse_error_to_string(error));
    exit(1);
  }
  return rv;
}

int main()
{
  JAST_Statement_Compound *stmt;

  stmt = parse_string ("");
  assert (stmt->n_children == 0);
  jast_statement_free ((JAST_Statement *) stmt);

  stmt = parse_string ("let v = 4;");
  assert(stmt->n_children == 1);
  assert(stmt->children[0]->type == JAST_STATEMENT_VARIABLE_DECLARATIONS);
  assert(stmt->children[0]->vardecl_stmt.n_vars == 1);
  assert(stmt->children[0]->vardecl_stmt.vars[0].type == JAST_BINDING_PATTERN_SIMPLE);
  assert_string_is_literal(stmt->children[0]->vardecl_stmt.vars[0].info.simple, "v");
  assert(stmt->children[0]->vardecl_stmt.vars[0].initializer != NULL);
  assert(stmt->children[0]->vardecl_stmt.vars[0].initializer->type == JAST_EXPR_NUMBER);
  assert(stmt->children[0]->vardecl_stmt.vars[0].initializer->number_expr.value == 4.);
  jast_statement_free ((JAST_Statement *) stmt);


  return 0;
}

