
typedef struct CompilerStatus {
  JSUS_Command_Scope *scope;
  JSUS_CompileOutput output;
} CompilerStatus;

static void
do_compile (JAST_Statement *stmt,
            CompilerStatus *status)
{
  JSUS_Command_Scope *scope = jsus_command_scope_new ();
  JAST_Base_Statement *base_stmt = (JAST_Base_Statement *) stmt;
  JAST_StatementType stmt_type = base_stmt->type;
  switch (stmt_type)
    {
    case JAST_STATEMENT_COMPOUND:
    case JAST_STATEMENT_IF:
    case JAST_STATEMENT_SWITCH:
    case JAST_STATEMENT_FOR:
    case JAST_STATEMENT_FOR_IN:
    case JAST_STATEMENT_WHILE:
    case JAST_STATEMENT_DO_WHILE:
    case JAST_STATEMENT_WITH:
    case JAST_STATEMENT_VARIABLE_DECLARATIONS:
    case JAST_STATEMENT_TRY_CATCH:
    case JAST_STATEMENT_THROW:
    case JAST_STATEMENT_LABEL:
    case JAST_STATEMENT_BREAK:
    case JAST_STATEMENT_CONTINUE:
    case JAST_STATEMENT_RETURN:
    case JAST_STATEMENT_EXPRESSION:
    default:
      assert(false);
    }
}
