/* Filenames and positions in source code */
typedef struct {
  JAST_Filename *filename;
  unsigned line_number;
} JAST_Position;

/* Tokens */
typedef enum
{
  JAST_TOKEN_IF,
  JAST_TOKEN_FOR,
  JAST_TOKEN_IN,
  JAST_TOKEN_DO,
  JAST_TOKEN_WHILE,
  JAST_TOKEN_WITH,
  JAST_TOKEN_SWITCH,
  JAST_TOKEN_CASE,
  JAST_TOKEN_DEFAULT,
  JAST_TOKEN_VAR,
  JAST_TOKEN_CONST,
  JAST_TOKEN_LET,
  JAST_TOKEN_NULL,
  JAST_TOKEN_TRUE,
  JAST_TOKEN_FALSE,
  JAST_TOKEN_UNDEFINED,

  JAST_TOKEN_BAREWORD,
  JAST_TOKEN_STRING,

  JAST_TOKEN_LPAREN,
  JAST_TOKEN_RPAREN,
  JAST_TOKEN_LBRACE,
  JAST_TOKEN_RBRACE,
  JAST_TOKEN_LBRACKET,
  JAST_TOKEN_RBRACKET,
  JAST_TOKEN_COMMA,

  JAST_TOKEN_SEMICOLON,

  JAST_TOKEN_PLUS,
  JAST_TOKEN_MINUS,
  JAST_TOKEN_MULTIPLY,
  JAST_TOKEN_DIVIDE,
  JAST_TOKEN_LOGICAL_NOT,
  JAST_TOKEN_BITWISE_NOT,
  JAST_TOKEN_BITWISE_AND,
  JAST_TOKEN_BITWISE_OR,
  JAST_TOKEN_LOGICAL_AND,
  JAST_TOKEN_LOGICAL_OR,
  JAST_TOKEN_COLON,
  JAST_TOKEN_QUESTION_MARK,
  JAST_TOKEN_DOT
} JAST_TokenType;

typedef struct {
  JAST_TokenType type;
  unsigned offset, length;
  unsigned token_index;
  JAST_Position position;
} JAST_Token;

/* Lexing */

typedef struct {
  size_t           data_size;
  const char      *data;
  size_t           offset;
  JAST_Position    position;

  JAST_Token cur, next, next_next;
  unsigned has_cur : 1, has_next : 1, has_next_next : 1;

  JAST_ParseError *error;
} JAST_Lexer;

typedef enum {
  JAST_LEXER_ADVANCE_OK,
  JAST_LEXER_ADVANCE_EOF,
  JAST_LEXER_ADVANCE_ERROR
} JAST_Lexer_AdvanceResult;


static JAST_Lexer_AdvanceResult
lexer_get_next_token_raw_bareword (JAST_Lexer *lexer, JAST_Token *token)
{
  const char *end = lexer->data + lexer->data_size;
  const char *bareword_start = lexer->data + lexer->offset;
  const char *at = bareword_start;
  if (!is_bareword_init_char(&at, end))
    {
      lexer->error = jast_parse_error_new ("bad character");
      return JAST_LEXER_ADVANCE_ERROR;
    }
  while (is_bareword_noninit_char(&at, end))
    {
      /* just keep waiting for a non-bareword character. */
    }
  token->length = at - bareword_start;
  lexer->offset = at - lexer->data;
  return JAST_LEXER_ADVANCE_OK;
}

static JAST_Lexer_AdvanceResult
lexer_get_next_token_raw_qstring (JAST_Lexer *lexer, JAST_Token *token)
{
  /* See spec 11.8.4 */
  const char *end = lexer->data + lexer->data_size;
  const char *q_start = lexer->data + lexer->offset;
  const char *at = q_start + 1;
  char terminal = *q_start;
  while (at < end && *at != terminal)
    {
      if (*at == '\\')
        {
          ...
        }
      else if (*at == '\n')
        {
          ... handle multiline strings?
        }
      else
        {
          at++;
        }
    }
  if (at == end)
    {
      lexer->error = unterminated quoted string
      return JAST_LEXER_ADVANCE_ERROR;
    }
  token->length = at + 1 - q_start;
  lexer->offset = at + 1 - lexer->data;
  return JAST_LEXER_ADVANCE_OK;
}
static JAST_Lexer_AdvanceResult
lexer_get_next_token_raw_number (JAST_Lexer *lexer, JAST_Token *token)
{
...
}

static JAST_Lexer_AdvanceResult
lexer_get_next_token_raw_regex (JAST_Lexer *lexer, JAST_Token *token)
{
  /* See Spec 11.8.5 */
  ...
}

/* Ignores cur/next/next_next and just lexes a token from the buffer. */
static JAST_Lexer_AdvanceResult
lexer_get_next_token_raw (JAST_Lexer *lexer, JAST_Token *token)
{
  /* Skip whitespace */
  const char *at = lexer->data + lexer->offset;
  const char *end = lexer->data + lexer->data_size;
  JAST_Boolean on_new_line = lexer->token_index == 0;
  while (at < end)
    {
      if (*at == ' ')
        at++;
      else if (*at == '\t')
        at++;
      else if (*at == '\n')
        {
          lexer->position.line_number += 1;
          on_new_line = TRUE;
        }
      else
        break;
    }
  token->on_new_line = on_new_line;
  if (at == end)
    return JAST_LEXER_ADVANCE_EOF;
  token->token_index = lexer->n_tokens_output;
  token->position = lexer->position;
  token->offset = at - lexer->data;

#define IMPLEMENT_SINGLE_CHARACTER_TOKEN(token_shortname) \
    do{ token->type = JAST_TOKEN_LOGICAL_##token_shortname; \
        token->length = 1; \
        at++; }while(0)
#define IMPLEMENT_TWO_CHARACTER_TOKEN(token_shortname) \
    do{ token->type = JAST_TOKEN_LOGICAL_##token_shortname; \
        token->length = 2; \
        at+=2; }while(0)
#define IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(token_shortname) \
    do{ if (rem > 1 && at[1] == '=') \
          IMPLEMENT_TWO_CHARACTER_TOKEN(token_shortname##_ASSIGN); \
        else \
          IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(token_shortname); \
    }while(0)


  switch (*at)
    {
      case '!':
        if (rem > 1 && at[1] == '=')
          {
            if (rem > 2 && at[2] == '=')
              IMPLEMENT_THREE_CHARACTER_TOKEN(CMP_EXACT_NE);
            else
              IMPLEMENT_TWO_CHARACTER_TOKEN(CMP_NUM_NE);
          }
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN(LOGICAL_NOT);
        break;

      case '~':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(BITWISE_NOT);
        break;

      case '%':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(MOD);
        break;

      case '^':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(BITWISE_XOR);
        break;

      case '&':
        if (rem > 1 && at[1] == '&')
          IMPLEMENT_TWO_CHARACTER_TOKEN(LOGICAL_AND);
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(BITWISE_AND);
        break;

      case '|':
        if (rem > 1 && at[1] == '|')
          IMPLEMENT_TWO_CHARACTER_TOKEN(LOGICAL_OR);
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(BITWISE_OR);
        break;

      case '*':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(MULTIPLY);
        break;

      case '(':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(LPAREN);
        break;

      case ')':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(RPAREN);
        break;

      case '[':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(LBRACKET);
        break;
      case ']':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(RBRACKET);
        break;
      case '{':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(LBRACE);
        break;
      case '}':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(RBRACE);
        break;
      case ',':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(COMMA);
        break;
      case '.':
        if (rem > 1 && '0' <= at[1] && at[1] <= '9')
          {
            lexer->last_token_type = JAST_TOKEN_NUMBER;
            return lexer_get_next_token_raw_number(lexer, token);
          }
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN(DOT);
        break;

      case ';':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(SEMICOLON);
        break;

      case '/':
        if (rem > 1 && at[1] == '/')
          {
            ... one-line comment
          }
        else if (rem > 1 && at[1] == '*')
          {
            ... multi-line comment
          }
        else if (last_token_permits_regex_literal (lexer->last_token_type)
          && could_be_regex (at, end))
          {
            lexer->last_token_type = JAST_TOKEN_REGEX;
            return lexer_get_token_raw_regex (lexer, token);
          }
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(DIVIDE);
        break;

      case '?':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(QUESTION_MARK);
        break;

      case '"':
      case '\'':
        return lexer_get_next_token_raw_qstring(lexer, token);

      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        lexer->last_token_type = JAST_TOKEN_NUMBER;
        return lexer_get_next_token_raw_number(lexer, token);

      case '<':
        if (rem > 1 && at[1] == '<')
          IMPLEMENT_TWO_CHARACTER_TOKEN(LEFT_SHIFT);
        else if (rem > 1 && at[1] == '=')
          IMPLEMENT_TWO_CHARACTER_TOKEN(CMP_LE);
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN(CMP_LT);
        break;

      case '>':
        if (rem > 1 && at[1] == '>')
          IMPLEMENT_TWO_CHARACTER_TOKEN(RIGHT_SHIFT);
        else if (rem > 1 && at[1] == '=')
          IMPLEMENT_TWO_CHARACTER_TOKEN(CMP_GE);
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN(CMP_GT);
        break;

      case '=':
        if (rem > 1 && at[1] == '=')
          {
            if (rem > 2 && at[2] == '=')
              IMPLEMENT_THREE_CHARACTER_TOKEN(CMP_EXACT_EQ);
            else
              IMPLEMENT_TWO_CHARACTER_TOKEN(CMP_NUM_EQ);
          }
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN(ASSIGN);
        break;

      default:
        lexer->last_token_type = JAST_TOKEN_BAREWORD;
        return lexer_get_next_token_raw_bareword(lexer, token);
    }

  lexer->last_token_type = 
}

static JAST_Boolean
jast_lexer_init (JAST_Lexer       *lexer,
                 size_t            data_size,
                 const char       *data,
                 const char       *filename,
                 JAST_ParseError **error)
{
  lexer->data_size = data_size;
  lexer->data = data;
  lexer->byte_offset = 0;
  lexer->position.filename = JAST_filename_make(filename);
  lexer->position.line_number = 1;
  switch (lexer_get_next_token_raw (lexer, &lexer->cur))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        lexer->has_cur = lexer->has_next = lexer->has_next_next = FALSE;
        return TRUE;
      case JAST_LEXER_ADVANCE_ERROR:
        JAST_filename_unref(lexer->position.filename);
        return FALSE;
    }
  lexer->has_cur = TRUE;
  switch (lexer_get_next_token_raw (lexer, &lexer->next))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        lexer->has_next = lexer->has_next_next = FALSE;
        return TRUE;
      case JAST_LEXER_ADVANCE_ERROR:
        JAST_filename_unref(lexer->position.filename);
        return FALSE;
    }
  lexer->has_next = TRUE;
  switch (lexer_get_next_token_raw (lexer, &lexer->next_next))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        lexer->has_next_next = FALSE;
        return TRUE;
      case JAST_LEXER_ADVANCE_ERROR:
        JAST_filename_unref(lexer->position.filename);
        return FALSE;
    }
  lexer->has_next_next = TRUE;
  return TRUE;
}

static JAST_Lexer_AdvanceResult jast_lexer_advance (JAST_Lexer *lexer)
{
  if (!lexer->has_cur)
    {
      lexer->error = ... read past eof
      return JAST_LEXER_ADVANCE_ERROR;
    }
  lexer->has_cur = lexer->has_next
  lexer->has_next = lexer->has_next_next;
  lexer->cur = lexer->next
  lexer->next = lexer->next_next;
  if (lexer->has_next)
    {
      switch (lexer_get_next_token_raw (lexer, &lexer->next_next))
        {
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_EOF:
          lexer->has_next_next = FALSE;
          break;
        case JAST_LEXER_ADVANCE_ERROR:
          return JAST_LEXER_ADVANCE_ERROR;
        }
    }
  if (!lexer->has_cur)
    return JAST_LEXER_ADVANCE_EOF;
  return JAST_LEXER_ADVANCE_OK;
}

typedef enum {
  MATCHER_TYPE_BY_TOKEN_TYPE,
  MATCHER_TYPE_VIRTUAL,
} MatcherType;

struct MatcherVirtual
{
  void *(*parse)(JAST_Lexer *lexer);    /* store error in lexer */
  void (*destroy)
};
  

struct MatcherPiece {
  MatcherType matcher_type;
  union {
    JAST_TokenType by_token_type;
    struct MatcherVirtual *matcher_virtual;
  } info;
};

static JAST_Boolean
match_pieces (JAST_Lexer *lexer,
              size_t      n_pieces,
              struct MatcherPiece *pieces,
              void      **virtuals_out,
              JAST_ParseError **error)
{
  size_t i;
  for (i = 0; i < n_pieces; i++)
    {
      switch (pieces[i].matcher_type)
        {
          case MATCHER_TYPE_BY_TOKEN_TYPE:
            if (!lexer.has_cur)
              {
                ...
              }
            if (lexer.cur.type != pieces[i].info.by_token_type)
              {
                ..
              }
            if (jast_lexer_advance (lexer) == JAST_LEXER_ADVANCE_ERROR)
              goto handle_error;
            break;

          case MATCHER_TYPE_VIRTUAL:
            {
              struct MatcherVirtual *v = pieces[i].info.matcher_virtual;
              void *obj = v->parse(lexer);
              if (obj == NULL)
                goto handle_error;
              virtuals_out[nv++] = obj;
              break;
            }
        }
    }
  return TRUE;
}


static struct MatcherPiece if_stmt_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(IF),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_statement),
};

static struct MatcherPiece else_if_stmt_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(ELSE),
  MATCHER_PIECE_BY_TOKEN_TYPE(IF),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_statement),
};

static struct MatcherPiece else_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(ELSE),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_statement),
};

static JAST_Statement *
parse_if_stmt(JAST_Lexer *lexer)
{
  JAST_Token lparen_token;
  void *pad[2];
#define CONDITIONAL_STATEMENT_STACK_INIT_SIZE 8
  JAST_ConditionalStatement_Clause cstmts_stack[CONDITIONAL_STATEMENT_STACK_INIT_SIZE];
  JAST_ConditionalStatement_Clause *cstmts = cstmts_stack;
  size_t cstmts_alloced = CONDITIONAL_STATEMENT_STACK_INIT_SIZE;

  if (!DO_STATIC_MATCHES(lexer, if_stmt_pieces, pad))
    return NULL;
  cstmts[0].expr = pad[0];
  cstmts[0].statement = pad[1];
  size_t n_cstmts = 1;

  JAST_Statement *else_statement = NULL;

  while (lexer->has_cur && lexer->cur.type == JAST_TOKEN_ELSE)
    {
      if (lexer->has_next && lexer->next.type == JAST_TOKEN_IF)
        {
          if (!DO_STATIC_MATCHES (lexer, else_if_stmt_pieces, pad, err))
            goto error;
          if (n_cstmts == cstmts_alloced)
            {
              size_t old_size = cstmts_alloced * sizeof (JAST_ConditionalStatement_Clause);
              cstmts_alloced *= 2;
              size_t size = cstmts_alloced * sizeof (JAST_ConditionalStatement_Clause);
              if (cstmts == cstmts_stack)
                cstmts = memcpy (malloc (size), cstmts, old_size);
              else
                cstmts = realloc (cstmts, size);
            }
          cstmts[n_cstmts].expr = pad[0];
          cstmts[n_cstmts].statement = pad[1];
          n_cstmts++;
        }
      else
        {
          /* simple else */
          if (!DO_STATIC_MATCHES (lexer, else_stmt_pieces, pad, err))
            goto error;
          else_statement = pad[0];
          break;
        }
    }

  size_t cstmt_size = sizeof(JAST_ConditionalStatement_Clause) * n_cstmts;
  size_t rv_size = sizeof(JAST_Statement_If) + cstmt_size;
  JAST_Statement_If *rv = alloc_statement(JAST_Statement_If, rv_size);
  rv->n_conditional_statements = n_cstmts;
  rv->conditional_statements = (JAST_ConditionalStatement_Clause*)(rv+1);
  memcpy(rv->conditional_statements, cstmts, cstmt_size);
  rv->else_statement = else_statement;
  if (cstmts != cstmts_stack)
    free (cstmts);
  return (JAST_Statement *) rv;
}
static struct MatcherPiece while_loop_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(WHILE),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_statement),
};

static JAST_Statement *
parse_while_stmt(JAST_Lexer *lexer)
{
  void pad[2];
  if (!DO_STATIC_MATCHES (lexer, while_loop_pieces, pad))
    return NULL;
  JAST_Statement_While *rv = alloc_statement(JAST_Statement_While, sizeof(JAST_Statement_While));
  rv->condition = pad[0];
  rv->body = pad[1];
  return (JAST_Statement *) rv;
}

static struct MatcherPiece do_while_loop_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(DO),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_statement),
  MATCHER_PIECE_BY_TOKEN_TYPE(WHILE),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
};
static JAST_Statement *
parse_do_while_stmt (JAST_Lexer *lexer)
{
  void pad[2];
  if (!DO_STATIC_MATCHES (lexer, do_while_loop_pieces, pad))
    return NULL;
  JAST_Statement_DoWhile *rv = alloc_statement(JAST_STATEMENT_DO_WHILE, sizeof(JAST_Statement_DoWhile));
  rv->body = pad[0];
  rv->condition = pad[1];
  return (JAST_Statement *) rv;
}

static JAST_Statement *
parse_either_for_statement (JAST_Lexer *lexer)
{
...
}

static JAST_Statement *
parse_with_statement (JAST_Lexer *lexer)
{
...
}

static JAST_Statement *
parse_switch_statement (JAST_Lexer *lexer)
{
...
}

static JAST_Statement *
parse_vardecl_statement (JAST_Lexer *lexer)
{
...
}

static JAST_Statement *
parse_compound_stmt_without_braces (JAST_Lexer *lexer)
{
  JAST_Statement *subs_stack[16];
  size_t subs_alloced = N_ELEMENTS(subs);
  JAST_Statement **subs = subs_stack;
  size_t n_subs = 0;
  while (lexer->has_cur && !lexer->cur.type == JAST_TOKEN_RBRACE)
    {
      JAST_Statement *stmt = parse_stmt (lexer);
      if (stmt == NULL)
        {
          ...
        }
      if (n_subs == subs_alloced)
        {
          ...
        }
      ...
      subs[n_subs++] = stmt;
    }
  size_t rv_size = sizeof (JAST_Statement_Compound)
                 + sizeof (JAST_Statement *) * n_subs;
  JAST_Statement_Compound *rv = alloc_statement(...rv_size...);
  rv->n_subs = n_subs;
  rv->subs = (JAST_Statement **) (rv + 1);
  memcpy (rv->subs, subs, sizeof (JAST_Statement*) * n_subs);
  if (subs != subs_stack)
    free(subs);
  return rv;

error_cleanup:
  for (size_t i = 0; i < n_subs; i++)
    jast_statement_free (subs[i]);
  if (subs != subs_stack)
    free(subs);
  return NULL;
}


static JAST_Statement *
parse_compound_stmt (JAST_Lexer *lexer)
{
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_EOF:
      lexer->error = premature_eof_parse_error ();
      return NULL;
    case JAST_LEXER_ADVANCE_ERROR:
      return NULL;
    }
  JAST_Statement *rv = parse_compound_stmt_without_braces (lexer);
  if (rv == NULL)
    return NULL;
  if (!lexer->has_cur)
    {
      jast_statement_free (rv);
      lexer->error = premature_eof_parse_error ();
      return NULL;
    }
  assert(lexer->cur.type == JAST_TOKEN_RBRACE);
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
    case JAST_LEXER_ADVANCE_EOF:
      return rv;
    case JAST_LEXER_ADVANCE_ERROR:
      jast_statement_free (rv);
      return NULL;
    }
}
    

/* --- expression parsing --- */
typedef struct {
  JAST_Boolean is_op;
  union {
    struct {
      JAST_TokenType token_type;
      unsigned precedence;
      unsigned tag;
    } op;
    JAST_Expr *expr;
  } info;
} ExprOpChainPiece;

static JAST_Expr *
parse_normalized_op_chain (size_t n_pieces,
                           ExprOpChainPiece *pieces,
                           JAST_ParseError **err_out)
{
  if (n_pieces == 1)
    {
      assert(!pieces[0].is_op);
      return pieces[0].info.expr;
    }

  /* Find op with lowest precedence */
  unsigned lowest_prec = pieces[1].info.op.precedence;
  for (size_t i = 3; i < n_pieces; i += 2)
    {
      unsigned prec = pieces[i].info.op.precedence;
      if (prec < lowest_prec)
        lowest_prec = prec;
    }

  size_t in_at = 0;
  size_t out_at = 0;
  size_t subsize = 1;
  while (in_at < n_pieces)
    {
      while (in_at + subsize < n_pieces
          && pieces[in_at+subsize].op.precedence != lowest_prec)
        {
          subsize += 2;
        }
      if (subsize == 1)
        {
          pieces[out_at++] = pieces[in_at++];
        }
      else
        {
          JAST_Expr *expr = parse_normalized_op_chain (subsize, pieces + in_at);
          if (expr == NULL)
            {
              ...
            }
          pieces[out_at].is_op = FALSE;
          pieces[out_at].info.expr = expr;
          out_at += 1;
          in_at += subsize;
        }
      if (in_at < n_pieces)
        {
          assert(in_at % 2 == 1);
          pieces[out_at++] = pieces[in_at++];
        }
    }

  /* Evaluate from L-to-R or R-to-L depending on associativity. */
  if (lowest_prec == JAST_PRECEDENCE_LEVEL_TERNARY)
    {
      /* ternary op requires special parsing */
      for (unsigned i = 1; i < n_pieces; i += 2)
        {
          assert (pieces[i].is_op);
          JAST_Boolean expect_colon = i / 2 % 2;
          JAST_TokenType expected_tt = expect_colon ? JAST_TOKEN_COLON : JAST_TOKEN_QUESTION_MARK;
          if (expect_colon)
            {
              if (pieces[i].info.op.token_type != JAST_TOKEN_COLON)
                {
                  error: 
                }
            }
          else
            {
              if (pieces[i].info.op.token_type != JAST_TOKEN_COLON)
                {
                  error: 
                }
            }
        }
      if (n_pieces % 4 != 1)
        {
          error: expected terminal ':' */
        }
      size_t n_exprs = (n_pieces + 1) / 2;
      size_t rv_size = sizeof (JAST_Expr_Cond)
                     + sizeof (JAST_Expr *) * n_exprs;
      JAST_Expr_Cond *rv = alloc_expr (JAST_EXPR_TYPE_COND, rv_size);
      rv->n_terms = n_exprs;
      rv->terms = (JAST_Expr **) (rv + 1);
      for (unsigned i = 0; i < n_pieces; i += 2)
        rv->terms[i/2] = pieces[i].info.expr;
      return (JAST_Expr *) rv;
    }
  else if (is_precedence_level_left_associative (lowest_prec))
    {
      /* handle left-associative operators */
      JAST_Expr *rv = pieces[0].info.expr;
      for (unsigned i = 2; i < n_pieces; i += 2)
        {
          JAST_TokenType op_token = pieces[i-1].info.op.token_type;
          rv = create_binary_expr(rv, op_token, pieces[i].info.expr);
        }
      return rv;
    }
  else
    {
      /* handle right-associative operators */
      JAST_Expr *rv = pieces[n_pieces-1].info.expr;
      for (unsigned i = 2; i < n_pieces; i += 2)
        {
          JAST_TokenType op_token = pieces[n_pieces-i].info.op.token_type;
          rv = create_binary_expr(pieces[n_pieces-i-1].info.expr, op_token, rv);
        }
      return rv;
    }
}

static JAST_Expr *
parse_expr(JAST_Lexer *lexer)
{
  ExprOpChainPiece pieces_stack[16];
  ExprOpChainPiece *pieces = pieces_stack;
  size_t pieces_alloced = N_ELEMENTS(pieces_stack);
  size_t n_pieces = 0;

  while (lexer->has_cur
      && !is_expression_terminator_token_type (lexer->cur.type))
    {
      ExprOpChainPiece piece;
      unsigned prec;
      if (is_normal_operator (lexer->cur.type, &prec))
        {
          piece.is_op = TRUE;
          piece.info.op.token_type = lexer->cur.type;
          piece.info.op.binary_op_precedence = prec;
        }
      else
        {
          JAST_Expr *expr = parse_opfree_expr (lexer);
          if (expr == NULL)
            {
              piece.is_op = FALSE;
              piece.info.expr = expr;
            }
          else
            {
              // cleanup
              for (size_t i = 0; i < n_pieces; i++)
                if (!pieces[i].is_op)
                  jast_expr_unref (pieces[i].info.expr);
              if (pieces != pieces_stack)
                free (pieces);
              return NULL;
            }
        }
      if (n_pieces == pieces_alloced)
        {
          size_t old_size = sizeof (ExprOpChainPiece) * pieces_alloced;
          pieces_alloced *= 2;
          size_t size = sizeof (ExprOpChainPiece) * pieces_alloced;
          if (pieces == pieces_stack)
            pieces = memcpy (malloc (size), pieces, old_size);
          else
            pieces = realloc (pieces, size);
        }
      pieces[n_pieces++] = piece;
    }

  /* Handle initial prefix operators. */
  unsigned piece_i = 0;
  while (piece_i < n_pieces && pieces[piece_i].is_op)
    {
      if (!token_type_is_prefix_op (pieces[piece_i].op.token_type))
        {
          lexer->error = parse_error("non-prefix operator");
          goto free_pieces_error;
        }
      pieces[piece_i].op.tag = TAG_PREFIX;
    }
  if (piece_i == n_pieces)
    {
      lexer->error = parse_error("missing expression");
      goto free_pieces_error;
    }
  while (piece_i < n_pieces)
    {
      unsigned next_piece_i = piece_i + 1;
      while (next_piece_i < n_pieces && pieces[next_piece_i].is_op)
        next_piece_i++;
      if (next_piece_i == n_pieces)
        {
          /* last piece: all remaining ops must be postfix ops */
          ...
        }
      else 
        {
          /* see if we can assign prefix/infix/postfix to the op list */
          unsigned n_ops = next_piece_i - piece_i - 1;
          if (n_ops == 0)
            {
              /* error: two adjacent expressions. */
              lexer->error = parse_error("missing operator");
              goto free_pieces_error;
            }
          else
            {
              unsigned max_prefix_ops = 0;
              unsigned max_postfix_ops = 0;
              ...

              ... now look for the first infix operator that yields something parsable
            }
        }
      piece_i = next_piece_i;
    }

  /* Reduce prefix and postfix operators. */
  unsigned out_at = 0;
  unsigned in_at = 0;
  while (in_at < n_pieces)
    {
      unsigned next_expr = in_at;
      while (next_expr < n_pieces && pieces[next_expr].is_op)
        next_expr++;
      if (next_expr == n_pieces)
        {
          lexer->error = parse_error("expected expression");
          n_pieces = out_at;
          goto free_pieces_error;
        }
      JAST_Expr *expr = pieces[next_expr].info.expr;
      size_t n_prefix_ops = 0;
      ExprOpChainPiece p = pieces + next_expr - n_prefix_ops - 1;
      while (in_at < next_expr - n_prefix_ops - 1
          && p->info.op.tag == TAG_PREFIX)
        {
          JAST_TokenType tt = p->info.op.token_type;
          JAST_UnaryOp_Type uot = token_type_to_prefix_op (tt);
          expr = jast_expr_new_unary (uot, expr);
          n_prefix_ops++;
          p--;
        }
      p = pieces + next_expr + 1;
      while (next_expr + n_postfix_ops + 1 < n_pieces
          && p->info.op.tag == TAG_POSTFIX)
        {
          JAST_TokenType tt = p->info.op.token_type;
          JAST_UnaryOp_Type uot = token_type_to_postfix_op (tt);
          expr = jast_expr_new_unary (uot, expr);
          n_postfix_ops++;
          p++;
        }
      while (in_at < next_expr - n_prefix_ops)
        pieces[out_at++] = pieces[in_at++];
      pieces[out_at].is_op = FALSE;
      pieces[out_at].info.expr = expr;
      out_at++;
      in_at = next_expr + n_postfix_ops;
    }
  n_pieces = out_at;

  /* Assert that the new state of pieces
   * is EXPR OP EXPR OP ... OP EXPR
   *
   * Should be guaranteed by avoid code, but
   * verify for sanity's sake.
   */
  assert(n_pieces % 2 == 1);
  for (size_t j = 0; j < n_pieces; j++)
    assert(pieces[j].is_op == (j % 2 == 1));

  /* Now the chain should be in EXPR OP EXPR OP ... OP EXPR format.
   * Find the lowest prec level in use,
   * and recurse on the remaining expressions.
   * Then make binary expressions l-r or r-l 
   */
  JAST_ParseError *op_chain_err;
  expr = parse_normalized_op_chain(n_pieces, pieces, &op_chain_err);
  if (pieces != pieces_stack)
    free(pieces);
  if (expr == NULL)
    lexer->error = op_chain_err;
  return expr;

free_pieces_error:
  for (size_t j = 0; j < n_pieces; j++)
    if (!pieces[j].is_op)
      jast_expr_free (pieces[j].info.expr);
  if (pieces != pieces_stack)
    free (pieces);
  return NULL;
}

static JAST_Statement *
jast_parse_statement (JAST_Lexer *lexer)
{
  if (!lexer->has_cur)
    break;

  switch (lexer->cur.type)
    {
      case JAST_TOKEN_IF:
        return parse_if_stmt(lexer);

      case JAST_TOKEN_WHILE:
        return parse_while_stmt(lexer);

      case JAST_TOKEN_DO:
        return parse_do_while_stmt (lexer);

      case JAST_TOKEN_FOR:
        return parse_either_for_statement (lexer);

      case JAST_TOKEN_WITH:
        return parse_with_statement(lexer, token, &suberr);

      case JAST_TOKEN_SWITCH:
        stmt = parse_switch_statement(lexer);
        if (stmt == NULL)
          goto error_cleanup;
        else
          goto got_statement;

      case JAST_TOKEN_LET:
      case JAST_TOKEN_VAR:
      case JAST_TOKEN_CONST:
        return parse_vardecl_statement(lexer);

      case JAST_TOKEN_LBRACE:
        {
          if ( (lexer->has_next_next
                   && lexer->next_next.type == JAST_TOKEN_COLON)
            || (lexer->has_next
                   && lexer->next.type == JAST_TOKEN_RBRACE) )
            {
              /* Empty object literal. */
              expr = parse_object_literal_expr(lexer);
              if (expr == NULL)
                return NULL;
              return jast_statement_from_expr(expr);
            }
          else
            {
              /* scope */
              return parse_compound_stmt(lexer);
            }
        }
        break;
      case JAST_TOKEN_BAREWORD:
        {
          if (lexer->has_next && lexer->next.type == JAST_TOKEN_COLON)
            {
              /* label */
              JAST_String *label = token_to_string(&lexer->cur, lexer->data);
              if (jast_lexer_advance(lexer) == JAST_LEXER_ADVANCE_ERROR
               || jast_lexer_advance(lexer) == JAST_LEXER_ADVANCE_ERROR)
                {
                  return NULL;
                }
              JAST_Statement_Label *rv = alloc_statement(JAST_STATEMENT_LABEL, sizeof (JAST_Statement_Label));
              rv->label = label;
              return (JAST_Statement *) rv;
            }
          else
            {
              expr = parse_expr(lexer);
              if (expr == NULL)
                goto error_cleanup;
              else
                return jast_statement_from_expr(expr);
            }
        }

      /* Statements that are just expressions. */
      case JAST_TOKEN_FUNCTION:
      case JAST_TOKEN_STRING:
      case JAST_TOKEN_NUMBER:

      case JAST_TOKEN_TRUE:
      case JAST_TOKEN_FALSE:
      case JAST_TOKEN_NULL:
      case JAST_TOKEN_UNDEFINED:

      case JAST_TOKEN_LPAREN:
      case JAST_TOKEN_MINUS:
      case JAST_TOKEN_BITWISE_NOT:
      case JAST_TOKEN_LOGICAL_NOT:
      case JAST_TOKEN_REGEX:

      case JAST_TOKEN_LBRACKET:
        expr = parse_expr(lexer);
        if (expr == NULL)
          return NULL;
        else
          return jast_statement_from_expr(expr);

      default:
        assert(0);
        break;
    }
}

JAST_Statement *
JAST_parse_data (size_t            data_size,
                 const char       *data,
                 const char       *filename,
                 JAST_ParseError **error)
{
  JAST_Lexer lexer;
  if (!jast_lexer_init(&lexer, data_size, data, filename, error))
    return NULL;

  JAST_Statement *rv = parse_compound_stmt_without_braces (&lexer);
  if (rv == NULL)
    {
      *error = lexer->error;
      lexer->error = NULL;
    }
  return rv;
}
