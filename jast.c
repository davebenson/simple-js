#include "jast.h"
#include "jutf8.h"
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#define LBRACE_CHAR   '{'
#define LBRACE_STR    "{"
#define RBRACE_CHAR   '}'
#define RBRACE_STR    "}"

#define IS_DECIMAL_DIGIT(c)       \
  ('0' <= (c) && (c) <= '9')
#define IS_OCTAL_DIGIT(c)         \
  ('0' <= (c) && (c) <= '7')
#define IS_BINARY_DIGIT(c)        \
  ('0' <= (c) && (c) <= '1')
#define IS_HEXIDECIMAL_DIGIT(c)   \
  (  IS_DECIMAL_DIGIT(c)          \
  || ('a' <= (c) && (c) <= 'f')   \
  || ('A' <= (c) && (c) <= 'F') )

#define DEFINE_STACK_STARTED_ARRAY(type, base_name, stack_size) \
  size_t n_##base_name = 0; \
  size_t base_name##_alloced = (stack_size); \
  type base_name##_stack[stack_size]; \
  type *base_name = base_name##_stack

#define APPEND_TO_STACK_STARTED_ARRAY(type, base_name, to_append) \
  do{ \
    if (n_##base_name == base_name##_alloced) { \
      size_t old_size = base_name##_alloced * sizeof(type); \
      base_name##_alloced *= 2; \
      size_t new_size = base_name##_alloced * sizeof(type); \
      if (base_name == base_name##_stack) \
        base_name = memcpy(malloc(new_size), base_name, old_size); \
      else \
        base_name = realloc(base_name, new_size); \
    } \
    base_name[n_##base_name++] = (to_append); \
  }while(0)

#define MAYBE_CLEAR_STACK_STARTED_ARRAY(base_name) \
  do { \
    if (base_name != base_name##_stack) \
      free(base_name); \
  }while(0)


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
  JAST_TOKEN_NUMBER,
  JAST_TOKEN_REGEX,

  JAST_TOKEN_TEMPLATE_HEAD,
  JAST_TOKEN_TEMPLATE_MIDDLE,
  JAST_TOKEN_TEMPLATE_TAIL,

  JAST_TOKEN_LPAREN,
  JAST_TOKEN_RPAREN,
  JAST_TOKEN_LBRACE,
  JAST_TOKEN_RBRACE,
  JAST_TOKEN_LBRACKET,
  JAST_TOKEN_RBRACKET,
  JAST_TOKEN_COMMA,

  JAST_TOKEN_SEMICOLON,

  JAST_TOKEN_ASSIGN,
  JAST_TOKEN_PLUS,
  JAST_TOKEN_MINUS,
  JAST_TOKEN_MULTIPLY,
  JAST_TOKEN_DIVIDE,
  JAST_TOKEN_MOD,
  JAST_TOKEN_SHIFT_LEFT,
  JAST_TOKEN_SHIFT_RIGHT,
  JAST_TOKEN_SHIFT_UNSIGNED_RIGHT,
  JAST_TOKEN_PLUS_ASSIGN,
  JAST_TOKEN_MINUS_ASSIGN,
  JAST_TOKEN_MULTIPLY_ASSIGN,
  JAST_TOKEN_DIVIDE_ASSIGN,
  JAST_TOKEN_MOD_ASSIGN,
  JAST_TOKEN_SHIFT_LEFT_ASSIGN,
  JAST_TOKEN_SHIFT_RIGHT_ASSIGN,
  JAST_TOKEN_SHIFT_UNSIGNED_RIGHT_ASSIGN,
  JAST_TOKEN_LOGICAL_NOT,
  JAST_TOKEN_BITWISE_NOT,
  JAST_TOKEN_BITWISE_AND,
  JAST_TOKEN_BITWISE_OR,
  JAST_TOKEN_BITWISE_XOR,
  JAST_TOKEN_LOGICAL_AND,
  JAST_TOKEN_LOGICAL_OR,
  JAST_TOKEN_COLON,
  JAST_TOKEN_QUESTION_MARK,
  JAST_TOKEN_DOT,
  JAST_TOKEN_BITWISE_AND_ASSIGN,
  JAST_TOKEN_BITWISE_OR_ASSIGN,
  JAST_TOKEN_BITWISE_XOR_ASSIGN,

  JAST_TOKEN_CMP_EXACT_EQ,
  JAST_TOKEN_CMP_NUM_EQ,
  JAST_TOKEN_CMP_EXACT_NE,
  JAST_TOKEN_CMP_NUM_NE,
  JAST_TOKEN_CMP_LT,
  JAST_TOKEN_CMP_LE,
  JAST_TOKEN_CMP_GT,
  JAST_TOKEN_CMP_GE,
} JAST_TokenType;

static const char *jast_token_type_name (JAST_TokenType type)
{
  switch (type)
  {
  case JAST_TOKEN_IF: return "if";
  case JAST_TOKEN_FOR: return "for";
  case JAST_TOKEN_IN: return "in";
  case JAST_TOKEN_DO: return "do";
  case JAST_TOKEN_WHILE: return "while";
  case JAST_TOKEN_WITH: return "with";
  case JAST_TOKEN_SWITCH: return "switch";
  case JAST_TOKEN_CASE: return "case";
  case JAST_TOKEN_DEFAULT: return "default";
  case JAST_TOKEN_VAR: return "var";
  case JAST_TOKEN_CONST: return "const";
  case JAST_TOKEN_LET: return "let";
  case JAST_TOKEN_NULL: return "null";
  case JAST_TOKEN_TRUE: return "true";
  case JAST_TOKEN_FALSE: return "false";
  case JAST_TOKEN_UNDEFINED: return "undefined";
  case JAST_TOKEN_BAREWORD: return "bareword";
  case JAST_TOKEN_STRING: return "string";
  case JAST_TOKEN_TEMPLATE_STRING: return "template_string";
  case JAST_TOKEN_NUMBER: return "number";
  case JAST_TOKEN_REGEX: return "regex";

  case JAST_TOKEN_LPAREN: return "(";
  case JAST_TOKEN_RPAREN: return ")";
  case JAST_TOKEN_LBRACE: return "{";
  case JAST_TOKEN_RBRACE: return "}";
  case JAST_TOKEN_LBRACKET: return "[";
  case JAST_TOKEN_RBRACKET: return "]";
  case JAST_TOKEN_COMMA: return ",";

  case JAST_TOKEN_SEMICOLON: return ";";

  case JAST_TOKEN_PLUS: return "+";
  case JAST_TOKEN_MINUS: return "-";
  case JAST_TOKEN_MULTIPLY: return "*";
  case JAST_TOKEN_DIVIDE: return "/";
  case JAST_TOKEN_MOD: return "%";
  case JAST_TOKEN_SHIFT_LEFT: return "<<";
  case JAST_TOKEN_SHIFT_RIGHT: return ">>";
  case JAST_TOKEN_SHIFT_UNSIGNED_RIGHT: return ">>>";
  case JAST_TOKEN_PLUS_ASSIGN: return "+=";
  case JAST_TOKEN_MINUS_ASSIGN: return "-=";
  case JAST_TOKEN_MULTIPLY_ASSIGN: return "*=";
  case JAST_TOKEN_DIVIDE_ASSIGN: return "/=";
  case JAST_TOKEN_MOD_ASSIGN: return "%=";
  case JAST_TOKEN_SHIFT_LEFT_ASSIGN: return "<<=";
  case JAST_TOKEN_SHIFT_RIGHT_ASSIGN: return ">>=";
  case JAST_TOKEN_SHIFT_UNSIGNED_RIGHT_ASSIGN: return ">>>=";
  case JAST_TOKEN_LOGICAL_NOT: return "!";
  case JAST_TOKEN_BITWISE_NOT: return "~";
  case JAST_TOKEN_BITWISE_AND: return "&";
  case JAST_TOKEN_BITWISE_OR: return "|";
  case JAST_TOKEN_BITWISE_XOR: return "^";
  case JAST_TOKEN_LOGICAL_AND: return "&&";
  case JAST_TOKEN_LOGICAL_OR: return "||";
  case JAST_TOKEN_BITWISE_AND_ASSIGN: return "&=";
  case JAST_TOKEN_BITWISE_OR_ASSIGN: return "|=";
  case JAST_TOKEN_BITWISE_XOR_ASSIGN: return "^=";
  case JAST_TOKEN_COLON: return ":";
  case JAST_TOKEN_QUESTION_MARK: return "?";
  case JAST_TOKEN_DOT: return ".";
  default: return "*unknwon token*";
  }
}

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
  size_t token_index;

  JAST_TokenType raw_last_token_type;

  JAST_Token cur, next, next_next;
  unsigned has_cur : 1, has_next : 1, has_next_next : 1;

  unsigned support_legacy_octal : 1;


  JAST_ParseError *error;
} JAST_Lexer;

static void
set_parse_error (JAST_Lexer *lexer,
                 JAST_ParseError_Type type,
                 JAST_Position *pos,
                 const char *format,
                 ...)
{
  char *err;
  va_list args;
  va_start (args, format);
  vasprintf (&err, format, args);
  va_end (args);
  assert(lexer->error == NULL);
  lexer->error = malloc (sizeof (JAST_ParseError));
  lexer->error->type = type;
  lexer->error->position = *pos;
  lexer->error->message = err;
}

static void
set_premature_eof_parse_error (JAST_Lexer *lexer, const char *situation)
{
  set_parse_error (
    lexer,
    JAST_PARSE_ERROR_PREMATURE_EOF,
    &lexer->cur.position,
    "premature eof in %s",
    situation
  );
}
static void
set_expected_token_parse_error (JAST_Lexer *lexer,
                                JAST_TokenType token_type,
                                const char *situation)
{
  set_parse_error (
    lexer,
    JAST_PARSE_ERROR_BAD_TOKEN,
    &lexer->cur.position,
    "expected %s, in %s",
    jast_token_type_name(token_type),
    situation
  );
}
static void
set_bad_character_parse_error(JAST_Lexer *lexer,
                              const char *str)
{
  set_parse_error (
    lexer,
    JAST_PARSE_ERROR_BAD_CHARACTER,
    &lexer->position,
    "unexpected character '%c'",
    *str
  );
}

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
  unsigned len = jutf8_scan_idstart_char (at);
  if (len == 0)
    {
      set_bad_character_parse_error (lexer, at);
      return JAST_LEXER_ADVANCE_ERROR;
    }
  at += len;
  while (at < end && (len=jutf8_scan_idcontinue_char(at)) != 0)
    {
      /* just keep waiting for a non-bareword character. */
      at += len;
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
          if (at + 1 == end)
            goto premature_eof;
          switch (at[1])
            {
            case '\n':          /* line terminator sequence */
              at += 2;
              lexer->position.line_number += 1;
              break;

            /* "single escape characters" */
            case '\'': case '"': case '\\': 
            case 'b': case 'f': case 'n': case 'r': case 't': case 'v':
              at += 2;
              continue;

            case 'u':
              /* "unicode escape sequence" */
              /* either uXXXX or u{X+} */
              if (at + 2 == end)
                goto premature_eof;
              if (at[2] == LBRACE_CHAR)
                {
                  at += 2;
                  while (at < end && *at != RBRACE_CHAR)
                    {
                      if (!IS_HEXIDECIMAL_DIGIT (*at))
                        goto bad_char;
                      at++;
                    }
                  if (at == end)
                    goto premature_eof;
                  at += 1;              /* skip right-brace */
                }
              else
                {
                  if (at + 6 > end)
                    goto premature_eof;
                  if (!IS_HEXIDECIMAL_DIGIT(at[2])
                   || !IS_HEXIDECIMAL_DIGIT(at[3])
                   || !IS_HEXIDECIMAL_DIGIT(at[4])
                   || !IS_HEXIDECIMAL_DIGIT(at[5]))
                    goto bad_char;
                  at += 6;
                  continue;
                }
              break;

            case 'x':
              /* xXX */
              if (end < at + 4)
                goto premature_eof;
              if (!IS_HEXIDECIMAL_DIGIT(at[2]) || !IS_HEXIDECIMAL_DIGIT(at[3]))
                goto bad_char;
              at += 4;
              break;

            case '0':
              /* special case for \0 */
              if (end == at + 2 || !IS_DECIMAL_DIGIT(at[2]))
                {
                  at += 2;
                  continue;
                }
              // otherwise fall-through to legacy octal handling
              
            case '1': case '2': case '3':
            case '4': case '5': case '6': case '7':
              if (lexer->support_legacy_octal)
                {
                  if (end < at + 4)
                    goto premature_eof;
                  if (!IS_OCTAL_DIGIT (at[2]) || !IS_OCTAL_DIGIT (at[3]))
                    goto bad_char;
                  at += 4;
                  continue;
                }
              else
                goto bad_char;

            /* Finally, non-escape characters */
            default:
              at += 2;
              continue;
            }
        }
      else if (*at == '\n')
        {
          /* newlines not allowed in quoted-strings */
          goto bad_char;
        }
      else
        {
          at++;
        }
    }
  if (at == end)
    {
      set_premature_eof_parse_error(lexer, "in quoted-string");
      return JAST_LEXER_ADVANCE_ERROR;
    }
  token->type = JAST_TOKEN_STRING;
  token->length = at + 1 - q_start;
  lexer->offset = at + 1 - lexer->data;
  return JAST_LEXER_ADVANCE_OK;

bad_char:
  set_bad_character_parse_error (lexer, at);
  return JAST_LEXER_ADVANCE_ERROR;

premature_eof:
  set_premature_eof_parse_error (lexer, "in quoted-string");
  return JAST_LEXER_ADVANCE_ERROR;
}

static JAST_Lexer_AdvanceResult
lexer_get_next_token_raw_number (JAST_Lexer *lexer, JAST_Token *token)
{
  /* See Spec 11.8.3 */
  const char *end = lexer->data + lexer->data_size;
  const char *n_start = lexer->data + lexer->offset;
  const char *at = n_start;
#define SKIP_WITH_PREDICATE(pred)  \
  while (at < end && (pred (*at))) \
    at++
  if (*at == '0')
    {
      if (at + 1 == end)
        {
          at++;
          goto success;
        }
      if (at[1] == 'o' || at[1] == 'O')
        {
          /* octal integer */
          if (at + 2 == end)
            goto premature_eof;
          at += 2;
          if (!IS_OCTAL_DIGIT (*at))
            goto bad_char;
          at++;
          SKIP_WITH_PREDICATE(IS_OCTAL_DIGIT);
          if (at < end && jutf8_is_post_number_character (at))
            goto bad_char;
          goto success;
        }
      else if (at[1] == 'b' || at[1] == 'B')
        {
          if (at + 2 == end)
            goto premature_eof;
          at += 2;
          if (!IS_BINARY_DIGIT (*at))
            goto bad_char;
          at++;
          SKIP_WITH_PREDICATE(IS_BINARY_DIGIT);
        }
      else if (at[1] == 'x' || at[1] == 'x')
        {
          if (at + 2 == end)
            goto premature_eof;
          if (at + 2 == end)
            goto premature_eof;
          at += 2;
          if (!IS_HEXIDECIMAL_DIGIT (*at))
            goto bad_char;
          at++;
          SKIP_WITH_PREDICATE(IS_HEXIDECIMAL_DIGIT);
        }
      else
        goto decimal_number;
    }
  else if (*at == '.' || ('1' <= *at && *at <= '9'))
    {
      goto decimal_number;
    }
  else
    {
      goto bad_char;
    }

decimal_number:
  SKIP_WITH_PREDICATE (IS_DECIMAL_DIGIT);
  if (at < end && *at == '.')
    {
      at++;
      SKIP_WITH_PREDICATE (IS_DECIMAL_DIGIT);
    }
  if (at < end && (*at == 'e' || *at == 'E'))
    {
      at++;
      if (at < end && (*at == '+' || *at == '-'))
        at++;
      if (at == end)
        goto premature_eof;
      if (!IS_DECIMAL_DIGIT(*at))
        goto bad_char;
      SKIP_WITH_PREDICATE (IS_DECIMAL_DIGIT);
    }
  if (n_start == at)
    goto bad_char;
  goto success;

success:
  token->type = JAST_TOKEN_NUMBER;
  token->length = at - n_start;
  lexer->offset = at - lexer->data;
  return JAST_LEXER_ADVANCE_OK;

bad_char:
  set_bad_character_parse_error (lexer, at);
  return JAST_LEXER_ADVANCE_ERROR;

premature_eof:
  set_premature_eof_parse_error (lexer, "number");
  return JAST_LEXER_ADVANCE_ERROR;

#undef SKIP_WITH_PREDICATE
}

static JAST_Lexer_AdvanceResult
lexer_get_next_token_raw_regex (JAST_Lexer *lexer, JAST_Token *token)
{
  /* See Spec 11.8.5 */
  const char *end = lexer->data + lexer->data_size;
  const char *regex_start = lexer->data + lexer->offset;
  const char *at = regex_start;

  assert (*at == '/');
  if (at + 3 > end)
    goto premature_eof;

  at++;     /* skip initial slash */

  /* Handle lexing rule that
     '*' is not allowed by RegularExpressionFirstChar
     but is allowed by RegularExpressionChar
     (which is handled by the loop below). */
  if (*at == '*')
    goto bad_char;

  while (at < end && *at != '/')
    {
      switch (*at)
        {
          case '\\':
            if (at + 1 == end)
              goto premature_eof;
            if (at[1] == '\n')
              goto bad_char;
            at += 2;
            continue;
          case '[':
            at++;
            while (at < end && *at != ']' && *at != '\n')
              {
                if (*at == '\\')
                  {
                    at++;
                    if (at == end)
                      goto premature_eof;
                    if (*at == '\n')
                      goto bad_char;
                    at++;
                  }
                else
                  at++;
              }
            if (at == end)
              goto premature_eof;
            if (*at == '\n')
              goto bad_char;
            at++;
            break;
          case '\n':
            goto bad_char;  /* newline not allowed in regex */
          default:
            at++;       /* RENonTerminator but not one of / \ or [ */
            break;
        }
    }
  if (at == end)
    goto premature_eof;
  assert (*at == '/');
  at++;

  /* skip flags (like "i" for case-insensitive) see spec . */
  unsigned len;
  while (at < end && (len=jutf8_scan_idcontinue_char (at)) != 0)
    at += len;
  token->length = at - regex_start;
  token->type = JAST_TOKEN_REGEX;
  lexer->offset = at - lexer->data;
  return JAST_LEXER_ADVANCE_OK;

premature_eof:
  set_premature_eof_parse_error (lexer, "regex");
  return JAST_LEXER_ADVANCE_ERROR;

bad_char:
  set_bad_character_parse_error (lexer, at);
  return JAST_LEXER_ADVANCE_ERROR;
}

/* Spec 11.8.6 */
static JAST_Lexer_AdvanceResult
lexer_get_next_token_raw_template_head (JAST_Lexer *lexer, JAST_Token *token)
{
  const char *t_start = lexer->data + lexer->offset;
  const char *at = t_start + 1;
  const char *end = lexer->data + lexer->data_size;
  while (at < end && *at != '`')
    {
      if (*at == '\\')
        {
          at++;
          if (at == end)
            {
              set_premature_eof_parse_error(lexer,"after \\ in template");
              return JAST_LEXER_ADVANCE_ERROR;
            }
          at++;
        }
      else if (at + 1 < end && at[0] == '$' && at[1] == LBRACE_CHAR)
        {
          at += 2;
          token->length = at - t_start;
          lexer->offset = at - lexer->data;
          token->type = JAST_TOKEN_TEMPLATE_HEAD;
          lexer->in_template_brace_depth = 1;
          return JAST_LEXER_ADVANCE_OK;
        }
      else
        {
          at++;
        }
    }
  if (at >= end)
    {
      set_premature_eof_parse_error(lexer, "in template string");
      return JAST_LEXER_ADVANCE_ERROR;
    }
  lexer->offset = at - lexer->data;
  return JAST_LEXER_ADVANCE_OK;
}

static JAST_Lexer_AdvanceResult
lexer_get_next_token_raw_template_tail (JAST_Lexer *lexer, JAST_Token *token)
{
  ...
}

static JS_Boolean
last_token_permits_regex_literal (JAST_TokenType type)
{
  switch (type)
  {
  case JAST_TOKEN_ASSIGN:
  case JAST_TOKEN_COMMA:
  case JAST_TOKEN_SEMICOLON:
  case JAST_TOKEN_LPAREN:
  case JAST_TOKEN_COLON:
  case JAST_TOKEN_LBRACKET:
  case JAST_TOKEN_LBRACE:
    return JS_TRUE;
  default:
    return JS_FALSE;
  }
}
static JS_Boolean
could_be_regex (const char *str, const char *end)
{
  if (str + 3 > end)
    return JS_FALSE;
  return JS_TRUE;
}

/* Ignores cur/next/next_next and just lexes a token from the buffer. */
static JAST_Lexer_AdvanceResult
_lexer_get_next_token_raw (JAST_Lexer *lexer, JAST_Token *token)
{
  /* Skip whitespace */
  const char *at = lexer->data + lexer->offset;
  const char *end = lexer->data + lexer->data_size;
  JS_Boolean on_new_line = lexer->token_index == 0;

restart:
  while (at < end)
    {
      if (*at == ' ')
        at++;
      else if (*at == '\t')
        at++;
      else if (*at == '\n')
        {
          lexer->position.line_number += 1;
          on_new_line = JS_TRUE;
        }
      else
        break;
    }
  //token->on_new_line = on_new_line;
  if (at == end)
    return JAST_LEXER_ADVANCE_EOF;
  token->token_index = lexer->token_index;
  token->position = lexer->position;
  token->offset = at - lexer->data;

#define IMPLEMENT_SINGLE_CHARACTER_TOKEN(token_shortname) \
    do{ token->type = JAST_TOKEN_##token_shortname; \
        token->length = 1; \
        at++; }while(0)
#define IMPLEMENT_TWO_CHARACTER_TOKEN(token_shortname) \
    do{ token->type = JAST_TOKEN_##token_shortname; \
        token->length = 2; \
        at+=2; }while(0)
#define IMPLEMENT_THREE_CHARACTER_TOKEN(token_shortname) \
    do{ token->type = JAST_TOKEN_##token_shortname; \
        token->length = 3; \
        at+=3; }while(0)
#define IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(token_shortname) \
    do{ if (at + 1 < end && at[1] == '=') \
          IMPLEMENT_TWO_CHARACTER_TOKEN(token_shortname##_ASSIGN); \
        else \
          IMPLEMENT_SINGLE_CHARACTER_TOKEN(token_shortname);       \
    }while(0)


  switch (*at)
    {
      case '!':
        if (at + 1 < end && at[1] == '=')
          {
            if (at + 2 < end && at[2] == '=')
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
        if (at + 1 < end && at[1] == '&')
          IMPLEMENT_TWO_CHARACTER_TOKEN(LOGICAL_AND);
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(BITWISE_AND);
        break;

      case '|':
        if (at + 1 < end && at[1] == '|')
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
        if (lexer->in_template_brace_depth > 0)
          lexer->in_template_brace_depth++;
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(LBRACE);
        break;
      case '}':
        if (lexer->in_template_brace_depth == 1)
          {
            return lexer_get_next_token_raw_template_tail(lexer, token);
          }
        else
          {
            if (lexer->in_template_brace_depth > 0)
              lexer->in_template_brace_depth++;
            IMPLEMENT_SINGLE_CHARACTER_TOKEN(RBRACE);
          }
        break;
      case ',':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(COMMA);
        break;
      case '.':
        if (at + 1 < end && '0' <= at[1] && at[1] <= '9')
          {
            return lexer_get_next_token_raw_number(lexer, token);
          }
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN(DOT);
        break;

      case ';':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN(SEMICOLON);
        break;

      case '/':
        if (at + 1 < end && at[1] == '/')
          {
            at += 2;
            while (at < end && *at == '\n')
              at++;
            lexer->position.line_number += 1;
          }
        else if (at + 1 < end && at[1] == '*')
          {
            /* multi-line comment */
            at += 1;
            while (at + 1 < end)
              {
                if (at[0] == '*' && at[1] == '/')
                  {
                    at += 2;
                    lexer->offset = at - lexer->data;
                    goto restart;
                  }
                else if (at[0] == '\n')
                  lexer->position.line_number += 1;
                at++;
              }
            set_premature_eof_parse_error (lexer, "comment");
            return JAST_LEXER_ADVANCE_ERROR;
          }
        else if (last_token_permits_regex_literal (lexer->raw_last_token_type)
          && could_be_regex (at, end))
          {
            return lexer_get_next_token_raw_regex (lexer, token);
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

      case '`':
        return lexer_get_next_token_raw_template_head(lexer, token);
        

      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
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
        return lexer_get_next_token_raw_bareword(lexer, token);
    }

}

static inline JAST_Lexer_AdvanceResult
lexer_get_next_token_raw (JAST_Lexer *lexer, JAST_Token *token)
{
  JAST_Lexer_AdvanceResult rv;
  rv = _lexer_get_next_token_raw(lexer, token);
  if (rv == JAST_LEXER_ADVANCE_OK)
    lexer->raw_last_token_type = token->type;
  return rv;
}

static JS_Boolean
jast_lexer_init (JAST_Lexer       *lexer,
                 size_t            data_size,
                 const char       *data,
                 const char       *filename,
                 JAST_ParseError **error)
{
  lexer->data_size = data_size;
  lexer->data = data;
  lexer->byte_offset = 0;
  lexer->token_index = 0;
  lexer->position.filename = JAST_filename_make(filename);
  lexer->position.line_number = 1;
  lexer->support_legacy_octal = 0;
  switch (lexer_get_next_token_raw (lexer, &lexer->cur))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        lexer->has_cur = lexer->has_next = lexer->has_next_next = JS_FALSE;
        return JS_TRUE;
      case JAST_LEXER_ADVANCE_ERROR:
        JAST_filename_unref(lexer->position.filename);
        return JS_FALSE;
    }
  lexer->has_cur = JS_TRUE;
  switch (lexer_get_next_token_raw (lexer, &lexer->next))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        lexer->has_next = lexer->has_next_next = JS_FALSE;
        return JS_TRUE;
      case JAST_LEXER_ADVANCE_ERROR:
        JAST_filename_unref(lexer->position.filename);
        return JS_FALSE;
    }
  lexer->has_next = JS_TRUE;
  switch (lexer_get_next_token_raw (lexer, &lexer->next_next))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        lexer->has_next_next = JS_FALSE;
        return JS_TRUE;
      case JAST_LEXER_ADVANCE_ERROR:
        JAST_filename_unref(lexer->position.filename);
        return JS_FALSE;
    }
  lexer->has_next_next = JS_TRUE;
  return JS_TRUE;
}

static JAST_Lexer_AdvanceResult jast_lexer_advance (JAST_Lexer *lexer)
{
  if (!lexer->has_cur)
    {
      lexer->error = premature_eof_parse_error ("advance past EOF");
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
          lexer->has_next_next = JS_FALSE;
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
  void (*destroy)(void *data);
};
  

struct MatcherPiece {
  MatcherType matcher_type;
  union {
    JAST_TokenType by_token_type;
    struct MatcherVirtual *matcher_virtual;
  } info;
};

static JS_Boolean
match_pieces (JAST_Lexer *lexer,
              size_t      n_pieces,
              struct MatcherPiece *pieces,
              void      **virtuals_out)
{
  size_t i;
  for (i = 0; i < n_pieces; i++)
    {
      switch (pieces[i].matcher_type)
        {
          case MATCHER_TYPE_BY_TOKEN_TYPE:
            if (!lexer.has_cur)
              {
                lexer->error = premature_eof_parse_error ("matching token");
                goto handle_error;
              }

            JAST_TokenType actual_tt = lexer.cur.type;
            JAST_TokenType expected_tt = pieces[i].info.by_token_type;
            if (actual_tt != expected_tt)
              {
                lexer->error = token_mismatch_parse_error (&lexer.cur, expected_tt);
                goto handle_error;
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
  return JS_TRUE;

handle_error:
  unsigned vi = 0;
  for (size_t j = 0; j < i; j++)
    if (pieces[i].matcher_type == MATCHER_TYPE_VIRTUAL)
      {
        void *v = virtuals_out[vi++];
        struct MatcherVirtual *mv = pieces[i].info.matcher_virtual;
        mv->destroy (v);
      }
  return JS_FALSE;
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
  DEFINE_STACK_STARTED_ARRAY(JAST_ConditionalStatement_Clause, cstmts, 8);

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
          JAST_ConditionalStatement_Clause clause = {
            .expr = pad[0],
            .statement = pad[1]
          };
          APPEND_TO_STACK_STARTED_ARRAY(JAST_ConditionalStatement_Clause, cstmts, clause);
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
  MAYBE_CLEAR_STACK_STARTED_ARRAY(cstmts);
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

/* From 13.6.0.1.  
for ( [lookahead ∉ {let '['}] Expressionopt ; Expressionopt ; Expressionopt ) Statement
for ( var VariableDeclarationList ; Expressionopt ; Expressionopt ) Statement
for ( LexicalDeclaration Expressionopt ; Expressionopt ) Statement
for ( [lookahead ∉ {let '['}] LeftHandSideExpression in Expression ) Statement
for ( var ForBinding in Expression ) Statement
for ( ForDeclaration in Expression ) Statement
for ( [lookahead ≠ let ] LeftHandSideExpression of AssignmentExpression ) Statement
for ( var ForBinding of AssignmentExpression ) Statement
for ( ForDeclaration of AssignmentExpression[In, ?Yield] ) Statement

ForDeclaration :== LetOrConst ForBinding
ForBinding :== BindingIdentifier | BindingPattern
VariableDeclarationList :== VariableDeclaration | VariableDeclarationList ',' VariableDeclaration
VariableDeclaration :== BindingIdentifier Initializeropt | BindingPattern Initializer
LexicalDeclaration :== LetOrConst BindingList ';'
LetOrConst :== let | const
 */
typedef enum
{
  VARTYPE_NONE,
  VARTYPE_LET,
  VARTYPE_CONST,
  VARTYPE_VAR,
} Vartype;

static JAST_Statement *
parse_either_for_statement (JAST_Lexer *lexer)
{
  DEFINE_STACK_STARTED_ARRAY(JAST_VariableDeclaration, vardecls, 8);
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK: break;
    case JAST_LEXER_ADVANCE_EOF:
      lexer->error = premature_eof_parse_error ("after for");
      return NULL;
    case JAST_LEXER_ADVANCE_ERROR:
      return NULL;
    }
  if (lexer->cur.type != JAST_TOKEN_LPAREN)
    { 
      lexer->error = token_mismatch_parse_error (&lexer.cur, JAST_TOKEN_LPAREN);
      return NULL;
    }
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK: break;
    case JAST_LEXER_ADVANCE_EOF:
      lexer->error = premature_eof_parse_error ("after for");
      return NULL;
    case JAST_LEXER_ADVANCE_ERROR:
      return NULL;
    }

  JAST_VariableDeclarationType vartype;
  if (lexer->cur.type == JAST_TOKEN_VAR
   || lexer->cur.type == JAST_TOKEN_LET
   || lexer->cur.type == JAST_TOKEN_CONST)
    {
      vartype = (lexer->cur.type == JAST_TOKEN_VAR) ? JAST_VARIABLE_DECLARATION_VAR
              : (lexer->cur.type == JAST_TOKEN_LET) ? JAST_VARIABLE_DECLARATION_LET
              : JAST_VARIABLE_DECLARATION_CONST;
      switch (jast_lexer_advance(lexer))
        {
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_EOF:
          lexer->error = premature_eof_parse_error("after var/let/const");
          return NULL;
        case JAST_LEXER_ADVANCE_ERROR:
          return NULL;
        }
    }
  else
    {
      vartype = JAST_VARIABLE_DECLARATION_NONE;
    }

  JAST_BindingPattern bp;
  if (!parse_binding_pattern (lexer, &bp))
    return NULL;

  JS_Boolean is_for_in;
  JAST_Expr *container = NULL;
  switch (lexer->cur.type)
    {
    case JAST_TOKEN_OF:
      is_for_in = JS_FALSE;
      goto for_in_loop;
    case JAST_TOKEN_IN:
      is_for_in = JS_TRUE;
      goto for_in_loop;

    case JAST_TOKEN_EQUALS:
      switch (jast_lexer_advance (lexer))
        {
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_EOF:
          lexer->error = premature_eof_parse_error ("after '='");
          return NULL;
        case JAST_LEXER_ADVANCE_ERROR:
          return NULL;
        }
      initializer = parse_expr (lexer);
      if (initializer == NULL)
        {
          jast_binding_pattern_clear (&bp);
          return NULL;
        }

      if (lexer->cur.type == JAST_TOKEN_SEMICOLON
       || lexer->cur.type == JAST_TOKEN_COMMA)
        goto cstyle_for_loop;
      else
        {
          lexer->error = unexpected_token_parse_error(&lexer->cur, "after parsing variable binding");
          return NULL;
        }

    case JAST_TOKEN_SEMICOLON:
    case JAST_TOKEN_COMMA:
      goto cstyle_for_loop;

    default:
      lexer->error = unexpected_token_parse_error(&lexer->cur, "after variable declaration");
      jast_binding_pattern_clear(&bp);
      return 
    }


for_in_loop:
/* skip in/of token */
  switch (jast_lexer_advance(lexer))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        jast_binding_pattern_clear(&bp);
        lexer->error = premature_eof_parse_error("in for-of");
        return NULL;
      case JAST_LEXER_ADVANCE_ERROR:
        jast_binding_pattern_clear(&bp);
        return NULL;
    }
  JAST_Expr *container = parse_expr (lexer);
  if (container == NULL)
    {
      jast_binding_pattern_clear (&bp);
      return NULL;
    }
  if (!lexer->has_cur)
    {
      lexer->error = parse_error("unexpected EOF after container in for-loop");
      jast_binding_pattern_clear (&bp);
      jast_expr_free (container);
      return NULL;
    }
  if (lexer->cur.token != JAST_TOKEN_RPAREN)
    {
      lexer->error = unexpected_token_parse_error(&lexer->cur, "after variable declaration");
      jast_binding_pattern_clear (&bp);
      jast_expr_free (container);
      return NULL;
    }
  switch (jast_lexer_advance (lexer))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        lexer->error = premature_eof_parse_error("for-in body");
        jast_binding_pattern_clear(&bp);
        jast_expr_unref(container);
        return NULL;
      case JAST_LEXER_ADVANCE_ERROR:
        jast_binding_pattern_clear(&bp);
        jast_expr_unref(container);
        return NULL;
    }
  JAST_Statement *body = parse_stmt (lexer);
  if (body == NULL)
    {
      jast_binding_pattern_clear(&bp);
      jast_expr_unref(container);
      return NULL;
    }
  JAST_Statement_ForIn *loop = alloc_statement(JAST_STATEMENT_FOR_IN, sizeof (JAST_Statement_ForIn));
  loop->binding = bp;
  loop->is_for_in = is_for_in;
  loop->container = container;
  loop->body = body;
  return loop;


cstyle_for_loop:
  n_vardecls = 1;
  vardecls[0].binding = bp;
  vardecls[0].initializer = initializer;
  while (lexer->cur.type == JAST_TOKEN_COMMA)
    {
      switch (jast_lexer_advance (lexer))
        {
          case JAST_LEXER_ADVANCE_OK:
            break;
          case JAST_LEXER_ADVANCE_EOF:
            lexer->error = premature_eof_parse_error("for variable declarations");
            goto error_in_vardecls;
          case JAST_LEXER_ADVANCE_ERROR:
            goto error_in_vardecls;
        }

      // parse binding pattern
      if (!parse_binding_pattern(lexer, &bp))
        goto error_in_vardecls;

      // parse optional initializer
      if (!lexer->has_cur)
        {
          lexer->error = premature_eof_parse_error("for variable declarations");
          goto error_in_vardecls;
        }
      if (lexer->cur.type == JAST_TOKEN_ASSIGN)
        {
          switch (jast_lexer_advance (lexer))
          {
          case JAST_LEXER_ADVANCE_OK:
            break;
          case JAST_LEXER_ADVANCE_EOF:
            lexer->error = premature_eof_parse_error("after '=' in for variable declarations");
            jast_binding_pattern_clear (&bp);
            goto error_in_vardecls;
          case JAST_LEXER_ADVANCE_ERROR:
            jast_binding_pattern_clear (&bp);
            goto error_in_vardecls;
          }
          initializer = parse_expr (lexer);
          if (initializer == NULL)
            {
              jast_binding_pattern_clear (&bp);
              goto error_in_vardecls;
            }
        }
      else
        initializer = NULL;

      JAST_VariableDeclaration vd;
      vd.binding = bp;
      vd.initializer = initializer;
      APPEND_TO_STACK_STARTED_ARRAY(JAST_VariableDeclaration, vardecls, vd);
    }
  if (lexer->cur.type != JAST_TOKEN_SEMICOLON)
    {
      lexer->error = token_mismatch_parse_error (&lexer.cur, JAST_TOKEN_SEMICOLON);
      return NULL;
    }
  size_t init_size = sizeof (JAST_Statement_VariableDeclarations)
                   + sizeof (JAST_VariableDeclaration) * n_vardecls;
  JAST_Statement_VariableDeclarations *initial = alloc_statement(JAST_STATEMENT_VARIABLE_DECLARATION, init_size);
  initial->type = vartype;
  initial->n_vars = n_vardecls;
  initial->vars = memcpy (initial + 1, vardecls, sizeof (JAST_VariableDeclaration) * n_vardecls);
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_EOF:
      lexer->error = premature_eof_parse_error("for conditional");
      goto error_in_conditional;
    case JAST_LEXER_ADVANCE_ERROR:
      goto error_in_conditional;
    }
  if (lexer->cur.type == JAST_TOKEN_SEMICOLON)
    {
      /* empty conditional */
      conditional = NULL;
    }
  else
    {
      conditional = parse_expr (lexer);
      if (conditional == NULL)
        goto error_in_conditional;
    }

  // update statement
  assert(lexer->cur.type == JAST_TOKEN_SEMICOLON);
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_EOF:
      lexer->error = premature_eof_parse_error("after second ; in for-loop");
      goto error_in_update;
    case JAST_LEXER_ADVANCE_ERROR:
      goto error_in_update;
    }

  if (lexer->cur.type == JAST_TOKEN_RPAREN)
    {
      update = NULL;
    }
  else
    {
      update = parse_stmt (lexer);
      if (update == NULL)
        goto error_in_update;
      if (!lexer->has_cur)
        {
          lexer->error = premature_eof_parse_error("expecting for-loop body");
          goto error_in_body;
        }
      if (lexer->cur.type != JAST_TOKEN_RPAREN)
        {
          lexer->error = token_mismatch_parse_error (&lexer.cur, JAST_TOKEN_RPAREN);
          goto error_in_body;
        }
    }

  assert(lexer->cur.type == JAST_TOKEN_RPAREN);
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_EOF:
      lexer->error = premature_eof_parse_error("expected for-loop body");
      goto error_in_body;
    case JAST_LEXER_ADVANCE_ERROR:
      goto error_in_body;
    }
  body = parse_stmt (lexer);
  if (body == NULL)
    goto error_in_body;

  JAST_Statement_For *rv = alloc_statement(JAST_STATEMENT_FOR, sizeof(JAST_Statement_For));
  rv->initial = (JAST_Statement *) initial;
  rv->condition = condition;
  rv->advance = advance;
  rv->body = body;
  return rv;

error_in_vardecls:
  for (size_t j = 0; j < n_vardecls; j++)
    {
      jast_binding_pattern_clear (&vardecls[j].binding);
      if (vardecls[j].initializer)
        jast_expr_free (vardecls[j].initializer);
    }
  return NULL;

error_in_body:
  if (update != NULL)
    jast_statement_free (update);

  /* fallthrough */

error_in_update:
  if (condition != NULL)
    jast_expr_free (condition);

  /* fallthrough */

error_in_conditional:
  if (initial != NULL)
    jast_statement_free ((JAST_Statement *) initial);
  return NULL;
}


static struct MatcherPiece with_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(WITH),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_statement),
};

static JAST_Statement *
parse_with_statement (JAST_Lexer *lexer)
{
  void pad[2];
  if (!DO_STATIC_MATCHES (lexer, with_pieces, pad))
    return NULL;
  JAST_Statement_With *rv = alloc_statement(JAST_STATEMENT_WITH, sizeof(JAST_Statement_With));
  rv->with_expr = pad[0];
  rv->body = pad[1];
  return (JAST_Statement *) rv;
}

static struct MatcherPiece switch_header_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(SWITCH),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_piece_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
  MATCHER_PIECE_BY_TOKEN_TYPE(LBRACE),
};

static JAST_Statement *
parse_switch_statement (JAST_Lexer *lexer)
{
  DEFINE_STACK_STARTED_ARRAY(JAST_Switch_Clause, clauses, 24);
  void *pad_expr[1];
  if (!DO_STATIC_MATCHES (lexer, switch_header_pieces, pad_expr))
    return NULL;
  while (lexer->has_cur && lexer->cur.type != JAST_TOKEN_RBRACE)
    {
      if (lexer->cur.type == JAST_TOKEN_CASE)
        {
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              lexer->error = premature_eof_parse_error("after 'case' in switch");
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
            }

          /* parse key */
          JAST_Expr *case_value = parse_expr(lexer);
          if (case_value == NULL)
             goto error_cleanup;

          /* skip colon */
          if (!lexer->has_cur)
            {
              lexer->error = premature_eof_parse_error("expected colon after case");
              goto error_cleanup;
            }
          if (lexer->cur.type != JAST_TOKEN_COLON)
            {
              lexer->error = unexpected_token_parse_error(&lexer->cur, "expected colon after case");
              goto error_cleanup;
            }

          JAST_Switch_Clause clause;
          clause.clause_type = JAST_SWITCH_CLAUSE_CASE;
          clause.info.case_value = case_value;
          APPEND_TO_STACK_STARTED_ARRAY(JAST_Switch_Clause, clauses, clause);
        }
      else if (lexer->cur.type == JAST_TOKEN_DEFAULT)
        {
          if (has_default)
            {
              lexer->error = parse_error("multiple default clauses in switch statement");
              goto error_cleanup;
            }
          has_default = JS_TRUE;
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              lexer->error = premature_eof_parse_error("after 'default' in switch");
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
            }
          if (lexer->cur.type != JAST_TOKEN_COLON)
            {
              lexer->error = unexpected_token_parse_error(&lexer->cur, "expected colon after default");
              goto error_cleanup;
            }
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              lexer->error = premature_eof_parse_error("in switch statement");
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
            }
          JAST_Switch_Clause clause;
          clause.clause_type = JAST_SWITCH_CLAUSE_DEFAULT;
          APPEND_TO_STACK_STARTED_ARRAY(JAST_Switch_Clause, clauses, clause);
        }
      else
        {
          /* parse arbitrary statement */
          JAST_Statement *sub = parse_stmt(lexer);
          if (sub == NULL)
            goto error_cleanup;
          JAST_Switch_Clause clause;
          clause.clause_type = JAST_SWITCH_CLAUSE_STATEMENT;
          clause.info.statement = sub;
          APPEND_TO_STACK_STARTED_ARRAY(JAST_Switch_Clause, clauses, clause);
        }
    }
  size_t clauses_size = sizeof(JAST_Switch_Clause) * n_clauses;
  size_t rv_size = sizeof(JAST_Statement_Switch) + clauses_size;
  JAST_Statement_Switch *rv = alloc_statement(JAST_STATEMENT_SWITCH, rv_size);
  rv->n_clauses = n_clauses;
  rv->clauses = (JAST_Switch_Clause *) (rv + 1);
  memcpy (rv->clauses, clauses, clauses_size);
  return (JAST_Statement *) rv;
}

static JAST_Statement *
parse_vardecl_statement (JAST_Lexer *lexer)
{
  JS_Boolean skip = JS_FALSE;
  JAST_VariableDeclarationType vardecl_type = JAST_VARIABLE_DECLARATION_NONE;
  if (lexer->cur.type == JAST_TOKEN_LET)
    {
      skip = JS_TRUE;
      vardecl_type = JAST_VARIABLE_DECLARATION_LET;
    }
  else if (lexer->cur.type == JAST_TOKEN_CONST)
    {
      skip = JS_TRUE;
      vardecl_type = JAST_VARIABLE_DECLARATION_CONST;
    }
  else if (lexer->cur.type == JAST_TOKEN_VAR)
    {
      skip = JS_TRUE;
      vardecl_type = JAST_VARIABLE_DECLARATION_VAR;
    }
  if (skip)
    {
      switch (jast_lexer_advance (lexer))
        {
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_EOF:
          lexer->error = premature_eof_parse_error ("unexpected EOF in var-decls");
          return NULL;
        case JAST_LEXER_ADVANCE_ERROR:
          return NULL;
        }
    }
  DEFINE_STACK_STARTED_ARRAY(JAST_VariableDeclaration, vardecls, 16);
  for (;;)
    {
      JAST_VariableDeclaration vd;
      if (!parse_binding_pattern (lexer, &vd.binding))
        goto error_cleanup;
      if (!lexer->has_cur)
        {
          lexer->error = premature_eof_parse_error("in variable declaration");
          goto error_cleanup;
        }
      if (lexer->cur.token == JAST_TOKEN_ASSIGN)
        {
          /* skip '=' */
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              lexer->error = premature_eof_parse_error("in variable initialization");
              jast_binding_pattern_clear(&vd.binding);
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              jast_binding_pattern_clear(&vd.binding);
              goto error_cleanup;
            }
          vd.initializer = parse_expr (lexer);
          if (vd.initializer == NULL)
            {
              jast_binding_pattern_clear(&vd.binding);
              goto error_cleanup;
            }
        }
      else
        vd.initializer = NULL;
      APPEND_TO_STACK_STARTED_ARRAY (JAST_VariableDeclaration, vardecls, vd);

      if (lexer->cur.type == JAST_TOKEN_SEMICOLON)
        {
          break;
        }
      else if (lexer->cur.type == JAST_TOKEN_COMMA)
        {
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              lexer->error = premature_eof_parse_error("after variable");
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
            }
        }
      else
        {
          lexer->error = unexpected_token_parse_error(&lexer->cur, "expected ';' or ',' after variable");
          goto error_cleanup;
        }
    }
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_ERROR:
      goto error_cleanup;
    case JAST_LEXER_ADVANCE_EOF:
      lexer->error = premature_eof_parse_error("after variable declaration statement");
      goto error_cleanup;
    }

  size_t vd_size = sizeof(JAST_VariableDeclaration) * n_vardecls;
  size_t rv_size = sizeof(JAST_Statement_VariableDeclarations) + vd_size;
  JAST_Statement_VariableDeclarations *rv = alloc_statement(JAST_STATEMENT_VARIABLE_DECLARATION, rv_size);
  rv->type = vardecl_type;
  rv->n_vars = n_vardecls;
  rv->vars = (JAST_VariableDeclaration*) (rv + 1);
  memcpy(rv->vars, vardecls, vd_size);
  return (JAST_Statement *) rv;

error_cleanup:
  for (size_t i = 0; i < n_vardecls; i++)
    {
      jast_binding_pattern_clear (&vardecls[i].binding);
      if (vardecls[i].initializer)
        jast_expr_free (vardecls[i].initializer);
    }
  MAYBE_CLEAR_STACK_STARTED_ARRAY(vardecls);
  return NULL;
}

static JAST_Statement *
parse_compound_stmt_without_braces (JAST_Lexer *lexer)
{
  DEFINE_STACK_STARTED_ARRAY(JAST_Statement*, subs, 16);
  while (lexer->has_cur && !lexer->cur.type == JAST_TOKEN_RBRACE)
    {
      JAST_Statement *stmt = parse_stmt (lexer);
      if (stmt == NULL)
        goto error_cleanup;
      APPEND_TO_STACK_STARTED_ARRAY(JAST_Statement *, subs, stmt);
    }
  size_t subs_size = sizeof (JAST_Statement *) * n_subs;
  size_t rv_size = sizeof (JAST_Statement_Compound) + subs_size;
  JAST_Statement_Compound *rv = alloc_statement(JAST_STATEMENT_COMPOUND, rv_size);
  rv->n_subs = n_subs;
  rv->subs = (JAST_Statement **) (rv + 1);
  memcpy (rv->subs, subs, subs_size);
  MAYBE_CLEAR_STACK_STARTED_ARRAY(subs);
  return rv;

error_cleanup:
  for (size_t i = 0; i < n_subs; i++)
    jast_statement_free (subs[i]);
  MAYBE_CLEAR_STACK_STARTED_ARRAY(subs);
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
      lexer->error = premature_eof_parse_error ("in compound statement");
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
      lexer->error = premature_eof_parse_error ("in compound statement");
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
  JS_Boolean is_op;
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
              for (size_t k = 0; k < out_at; k++)
                if (!pieces[k].is_op)
                  jast_expr_free (pieces[k].info.expr);
              for (size_t k = pieces + in_at + subsize; k < n_pieces; k++)
                if (!pieces[k].is_op)
                  jast_expr_free (pieces[k].info.expr);
              return NULL;
            }
          pieces[out_at].is_op = JS_FALSE;
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
          JS_Boolean expect_colon = i / 2 % 2;
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
  DEFINE_STACK_STARTED_ARRAY(ExprOpChainPiece, pieces, 16);

  while (lexer->has_cur
      && !is_expression_terminator_token_type (lexer->cur.type))
    {
      ExprOpChainPiece piece;
      unsigned prec;
      if (is_normal_operator (lexer->cur.type, &prec))
        {
          piece.is_op = JS_TRUE;
          piece.info.op.token_type = lexer->cur.type;
          piece.info.op.binary_op_precedence = prec;
        }
      else
        {
          JAST_Expr *expr = parse_opfree_expr (lexer);
          if (expr == NULL)
            {
              piece.is_op = JS_FALSE;
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
      APPEND_TO_STACK_STARTED_ARRAY(ExprOpChainPiece, pieces, piece);
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
          if (!token_type_is_postfix_op (pieces[piece_i].op.token_type))
            {
              lexer->error = parse_error("non-postfix operator");
              goto free_pieces_error;
            }
          pieces[piece_i].op.tag = TAG_POSTFIX;
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
              while (max_postfix_ops < n_ops - 1
                 && token_type_is_postfix_op (pieces[piece_i + 1 + max_postfix_ops].op.token_type))
                max_postfix_ops;
              while (max_prefix_ops < n_ops - 1
                 && token_type_is_prefix_op (pieces[next_piece_i - 1 - max_prefix_ops].op.token_type))
                max_prefix_ops;
              if (max_prefix_ops + max_postfix_ops + 1 < n_ops)
                {
                  /* at pieces[piece_i+1+max_prefix_ops] */
                  lexer->error = parse_error ("unexpected operator");
                  goto free_pieces_error;
                }
              unsigned n_next_prefix_ops = max_prefix_ops;
              unsigned n_postfix_ops = n_ops - 1 - n_next_prefix_ops;
              for (unsigned i = 0; i < n_postfix_ops; i++)
                pieces[piece_i + 1 + i].op.tag = TAG_POSTFIX;
              pieces[piece_i + 1 + n_postfix_ops].op.tag = TAG_INFIX;
              for (unsigned i = 0; i < n_next_prefix_ops; i++)
                pieces[piece_i + 1 + n_postfix_ops + 1].op.tag = TAG_PREFIX;
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
      pieces[out_at].is_op = JS_FALSE;
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

  /* Find the lowest prec level in use,
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
  MAYBE_CLEAR_STACK_STARTED_ARRAY(pieces);
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
              JS_String *label = token_to_string(&lexer->cur, lexer->data);
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

/*
LexicalBinding :== BindingIdentifier Initializeropt | BindingPattern Initializer
BindingList :== LexicalBinding | BindingList ',' LexicalBinding
BindingPattern :== ObjectBindingPattern | ArrayBindingPattern
ObjectBindingPattern ::= '{' (BindingProperty ,)* '}'
BindingProperty ::= SingleNameBinding | PropertyName ':' BindingElement
BindingElement ::= SingleNameBinding | BindingPattern Initializer
Initializer ::= '=' AssignmentExpression
*/
static JS_Boolean
parse_object_binding_pattern(JAST_Lexer *lexer, JAST_BindingPattern *out)
{
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_ERROR:
    case JAST_LEXER_ADVANCE_EOF:
      lexer->error = premature_eof_parse_error("after left-brace in binding-pattern");
      return JS_FALSE;
    }
  DEFINE_STACK_STARTED_ARRAY(JAST_FieldBindingPattern, fields, 16);
  while (lexer->cur.type != JAST_TOKEN_RBRACE)
    {
      JS_String *key = NULL;
      JAST_Expr *computed_key = NULL;
      switch (debareword_token_type (lexer->cur.type))
        {
          case JAST_TOKEN_BAREWORD:
            key = bareword_token_to_string (&lexer->cur);
            switch (jast_lexer_advance (lexer))
              {
              case JAST_LEXER_ADVANCE_OK: break;
              case JAST_LEXER_ADVANCE_ERROR: goto error_cleanup;
              case JAST_LEXER_ADVANCE_EOF: goto premature_eof;
              }
            break;
          case JAST_TOKEN_STRING:
            key = string_literal_to_string (&lexer->cur);
            switch (jast_lexer_advance (lexer))
              {
              case JAST_LEXER_ADVANCE_OK: break;
              case JAST_LEXER_ADVANCE_ERROR: goto error_cleanup;
              case JAST_LEXER_ADVANCE_EOF: goto premature_eof;
              }
          case JAST_TOKEN_NUMBER:
            ...
            switch (jast_lexer_advance (lexer))
              {
              case JAST_LEXER_ADVANCE_OK: break;
              case JAST_LEXER_ADVANCE_ERROR: goto error_cleanup;
              case JAST_LEXER_ADVANCE_EOF: goto premature_eof;
              }
            break;
          case JAST_TOKEN_LBRACKET:
            switch (jast_lexer_advance (lexer))
              {
              case JAST_LEXER_ADVANCE_OK: break;
              case JAST_LEXER_ADVANCE_ERROR: goto error_cleanup;
              case JAST_LEXER_ADVANCE_EOF: goto premature_eof;
              }
            computed_key = parse_expr (lexer);
            if (computed_key == NULL)
              goto error_cleanup;
            if (lexer->cur.type != JAST_TOKEN_RBRACKET)
              {
                set_expected_token_parse_error (lexer, "in quoted-string");
                jast_expr_free (computed_key);
                goto error_cleanup;
              }
            switch (jast_lexer_advance (lexer))
              {
              case JAST_LEXER_ADVANCE_OK: break;
              case JAST_LEXER_ADVANCE_ERROR: goto error_cleanup;
              case JAST_LEXER_ADVANCE_EOF: goto premature_eof;
              }
            break;
          default:
            ...
        }
      if (lexer->cur.type == JAST_TOKEN_COLON)
        {
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK: break;
            case JAST_LEXER_ADVANCE_ERROR: goto error_cleanup;
            case JAST_LEXER_ADVANCE_EOF: goto premature_eof;
            }
          if (!parse_binding_pattern (lexer, &binding_pattern))
            goto error_cleanup;
        }
      else if (key != NULL)
        {
          binding_pattern.type = JAST_BINDING_PATTERN_SIMPLE;
          binding_pattern.info.simple = key;
        }
      else
        {
          lexer->error = parse_error("expected key in object-binding pattern");
          return JS_FALSE;
        }
      if (lexer->cur.type == JAST_TOKEN_COMMA)
        {
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK: break;
            case JAST_LEXER_ADVANCE_ERROR: goto error_cleanup;
            case JAST_LEXER_ADVANCE_EOF: goto premature_eof;
            }
        }
      else if (lexer->cur.type != JAST_TOKEN_RBRACKET)
        {
          lexer->error = unexpected_token_parse_error(&lexer->cur, "in object-binding pattern");
          goto error_cleanup;
        }

      JAST_FieldBindingPattern field = {
        key,
        computed_key,
        binding_pattern
      };
      APPEND_TO_STACK_STARTED_ARRAY(JAST_FieldBindingPattern, fields, field);
    }
  out->type = JAST_BINDING_PATTERN_OBJECT;
  out->info.object.n_fields = n_fields;
  out->info.object.fields = alloc_array (n_fields, JAST_FieldBindingPattern);
  memcpy (out->info.object.fields, fields, sizeof (JAST_FieldBindingPattern) * n_fields);
  MAYBE_CLEAR_STACK_STARTED_ARRAY(fields);
  return JS_TRUE;

error_cleanup:
  for (size_t k = 0; k < n_fields; k++)
    {
      if (fields[k].key)
        js_string_unref (fields[k].key);
      if (fields[k].computed_key)
        jast_expr_free (fields[k].computed_key);
      jast_binding_pattern_clear (&fields[k].binding);
    }
  MAYBE_CLEAR_STACK_STARTED_ARRAY(fields);
  return JS_FALSE;
}
static JS_Boolean
parse_array_binding_pattern (JAST_Lexer *lexer,
                             JAST_BindingPattern *out)
{
/*
  ArrayBindingPattern :
        [ Elision(opt) BindingRestElement(opt) ]
        [ BindingElementList ]
        [ BindingElementList , Elisionopt BindingRestElement(opt) ]
  BindingElement :
        SingleNameBinding
        [+GeneratorParameter] BindingPattern Initializer(opt)
        [~GeneratorParameter] BindingPattern Initializer(opt)
  BindingRestElement :
        [+GeneratorParameter] ... BindingIdentifier
        [~GeneratorParameter] ... BindingIdentifier
 */
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_ERROR:
      return FALSE;
    case JAST_LEXER_ADVANCE_EOF:
      lexer->error = premature_eof_parse_error("after [ in binding-pattern");
      return JS_FALSE;
    }
  DEFINE_STACK_STARTED_ARRAY(JAST_BindingPattern, patterns, 16);
  while (lexer->cur.type != JAST_TOKEN_RBRACKET)
    {
      JAST_BindingPattern bp;
      if (lexer->cur.type == JAST_TOKEN_COMMA)
        {
          /* elided element */
          bp.type = JAST_BINDING_PATTERN_NONE;
          APPEND_TO_STACK_STARTED_ARRAY (patterns, bp);
        }
      else
        {
          if (!parse_binding_pattern (lexer, &bp))
            goto error_cleanup;
          if (!lexer->has_cur)
            {
              lexer->error = premature_eof_parse_error("in array binding-pattern");
              jast_binding_pattern_clear (&bp);
              goto error_cleanup;
            }
          if (lexer->cur.type == JAST_TOKEN_COMMA)
            {
              switch (jast_lexer_advance (lexer))
                {
                  ...
                }
            }
          else if (lexer->cur.type != JAST_TOKEN_RBRACKET)
            {
              ...
            }
        }
    }
  return JS_TRUE;
}
static JS_Boolean
parse_binding_pattern (JAST_Lexer *lexer, JAST_BindingPattern *out)
{
  if (!lexer->has_cur)
    {
      lexer->error = premature_eof_parse_error("parsing binding pattern");
      return JS_FALSE;
    }
  if (lexer->cur.type == JAST_TOKEN_BAREWORD)
    {
      out->type = JAST_BINDING_PATTERN_SIMPLE;
      out->info.simple = bareword_token_to_string (&lexer->cur);
      return JS_TRUE;
    }
  else if (lexer->cur.type == JAST_TOKEN_LBRACE)
    {
      return parse_object_binding_pattern (lexer, out);
    }
  else if (lexer->cur.type == JAST_TOKEN_LBRACKET)
    {
      return parse_array_binding_pattern (lexer, out);
    }
  else
    {
      lexer->error = unexpected_token_parse_error(&lexer->cur, "parsing variable binding");
      return JS_FALSE;
    }
  if (lexer->has_cur && lexer->cur.type == JAST_TOKEN_ASSIGN)
    {
      switch (jast_lexer_advance (lexer))
        {
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_ERROR:
          return JS_FALSE;
        case JAST_LEXER_ADVANCE_EOF:
          goto premature_eof;
        }
      out->initializer = parse_expr (lexer);
      if (out->initializer == NULL)
        {
          jast_binding_pattern_clear (out);
          return JS_FALSE;
        }
    }
  return JS_TRUE;

premature_eof:
  lexer->error = premature_eof_parse_error ("in binding pattern");
  return JS_FALSE;
}
