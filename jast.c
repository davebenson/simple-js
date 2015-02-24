#include "jast.h"
#include "jutf8.h"
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define YELL printf("at %s, line %d\n", __FILE__,__LINE__)

/* --- Stack Started Arrays --- */
#define DEFINE_STACK_STARTED_ARRAY(type, base_name, stack_size) \
  size_t n_##base_name = 0; \
  size_t base_name##_alloced = (stack_size); \
  type base_name##_stack[stack_size]; \
  type *base_name = base_name##_stack

#define ENSURE_SPACE_IN_STACK_STARTED_ARRAY(type, base_name, space) \
  do{ \
    if (n_##base_name + (space) > base_name##_alloced) { \
      size_t old_size = base_name##_alloced * sizeof(type); \
      do { \
        base_name##_alloced *= 2; \
      } while (n_##base_name + (space) > base_name##_alloced); \
      size_t new_size = base_name##_alloced * sizeof(type); \
      if (base_name == base_name##_stack) \
        base_name = memcpy(malloc(new_size), base_name, old_size); \
      else \
        base_name = realloc(base_name, new_size); \
    } \
  }while(0)

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
  JAST_TOKEN_ELSE,
  JAST_TOKEN_FOR,
  JAST_TOKEN_IN,
  JAST_TOKEN_OF,
  JAST_TOKEN_ON,
  JAST_TOKEN_DO,
  JAST_TOKEN_WHILE,
  JAST_TOKEN_WITH,
  JAST_TOKEN_SWITCH,
  JAST_TOKEN_CASE,
  JAST_TOKEN_DEFAULT,
  JAST_TOKEN_BREAK,
  JAST_TOKEN_CONTINUE,
  JAST_TOKEN_FUNCTION,
  JAST_TOKEN_VAR,
  JAST_TOKEN_CONST,
  JAST_TOKEN_LET,
  JAST_TOKEN_NULL,
  JAST_TOKEN_TRUE,
  JAST_TOKEN_FALSE,
  JAST_TOKEN_UNDEFINED,

#define FIRST_RESERVED_WORD_TOKEN JAST_TOKEN_IF
#define LAST_RESERVED_WORD_TOKEN JAST_TOKEN_UNDEFINED

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
  JAST_TOKEN_PLUS_PLUS,
  JAST_TOKEN_MINUS_MINUS,
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
  case JAST_TOKEN_OF: return "of";
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
  case JAST_TOKEN_BREAK: return "break";
  case JAST_TOKEN_CONTINUE: return "continue";
  case JAST_TOKEN_UNDEFINED: return "undefined";
  case JAST_TOKEN_FUNCTION: return "function";
  case JAST_TOKEN_BAREWORD: return "bareword";
  case JAST_TOKEN_STRING: return "string";
  case JAST_TOKEN_TEMPLATE_HEAD: return "template_head";
  case JAST_TOKEN_TEMPLATE_MIDDLE: return "template_middle";
  case JAST_TOKEN_TEMPLATE_TAIL: return "template_tail";
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

  case JAST_TOKEN_ASSIGN: return "=";
  case JAST_TOKEN_PLUS: return "+";
  case JAST_TOKEN_PLUS_PLUS: return "++";
  case JAST_TOKEN_MINUS: return "-";
  case JAST_TOKEN_MINUS_MINUS: return "--";
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
  case JAST_TOKEN_CMP_EXACT_EQ: return "===";
  case JAST_TOKEN_CMP_NUM_EQ: return "==";
  case JAST_TOKEN_CMP_EXACT_NE: return "!==";
  case JAST_TOKEN_CMP_NUM_NE: return "!=";
  case JAST_TOKEN_CMP_LT: return "<";
  case JAST_TOKEN_CMP_LE: return "<=";
  case JAST_TOKEN_CMP_GT: return ">";
  case JAST_TOKEN_CMP_GE: return ">=";

  default:
    // TODO double check by commenting this out?
    return "*unknown token*";
  }
}

static JAST_TokenType
convert_reserved_word_to_bareword (JAST_TokenType tt)
{
  if (FIRST_RESERVED_WORD_TOKEN <= tt && tt <= LAST_RESERVED_WORD_TOKEN)
    return JAST_TOKEN_BAREWORD;
  return tt;
}


const char *jast_expr_type_name (JAST_ExprType type)
{
  switch (type)
    {
      case JAST_EXPR_UNARY_OP: return "unary-op";
      case JAST_EXPR_BINARY_OP: return "binary-op";
      case JAST_EXPR_COND: return "cond";
      case JAST_EXPR_DOT: return "dot";
      case JAST_EXPR_INDEX: return "index";
      case JAST_EXPR_INVOKE: return "invoke";
      case JAST_EXPR_FUNCTION_VALUE: return "function-value";
      case JAST_EXPR_OBJECT_VALUE: return "object-value";
      case JAST_EXPR_ARRAY_VALUE: return "array-value";
      case JAST_EXPR_STRING_VALUE: return "string-value";
      case JAST_EXPR_NUMBER_VALUE: return "number-value";
      case JAST_EXPR_BOOLEAN_VALUE: return "boolean-value";
      case JAST_EXPR_UNDEFINED_VALUE: return "undefined-value";
      case JAST_EXPR_NULL_VALUE: return "null-value";
      case JAST_EXPR_IDENTIFIER: return "identifier";
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

  unsigned in_template_brace_depth;


  JAST_ParseError *error;
} JAST_Lexer;

static JAST_ParseError *
new_parse_error (JAST_ParseError_Type type,
                 JAST_Position *pos,
                 const char *format,
                 ...)
{
  char *err;
  va_list args;
  va_start (args, format);
  vasprintf (&err, format, args);
  va_end (args);
  JAST_ParseError *rv = malloc (sizeof (JAST_ParseError));
  rv->type = type;
  rv->position = *pos;
  rv->message = err;
  return rv;
}
static void
set_parse_error (JAST_Lexer *lexer,
                 JAST_ParseError_Type type,
                 const JAST_Position *pos,
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
set_bad_token_parse_error (JAST_Lexer *lexer,
                           JAST_Token *token,
                                const char *situation)
{
  set_parse_error (
    lexer,
    JAST_PARSE_ERROR_BAD_TOKEN,
    &token->position,
    "unexpected token %s, in %s",
    jast_token_type_name(token->type),
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

char *
jast_parse_error_to_string (const JAST_ParseError *error)
{
  char *rv;
  asprintf (&rv, "parse error: %s [%s:%u]",
            error->message,
            JS_STRING_GET_STR (error->position.filename),
            error->position.line_number);
  return rv;
}

static JS_String *
bareword_token_to_string (JAST_Lexer *lexer, const JAST_Token *token)
{
  const char *start = lexer->data + token->offset;

  // TODO: backslash sequences in symbols are not supported yet.
  assert (memchr (start, '\\', token->length) == NULL);

  JS_String *rv = js_string_new_utf8_len (token->length, start);
  return rv;
}

static JS_Boolean
interpret_bs_sequence_length (JAST_Lexer *lexer,
                              const char *at,
                              const char *end,
                              unsigned   *bs_len_out,
                              char       *utf8_out,
                              unsigned   *utf8_len_out)
{
  unsigned unicode_value;
  switch (*at)
    {
    case 'b':
      *bs_len_out = 1;
      *utf8_out = '\b';
      *utf8_len_out = 1;
      return JS_TRUE;

    case 'f':
      *bs_len_out = 1;
      *utf8_out = '\f';
      *utf8_len_out = 1;
      return JS_TRUE;

    case 'n':
      *bs_len_out = 1;
      *utf8_out = '\n';
      *utf8_len_out = 1;
      return JS_TRUE;

    case 'r':
      *bs_len_out = 1;
      *utf8_out = '\r';
      *utf8_len_out = 1;
      return JS_TRUE;

    case 't':
      *bs_len_out = 1;
      *utf8_out = '\t';
      *utf8_len_out = 1;
      return JS_TRUE;

    case 'v':
      *bs_len_out = 1;
      *utf8_out = '\v';
      *utf8_len_out = 1;
      return JS_TRUE;

    case '0':
      if (at + 1 < end && !IS_OCTAL_DIGIT (at[1]))
        {
          *bs_len_out = 1;
          *utf8_out = '\0';
          *utf8_len_out = 1;
          return JS_TRUE;
        }

      /* fallthrough to obsolete octal parsing */

    case '1': case '2': case '3': case '4': case '5': case '6': case '7':
      if (lexer->support_legacy_octal)
        {
          /* octal not allowed */
          return JS_FALSE;
        }
      if (at + 2 < end && IS_OCTAL_DIGIT (at[2]))
        {
          unicode_value = ((at[0] - '0') << 6)
                        | ((at[1] - '0') << 3)
                        | ((at[2] - '0'));
          goto has_unicode_value;
        }
      return JS_FALSE;

    case 'u':
      {
        /* hex unicode */
        if (at + 1 < end && at[1] == LBRACE_CHAR)
          {
            unicode_value = 0;
            at += 2;
            while (at < end && *at != RBRACE_CHAR)
              {
                if (IS_HEXIDECIMAL_DIGIT (*at))
                  {
                    unicode_value <<= 4;
                    unicode_value += HEXDIGIT_VALUE (*at);
                  }
                else
                  {
                    return JS_FALSE;
                  }
              }
            if (at == end)
              return JS_FALSE;
            goto has_unicode_value;
          }
        else if (at + 4 < end)
          {
            if (!IS_HEXIDECIMAL_DIGIT (at[1])
             || !IS_HEXIDECIMAL_DIGIT (at[2])
             || !IS_HEXIDECIMAL_DIGIT (at[3])
             || !IS_HEXIDECIMAL_DIGIT (at[4]))
              return JS_FALSE;
            unicode_value = (HEXDIGIT_VALUE (at[1]) << 12)
                          | (HEXDIGIT_VALUE (at[2]) << 8)
                          | (HEXDIGIT_VALUE (at[3]) << 4)
                          | (HEXDIGIT_VALUE (at[4]) << 0);
            goto has_unicode_value;
          }
        else
          {
            return JS_FALSE;
          }
        break;
      }

    default:
      *bs_len_out = 1;
      *utf8_out = *at;
      *utf8_len_out = 1;
      return JS_TRUE;
    }

has_unicode_value:
  *utf8_len_out = jutf8_encode1 (unicode_value, utf8_out);
  return JS_TRUE;
}

static JS_String *
string_literal_to_string (JAST_Lexer *lexer, const JAST_Token *token)
{
  /* See spec 11.8.4 */
  assert(token->type == JAST_TOKEN_STRING);
  const char *at = lexer->data + token->offset;
  const char *end = lexer->data + lexer->data_size;
  char q = *at++;
  DEFINE_STACK_STARTED_ARRAY(char, buf, 256);

  assert (q == '\'' || q == '"');
  while (at < end && *at != q)
    {
      if (*at == '\\')
        {
          unsigned bs_seq_len, utf8_len;
          ENSURE_SPACE_IN_STACK_STARTED_ARRAY (char, buf, 10);
          if (!interpret_bs_sequence_length (lexer,
                                             at + 1, end,
                                             &bs_seq_len,
                                             buf + n_buf,
                                             &utf8_len))
            {
              set_parse_error (lexer, JAST_PARSE_ERROR_BAD_CHARACTER,
                               &token->position,
                               "bad backslash in quoted string");
              MAYBE_CLEAR_STACK_STARTED_ARRAY (buf);
              return NULL;
            }
          at += 1 + bs_seq_len;
          n_buf += utf8_len;
        }
      else
        {
          APPEND_TO_STACK_STARTED_ARRAY (char, buf, *at);
          at++;
        }
    }
  if (at == end)
    {
      MAYBE_CLEAR_STACK_STARTED_ARRAY (buf);
      set_premature_eof_parse_error (lexer, "in string literal");
      return NULL;
    }
  JS_String *rv = js_string_new_utf8_len (n_buf, buf);
  MAYBE_CLEAR_STACK_STARTED_ARRAY (buf);
  return rv;
}

static JS_String *
number_literal_to_key_string (JAST_Lexer *lexer, JAST_Token *token)
{
  double nv = strtod (lexer->data + token->offset, NULL);
  return js_string_new_from_number (nv);
}

typedef enum {
  JAST_LEXER_ADVANCE_OK,
  JAST_LEXER_ADVANCE_EOF,
  JAST_LEXER_ADVANCE_ERROR
} JAST_Lexer_AdvanceResult;

#if 0
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
#endif

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
  const char *tail_start = lexer->data + lexer->offset;
  const char *at = tail_start;
  const char *end = lexer->data + lexer->data_size;
  assert(at < end && *at == RBRACE_CHAR);
  at++;
  while (at < end && *at != '`')
    {
      if (at[0] == '$' && at + 1 < end && at[1] == LBRACE_CHAR)
        {
          at += 2;
          token->type = JAST_TOKEN_TEMPLATE_MIDDLE;
          token->length = at - tail_start;
          lexer->offset = at - lexer->data;
          lexer->in_template_brace_depth = 1;
          return JAST_LEXER_ADVANCE_OK;
        }
      else if (at[0] == '\\')
        {
          at++;
          if (at == end)
            {
              set_premature_eof_parse_error(lexer, "in template \\ expression");
              return JAST_LEXER_ADVANCE_ERROR;
            }
          at++;
        }
      else
        at++;
    }
  if (at == end)
    {
      set_premature_eof_parse_error (lexer, "in template-string");
      return JAST_LEXER_ADVANCE_ERROR;
    }
  at++;
  token->type = JAST_TOKEN_TEMPLATE_TAIL;
  token->length = at - tail_start;
  lexer->offset = at - lexer->data;
  lexer->in_template_brace_depth = 0;
  return JAST_LEXER_ADVANCE_OK;
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

static JAST_TokenType
compute_bareword_token_type (size_t len, const char *bw)
{
  switch (len)
    {
    case 2:
      switch (*bw)
        {
        case 'i':
          if (bw[1] == 'f') return JAST_TOKEN_IF;
          if (bw[1] == 'n') return JAST_TOKEN_IN;
          return JAST_TOKEN_BAREWORD;
        case 'o':
          if (bw[1] == 'f') return JAST_TOKEN_OF;
          if (bw[1] == 'n') return JAST_TOKEN_ON;
          return JAST_TOKEN_BAREWORD;
        case 'd':
          if (bw[1] == 'o') return JAST_TOKEN_DO;
          return JAST_TOKEN_BAREWORD;
        }
      return JAST_TOKEN_BAREWORD;
    case 3:
      switch (*bw)
        {
        case 'f':
          if (bw[1] == 'o' && bw[2] == 'r') return JAST_TOKEN_FOR;
          return JAST_TOKEN_BAREWORD;
        case 'v':
          if (bw[1] == 'a' && bw[2] == 'r') return JAST_TOKEN_VAR;
          return JAST_TOKEN_BAREWORD;
        case 'l':
          if (bw[1] == 'e' && bw[2] == 't') return JAST_TOKEN_LET;
          return JAST_TOKEN_BAREWORD;
        }
      return JAST_TOKEN_BAREWORD;
    case 4:
      switch (*bw)
        {
        case 'e':
          if (bw[1] == 'l' && bw[2] == 's' && bw[3] == 'e') return JAST_TOKEN_ELSE;
          return JAST_TOKEN_BAREWORD;
        case 'w':
          if (bw[1] == 'i' && bw[2] == 't' && bw[3] == 'h') return JAST_TOKEN_WITH;
          return JAST_TOKEN_BAREWORD;
        case 'c':
          if (bw[1] == 'a' && bw[2] == 's' && bw[3] == 'e') return JAST_TOKEN_CASE;
          return JAST_TOKEN_BAREWORD;
        case 'n':
          if (bw[1] == 'u' && bw[2] == 'l' && bw[3] == 'l') return JAST_TOKEN_NULL;
          return JAST_TOKEN_BAREWORD;
        case 't':
          if (bw[1] == 'r' && bw[2] == 'u' && bw[3] == 'e') return JAST_TOKEN_TRUE;
          return JAST_TOKEN_BAREWORD;
        }
      return JAST_TOKEN_BAREWORD;
    case 5:
      switch (*bw)
        {
        case 'b':
          if (memcmp (bw+1, "reak", 4) == 0) return JAST_TOKEN_BREAK;
          return JAST_TOKEN_BAREWORD;
        case 'w':
          if (memcmp (bw+1, "hile", 4) == 0) return JAST_TOKEN_WHILE;
          return JAST_TOKEN_BAREWORD;
        case 'c':
          if (memcmp (bw+1, "onst", 4) == 0) return JAST_TOKEN_CONST;
          return JAST_TOKEN_BAREWORD;
        case 'f':
          if (memcmp (bw+1, "alse", 4) == 0) return JAST_TOKEN_FALSE;
          return JAST_TOKEN_BAREWORD;
        }
      return JAST_TOKEN_BAREWORD;

    case 6:
      if (memcmp (bw, "switch", 6) == 0) return JAST_TOKEN_SWITCH;
      return JAST_TOKEN_BAREWORD;

    case 7:
      if (memcmp (bw, "default", 6) == 0) return JAST_TOKEN_DEFAULT;
      return JAST_TOKEN_BAREWORD;

    case 8:
      if (memcmp (bw, "continue", 8) == 0) return JAST_TOKEN_CONTINUE;
      if (memcmp (bw, "function", 8) == 0) return JAST_TOKEN_FUNCTION;
      return JAST_TOKEN_BAREWORD;

    case 9:
      if (memcmp (bw, "undefined", 8) == 0) return JAST_TOKEN_UNDEFINED;
      return JAST_TOKEN_BAREWORD;

    default:
      return JAST_TOKEN_BAREWORD;
    }
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
  lexer->offset = token->offset = at - lexer->data;

#define IMPLEMENT_N_CHARACTER_TOKEN(token_shortname, N) \
    do{ token->type = JAST_TOKEN_##token_shortname; \
        token->length = N; \
        lexer->offset += N; \
        return JAST_LEXER_ADVANCE_OK; }while(0)
#define IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(token_shortname) \
    do{ if (at + 1 < end && at[1] == '=') \
          IMPLEMENT_N_CHARACTER_TOKEN(token_shortname##_ASSIGN, 2); \
        else \
          IMPLEMENT_N_CHARACTER_TOKEN(token_shortname, 1);       \
    }while(0)

  switch (*at)
    {
      case '!':
        if (at + 1 < end && at[1] == '=')
          {
            if (at + 2 < end && at[2] == '=')
              IMPLEMENT_N_CHARACTER_TOKEN(CMP_EXACT_NE, 3);
            else
              IMPLEMENT_N_CHARACTER_TOKEN(CMP_NUM_NE, 2);
          }
        else
          IMPLEMENT_N_CHARACTER_TOKEN(LOGICAL_NOT, 1);

      case '~':
        IMPLEMENT_N_CHARACTER_TOKEN(BITWISE_NOT, 1);

      case '%':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(MOD);

      case '+':
        if (at + 1 < end && at[1] == '+')
          IMPLEMENT_N_CHARACTER_TOKEN(PLUS_PLUS, 2);
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(PLUS);
        break;
      case '-':
        if (at + 1 < end && at[1] == '-')
          IMPLEMENT_N_CHARACTER_TOKEN(MINUS_MINUS, 2);
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(MINUS);

      case '^':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(BITWISE_XOR);

      case '&':
        if (at + 1 < end && at[1] == '&')
          IMPLEMENT_N_CHARACTER_TOKEN(LOGICAL_AND, 2);
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(BITWISE_AND);

      case '|':
        if (at + 1 < end && at[1] == '|')
          IMPLEMENT_N_CHARACTER_TOKEN(LOGICAL_OR, 2);
        else
          IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(BITWISE_OR);

      case '*':
        IMPLEMENT_SINGLE_CHARACTER_TOKEN_OR_ASSIGN(MULTIPLY);

      case '(':
        IMPLEMENT_N_CHARACTER_TOKEN(LPAREN, 1);

      case ')':
        IMPLEMENT_N_CHARACTER_TOKEN(RPAREN, 1);

      case '[':
        IMPLEMENT_N_CHARACTER_TOKEN(LBRACKET, 1);
      case ']':
        IMPLEMENT_N_CHARACTER_TOKEN(RBRACKET, 1);
      case '{':
        if (lexer->in_template_brace_depth > 0)
          lexer->in_template_brace_depth++;
        IMPLEMENT_N_CHARACTER_TOKEN(LBRACE, 1);
      case '}':
        if (lexer->in_template_brace_depth == 1)
          {
            return lexer_get_next_token_raw_template_tail(lexer, token);
          }
        else
          {
            if (lexer->in_template_brace_depth > 0)
              lexer->in_template_brace_depth++;
            IMPLEMENT_N_CHARACTER_TOKEN(RBRACE, 1);
          }

      case ',':
        IMPLEMENT_N_CHARACTER_TOKEN(COMMA, 1);

      case '.':
        if (at + 1 < end && '0' <= at[1] && at[1] <= '9')
          {
            return lexer_get_next_token_raw_number(lexer, token);
          }
        else
          IMPLEMENT_N_CHARACTER_TOKEN(DOT, 1);

      case ';':
        IMPLEMENT_N_CHARACTER_TOKEN(SEMICOLON, 1);

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

      case '?':
        IMPLEMENT_N_CHARACTER_TOKEN(QUESTION_MARK, 1);

      case ':':
        IMPLEMENT_N_CHARACTER_TOKEN(COLON, 1);

      case '"':
      case '\'':
        return lexer_get_next_token_raw_qstring(lexer, token);

      case '`':
        return lexer_get_next_token_raw_template_head(lexer, token);
        

      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        return lexer_get_next_token_raw_number(lexer, token);

      case '<':
        if (at + 1 < end && at[1] == '<')
          IMPLEMENT_N_CHARACTER_TOKEN(SHIFT_LEFT, 2);
        else if (at + 1 < end && at[1] == '=')
          IMPLEMENT_N_CHARACTER_TOKEN(CMP_LE, 2);
        else
          IMPLEMENT_N_CHARACTER_TOKEN(CMP_LT, 1);
        break;

      case '>':
        if (at + 1 < end && at[1] == '>')
          {
            if (at + 2 < end && at[2] == '>')
              {
                if (at + 3 < end && at[3] == '=')
                  {
                    IMPLEMENT_N_CHARACTER_TOKEN(SHIFT_UNSIGNED_RIGHT_ASSIGN, 4);
                  }
                else
                  {
                    IMPLEMENT_N_CHARACTER_TOKEN(SHIFT_UNSIGNED_RIGHT, 3);
                  }
              }
            else
              IMPLEMENT_N_CHARACTER_TOKEN(SHIFT_RIGHT, 2);
          }
        else if (at + 1 < end && at[1] == '=')
          IMPLEMENT_N_CHARACTER_TOKEN(CMP_GE, 2);
        else
          IMPLEMENT_N_CHARACTER_TOKEN(CMP_GT, 1);
        break;

      case '=':
        if (at + 1 < end && at[1] == '=')
          {
            if (at + 2 < end && at[2] == '=')
              IMPLEMENT_N_CHARACTER_TOKEN(CMP_EXACT_EQ, 3);
            else
              IMPLEMENT_N_CHARACTER_TOKEN(CMP_NUM_EQ, 2);
          }
        else
          IMPLEMENT_N_CHARACTER_TOKEN(ASSIGN, 1);
        break;
    }

  unsigned bw_start = jutf8_scan_idstart_char (at);
  if (bw_start > 0)
    {
      const char *start = at;
      unsigned l;
      at += bw_start;
      while (at < end && (l=jutf8_scan_idcontinue_char (at)) != 0)
        at += l;
      token->length = at - start;
      token->type = compute_bareword_token_type (at - start, start);
      lexer->offset = at - lexer->data;
      return JAST_LEXER_ADVANCE_OK;
    }

  // Bad character error
  set_bad_character_parse_error (lexer, at);
  return JAST_LEXER_ADVANCE_ERROR;
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
  lexer->offset = 0;
  lexer->token_index = 0;
  lexer->position.filename = js_string_new_utf8 (filename);
  lexer->position.line_number = 1;
  lexer->error = NULL;
  lexer->support_legacy_octal = 0;
  switch (lexer_get_next_token_raw (lexer, &lexer->cur))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        lexer->has_cur = lexer->has_next = lexer->has_next_next = JS_FALSE;
        return JS_TRUE;
      case JAST_LEXER_ADVANCE_ERROR:
        goto failed;
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
        goto failed;
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
        goto failed;
    }
  lexer->has_next_next = JS_TRUE;
  return JS_TRUE;


failed:
  *error = lexer->error;
  lexer->error = NULL;
  js_string_unref(lexer->position.filename);
  return JS_FALSE;
}

static JAST_Lexer_AdvanceResult jast_lexer_advance (JAST_Lexer *lexer)
{
  if (!lexer->has_cur)
    {
      set_premature_eof_parse_error (lexer, "advance past EOF");
      return JAST_LEXER_ADVANCE_ERROR;
    }
  lexer->has_cur = lexer->has_next;
  lexer->has_next = lexer->has_next_next;
  lexer->cur = lexer->next;
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
  unsigned n_virtuals = 0;
  for (i = 0; i < n_pieces; i++)
    {
      switch (pieces[i].matcher_type)
        {
          case MATCHER_TYPE_BY_TOKEN_TYPE:
            {
              JAST_TokenType expected_tt = pieces[i].info.by_token_type;
              if (!lexer->has_cur)
                {
                  set_parse_error(lexer, JAST_PARSE_ERROR_PREMATURE_EOF,
                                  &lexer->position,
                                  "matching token %s",
                                  jast_token_type_name (expected_tt));
                  goto handle_error;
                }

              JAST_TokenType actual_tt = lexer->cur.type;
              if (actual_tt != expected_tt)
                {
                  // XXX: need a "situation" parameter that's more specific
                  set_expected_token_parse_error (lexer, expected_tt, "matching");
                  goto handle_error;
                }
              if (jast_lexer_advance (lexer) == JAST_LEXER_ADVANCE_ERROR)
                goto handle_error;
              break;
            }

          case MATCHER_TYPE_VIRTUAL:
            {
              struct MatcherVirtual *v = pieces[i].info.matcher_virtual;
              void *obj = v->parse(lexer);
              if (obj == NULL)
                goto handle_error;
              virtuals_out[n_virtuals++] = obj;
              break;
            }
        }
    }
  return JS_TRUE;

handle_error:
  {
    unsigned virtuals_index = 0;
    for (size_t j = 0; j < i; j++)
      if (pieces[i].matcher_type == MATCHER_TYPE_VIRTUAL)
        {
          void *v = virtuals_out[virtuals_index++];
          struct MatcherVirtual *mv = pieces[i].info.matcher_virtual;
          mv->destroy (v);
        }
  }
  return JS_FALSE;
}

#define MATCHER_PIECE_BY_TOKEN_TYPE(token_type_suffix) \
{ \
  .matcher_type = MATCHER_TYPE_BY_TOKEN_TYPE, \
  .info = { \
    .by_token_type = JAST_TOKEN_##token_type_suffix \
  } \
}

#define MATCHER_PIECE_VIRTUAL(matcher_virtual_data) \
{ \
  .matcher_type = MATCHER_TYPE_VIRTUAL, \
  .info = { \
    .matcher_virtual = &matcher_virtual_data \
  } \
}

typedef enum
{
  PARSE_EXPR_ALLOW_COMMA = (1<<0),
  PARSE_EXPR_ALLOW_TERNARY = (1<<1)
} ParseExprFlags;


static JAST_Expr      *parse_expr(JAST_Lexer *lexer, ParseExprFlags flags);
static JAST_Statement *parse_stmt(JAST_Lexer *lexer);
static JS_Boolean      parse_binding_pattern (JAST_Lexer *lexer,
                                              JAST_BindingPattern *out);
static void            binding_pattern_clear (JAST_BindingPattern *pattern);

static JAST_Expr      *parse_expr_no_comma(JAST_Lexer *lexer)
{
  return parse_expr (lexer, PARSE_EXPR_ALLOW_TERNARY);
}
static struct MatcherVirtual matcher_virtual_parse_expr =
{
  .parse = (void *(*)(JAST_Lexer *)) parse_expr_no_comma,
  .destroy = (void (*)(void *)) jast_expr_free
};

static struct MatcherVirtual matcher_virtual_parse_statement =
{
  .parse = (void *(*)(JAST_Lexer *)) parse_stmt,
  .destroy = (void (*)(void *)) jast_statement_free
};

static struct MatcherPiece if_stmt_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(IF),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_statement),
};

static struct MatcherPiece else_if_stmt_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(ELSE),
  MATCHER_PIECE_BY_TOKEN_TYPE(IF),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_statement),
};

static struct MatcherPiece else_stmt_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(ELSE),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_statement),
};

/* Array must actually be a c-level array,
   meaning that the sizeof(arr)/sizeof(arr[0]) === n_elements trick
   must work. */
#define DO_STATIC_MATCHES(lexer, array, pad) \
  match_pieces ((lexer), sizeof(array)/sizeof((array)[0]), (array), (pad))


static void *
alloc_statement (JAST_StatementType type,
                 size_t             alloc_size)
{
  // TODO
  //assert (alloc_size >= get_min_alloc_size_for_type (type));
  assert(alloc_size >= sizeof (JAST_Base_Statement));

  JAST_Base_Statement *rv = malloc (alloc_size);
  rv->type = type;
  memset (rv + 1, 0, alloc_size - sizeof (JAST_Base_Statement));
  return rv;
}

static void *
alloc_expr (JAST_ExprType type,
                 size_t             alloc_size)
{
  // TODO
  //assert (alloc_size >= get_min_alloc_size_for_type (type));
  assert(alloc_size >= sizeof (JAST_Base_Expr));

  JAST_Base_Expr *rv = malloc (alloc_size);
  rv->type = type;
  memset (rv + 1, 0, alloc_size - sizeof (JAST_Base_Expr));
  return rv;
}

static JAST_Statement *
parse_if_stmt(JAST_Lexer *lexer)
{
  void *pad[2];
  DEFINE_STACK_STARTED_ARRAY(JAST_ConditionalStatement_Clause, cstmts, 8);

  if (!DO_STATIC_MATCHES (lexer, if_stmt_pieces, pad))
    return NULL;

  n_cstmts = 1;
  cstmts[0].expr = pad[0];
  cstmts[0].statement = pad[1];

  JAST_Statement *else_statement = NULL;

  while (lexer->has_cur && lexer->cur.type == JAST_TOKEN_ELSE)
    {
      if (lexer->has_next && lexer->next.type == JAST_TOKEN_IF)
        {
          if (!DO_STATIC_MATCHES (lexer, else_if_stmt_pieces, pad))
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
          if (!DO_STATIC_MATCHES (lexer, else_stmt_pieces, pad))
            goto error;
          else_statement = pad[0];
          break;
        }
    }

  size_t cstmt_size = sizeof(JAST_ConditionalStatement_Clause) * n_cstmts;
  size_t rv_size = sizeof(JAST_If_Statement) + cstmt_size;
  JAST_If_Statement *rv = alloc_statement(JAST_STATEMENT_IF, rv_size);
  rv->n_conditional_statements = n_cstmts;
  rv->conditional_statements = (JAST_ConditionalStatement_Clause*)(rv+1);
  memcpy(rv->conditional_statements, cstmts, cstmt_size);
  rv->else_statement = else_statement;
  MAYBE_CLEAR_STACK_STARTED_ARRAY(cstmts);
  return (JAST_Statement *) rv;

error:
  for (size_t k = 0; k < n_cstmts; k++)
    {
      jast_expr_free (cstmts[k].expr);
      jast_statement_free (cstmts[k].statement);
    }
  MAYBE_CLEAR_STACK_STARTED_ARRAY(cstmts);
  return NULL;
}
static struct MatcherPiece while_loop_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(WHILE),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_statement),
};

static JAST_Statement *
parse_while_stmt(JAST_Lexer *lexer)
{
  void *pad[2];
  if (!DO_STATIC_MATCHES (lexer, while_loop_pieces, pad))
    return NULL;
  size_t rv_size = sizeof(JAST_While_Statement);
  JAST_While_Statement *rv = alloc_statement(JAST_STATEMENT_WHILE, rv_size);
  rv->condition = pad[0];
  rv->body = pad[1];
  return (JAST_Statement *) rv;
}

static struct MatcherPiece do_while_loop_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(DO),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_statement),
  MATCHER_PIECE_BY_TOKEN_TYPE(WHILE),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
  MATCHER_PIECE_BY_TOKEN_TYPE(SEMICOLON),
};
static JAST_Statement *
parse_do_while_stmt (JAST_Lexer *lexer)
{
  void *pad[2];
  if (!DO_STATIC_MATCHES (lexer, do_while_loop_pieces, pad))
    return NULL;
  size_t rv_size = sizeof (JAST_DoWhile_Statement);
  JAST_DoWhile_Statement *rv = alloc_statement(JAST_STATEMENT_DO_WHILE, rv_size);
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
  DEFINE_STACK_STARTED_ARRAY(JAST_BindingPattern, vardecls, 8);
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK: break;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "after for");
      return NULL;
    case JAST_LEXER_ADVANCE_ERROR:
      return NULL;
    }
  if (lexer->cur.type != JAST_TOKEN_LPAREN)
    { 
      set_expected_token_parse_error (lexer, JAST_TOKEN_LPAREN, "for-statement");
      return NULL;
    }
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK: break;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "after for");
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
          set_premature_eof_parse_error(lexer, "after var/let/const");
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
  switch (lexer->cur.type)
    {
    case JAST_TOKEN_OF:
      is_for_in = JS_FALSE;
      goto for_in_loop;
    case JAST_TOKEN_IN:
      is_for_in = JS_TRUE;
      goto for_in_loop;

      if (lexer->cur.type == JAST_TOKEN_SEMICOLON
       || lexer->cur.type == JAST_TOKEN_COMMA)
        goto cstyle_for_loop;
      else
        {
          set_bad_token_parse_error (lexer, &lexer->cur, "after parsing variable binding");
          return NULL;
        }

    case JAST_TOKEN_SEMICOLON:
    case JAST_TOKEN_COMMA:
      goto cstyle_for_loop;

    default:
      set_bad_token_parse_error (lexer, &lexer->cur, "after variable declaration");
      binding_pattern_clear(&bp);
      return NULL;
    }


for_in_loop:
/* skip in/of token */
  switch (jast_lexer_advance(lexer))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        binding_pattern_clear(&bp);
        set_premature_eof_parse_error(lexer, "in for-of");
        return NULL;
      case JAST_LEXER_ADVANCE_ERROR:
        binding_pattern_clear(&bp);
        return NULL;
    }
  JAST_Expr *container = parse_expr (lexer, PARSE_EXPR_ALLOW_TERNARY);
  if (container == NULL)
    {
      binding_pattern_clear (&bp);
      return NULL;
    }
  if (!lexer->has_cur)
    {
      set_premature_eof_parse_error (lexer, "after container in for-loop");
      binding_pattern_clear (&bp);
      jast_expr_free (container);
      return NULL;
    }
  if (lexer->cur.type != JAST_TOKEN_RPAREN)
    {
      set_bad_token_parse_error(lexer, &lexer->cur, "after variable declaration");
      binding_pattern_clear (&bp);
      jast_expr_free (container);
      return NULL;
    }
  switch (jast_lexer_advance (lexer))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        set_premature_eof_parse_error (lexer, "for-in body");
        binding_pattern_clear(&bp);
        jast_expr_free(container);
        return NULL;
      case JAST_LEXER_ADVANCE_ERROR:
        binding_pattern_clear(&bp);
        jast_expr_free(container);
        return NULL;
    }
  JAST_Statement *body = parse_stmt (lexer);
  if (body == NULL)
    {
      binding_pattern_clear(&bp);
      jast_expr_free(container);
      return NULL;
    }
  JAST_ForIn_Statement *loop = alloc_statement(JAST_STATEMENT_FOR_IN, sizeof (JAST_ForIn_Statement));
  loop->binding = bp;
  loop->is_for_in = is_for_in;
  loop->container = container;
  loop->body = body;
  return (JAST_Statement *) loop;


cstyle_for_loop:
  n_vardecls = 1;
  vardecls[0] = bp;
  while (lexer->cur.type == JAST_TOKEN_COMMA)
    {
      switch (jast_lexer_advance (lexer))
        {
          case JAST_LEXER_ADVANCE_OK:
            break;
          case JAST_LEXER_ADVANCE_EOF:
            set_premature_eof_parse_error (lexer, "for variable declarations");
            goto error_in_vardecls;
          case JAST_LEXER_ADVANCE_ERROR:
            goto error_in_vardecls;
        }

      // parse binding pattern
      if (!parse_binding_pattern(lexer, &bp))
        goto error_in_vardecls;

      APPEND_TO_STACK_STARTED_ARRAY(JAST_BindingPattern, vardecls, bp);
    }
  if (lexer->cur.type != JAST_TOKEN_SEMICOLON)
    {
      set_expected_token_parse_error (lexer, JAST_TOKEN_SEMICOLON, "after for-init clause");
      return NULL;
    }
  size_t init_size = sizeof (JAST_VariableDeclarations_Statement)
                   + sizeof (JAST_BindingPattern) * n_vardecls;
  JAST_VariableDeclarations_Statement *initial = alloc_statement(JAST_STATEMENT_VARIABLE_DECLARATIONS, init_size);
  initial->type = vartype;
  initial->n_vars = n_vardecls;
  initial->vars = memcpy (initial + 1, vardecls, sizeof (JAST_BindingPattern) * n_vardecls);
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "for conditional");
      goto error_in_conditional;
    case JAST_LEXER_ADVANCE_ERROR:
      goto error_in_conditional;
    }

  JAST_Expr *conditional;
  if (lexer->cur.type == JAST_TOKEN_SEMICOLON)
    {
      /* empty conditional */
      conditional = NULL;
    }
  else
    {
      conditional = parse_expr (lexer, PARSE_EXPR_ALLOW_COMMA|PARSE_EXPR_ALLOW_TERNARY);
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
      set_premature_eof_parse_error (lexer, "after second ; in for-loop");
      goto error_in_update;
    case JAST_LEXER_ADVANCE_ERROR:
      goto error_in_update;
    }

  JAST_Statement *update;
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
          set_premature_eof_parse_error (lexer, "expecting for-loop body");
          goto error_in_body;
        }
      if (lexer->cur.type != JAST_TOKEN_RPAREN)
        {
          set_expected_token_parse_error (lexer, JAST_TOKEN_RPAREN, "after for-loop head");
          goto error_in_body;
        }
    }

  assert(lexer->cur.type == JAST_TOKEN_RPAREN);
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "expected for-loop body");
      goto error_in_body;
    case JAST_LEXER_ADVANCE_ERROR:
      goto error_in_body;
    }
  body = parse_stmt (lexer);
  if (body == NULL)
    goto error_in_body;

  JAST_For_Statement *rv = alloc_statement(JAST_STATEMENT_FOR, sizeof(JAST_For_Statement));
  rv->initial = (JAST_Statement *) initial;
  rv->condition = conditional;
  rv->advance = update;
  rv->body = body;
  return (JAST_Statement *) rv;

error_in_vardecls:
  for (size_t j = 0; j < n_vardecls; j++)
    binding_pattern_clear (&vardecls[j]);
  MAYBE_CLEAR_STACK_STARTED_ARRAY(vardecls);
  return NULL;

error_in_body:
  if (update != NULL)
    jast_statement_free (update);

  /* fallthrough */

error_in_update:
  if (conditional != NULL)
    jast_expr_free (conditional);

  /* fallthrough */

error_in_conditional:
  if (initial != NULL)
    jast_statement_free ((JAST_Statement *) initial);
  return NULL;
}


static struct MatcherPiece with_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(WITH),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_statement),
};

static JAST_Statement *
parse_with_statement (JAST_Lexer *lexer)
{
  void *pad[2];
  if (!DO_STATIC_MATCHES (lexer, with_pieces, pad))
    return NULL;
  JAST_With_Statement *rv = alloc_statement(JAST_STATEMENT_WITH, sizeof(JAST_With_Statement));
  rv->expr = pad[0];
  rv->body = pad[1];
  return (JAST_Statement *) rv;
}

static struct MatcherPiece switch_header_pieces[] = {
  MATCHER_PIECE_BY_TOKEN_TYPE(SWITCH),
  MATCHER_PIECE_BY_TOKEN_TYPE(LPAREN),
  MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_expr),
  MATCHER_PIECE_BY_TOKEN_TYPE(RPAREN),
  MATCHER_PIECE_BY_TOKEN_TYPE(LBRACE),
};

static JAST_Statement *
parse_switch_statement (JAST_Lexer *lexer)
{
  DEFINE_STACK_STARTED_ARRAY(JAST_Switch_Clause, clauses, 24);
  void *pad_expr[1];
  JS_Boolean has_default = JS_FALSE;
  JAST_Expr *expr;
  if (!DO_STATIC_MATCHES (lexer, switch_header_pieces, pad_expr))
    return NULL;
  expr = pad_expr[0];
  while (lexer->has_cur && lexer->cur.type != JAST_TOKEN_RBRACE)
    {
      if (lexer->cur.type == JAST_TOKEN_CASE)
        {
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error (lexer, "after 'case' in switch");
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
            }

          /* parse key: require parens around ternary operations */
          JAST_Expr *case_value = parse_expr(lexer, PARSE_EXPR_ALLOW_COMMA);
          if (case_value == NULL)
             goto error_cleanup;

          /* skip colon */
          if (!lexer->has_cur)
            {
              set_premature_eof_parse_error (lexer, "expected colon after case");
              goto error_cleanup;
            }
          if (lexer->cur.type != JAST_TOKEN_COLON)
            {
              set_bad_token_parse_error(lexer, &lexer->cur, "expected colon after case");
              goto error_cleanup;
            }
          switch (jast_lexer_advance (lexer))
            {
              case JAST_LEXER_ADVANCE_OK:
                break;
              case JAST_LEXER_ADVANCE_EOF:
                set_premature_eof_parse_error (lexer, "switch");
                goto error_cleanup;
              case JAST_LEXER_ADVANCE_ERROR:
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
              set_parse_error (lexer, JAST_PARSE_ERROR_SEMANTICS,
                               &lexer->cur.position,
                               "multiple default clauses in switch statement");
              goto error_cleanup;
            }
          has_default = JS_TRUE;
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error (lexer, "after 'default' in switch");
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
            }
          if (lexer->cur.type != JAST_TOKEN_COLON)
            {
              set_bad_token_parse_error(lexer, &lexer->cur, "expected colon after default");
              goto error_cleanup;
            }
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error (lexer, "in switch statement");
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
  size_t rv_size = sizeof(JAST_Switch_Statement) + clauses_size;
  JAST_Switch_Statement *rv = alloc_statement(JAST_STATEMENT_SWITCH, rv_size);
  rv->expr = expr;
  rv->n_clauses = n_clauses;
  rv->clauses = (JAST_Switch_Clause *) (rv + 1);
  memcpy (rv->clauses, clauses, clauses_size);
  return (JAST_Statement *) rv;

error_cleanup:
  jast_expr_free (expr);
  for (size_t k = 0; k < n_clauses; k++)
    switch (clauses[k].clause_type)
      {
      case JAST_SWITCH_CLAUSE_CASE:
        jast_expr_free (clauses[k].info.case_value);
        break;
      case JAST_SWITCH_CLAUSE_STATEMENT:
        jast_statement_free (clauses[k].info.statement);
        break;
      case JAST_SWITCH_CLAUSE_DEFAULT:
        break;
      }
  MAYBE_CLEAR_STACK_STARTED_ARRAY(clauses);
  return NULL;
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
          set_premature_eof_parse_error (lexer, "unexpected EOF in var-decls");
          return NULL;
        case JAST_LEXER_ADVANCE_ERROR:
          return NULL;
        }
    }
  DEFINE_STACK_STARTED_ARRAY(JAST_BindingPattern, vardecls, 16);
  for (;;)
    {
      JAST_BindingPattern binding;
      if (!parse_binding_pattern (lexer, &binding))
        goto error_cleanup;
      if (!lexer->has_cur)
        {
          set_premature_eof_parse_error (lexer, "in variable declaration");
          goto error_cleanup;
        }
      APPEND_TO_STACK_STARTED_ARRAY (JAST_BindingPattern, vardecls, binding);
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
              set_premature_eof_parse_error (lexer, "after variable");
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
            }
        }
      else
        {
          set_bad_token_parse_error (lexer, &lexer->cur, "expected ';' or ',' after variable");
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
      break;
    }

  size_t vd_size = sizeof(JAST_BindingPattern) * n_vardecls;
  size_t rv_size = sizeof(JAST_VariableDeclarations_Statement) + vd_size;
  JAST_VariableDeclarations_Statement *rv = alloc_statement(JAST_STATEMENT_VARIABLE_DECLARATIONS, rv_size);
  rv->type = vardecl_type;
  rv->n_vars = n_vardecls;
  rv->vars = (JAST_BindingPattern*) (rv + 1);
  memcpy(rv->vars, vardecls, vd_size);
  return (JAST_Statement *) rv;

error_cleanup:
  for (size_t i = 0; i < n_vardecls; i++)
    binding_pattern_clear (&vardecls[i]);
  MAYBE_CLEAR_STACK_STARTED_ARRAY(vardecls);
  return NULL;
}

static JAST_Statement *
parse_compound_stmt_without_braces (JAST_Lexer *lexer)
{
  DEFINE_STACK_STARTED_ARRAY(JAST_Statement*, subs, 16);
  while (lexer->has_cur && lexer->cur.type != JAST_TOKEN_RBRACE)
    {
      JAST_Statement *stmt = parse_stmt (lexer);
      if (stmt == NULL)
        goto error_cleanup;
      APPEND_TO_STACK_STARTED_ARRAY(JAST_Statement *, subs, stmt);
    }
  size_t subs_size = sizeof (JAST_Statement *) * n_subs;
  size_t rv_size = sizeof (JAST_Compound_Statement) + subs_size;
  JAST_Compound_Statement *rv = alloc_statement(JAST_STATEMENT_COMPOUND, rv_size);
  rv->n_subs = n_subs;
  rv->subs = (JAST_Statement **) (rv + 1);
  memcpy (rv->subs, subs, subs_size);
  MAYBE_CLEAR_STACK_STARTED_ARRAY(subs);
  return (JAST_Statement *) rv;

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
      set_premature_eof_parse_error (lexer, "in compound statement");
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
      set_premature_eof_parse_error (lexer, "in compound statement");
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
typedef enum
{
  JAST_PRECEDENCE_LEVEL_NONBINDING,   /* , and ; */
  JAST_PRECEDENCE_LEVEL_ASSIGNMENT,   /* = etc */
  JAST_PRECEDENCE_LEVEL_TERNARY,
  JAST_PRECEDENCE_LEVEL_LOGICAL,      /* || and && */
  JAST_PRECEDENCE_LEVEL_BITWISE,      /* | & and ^ */
  JAST_PRECEDENCE_LEVEL_COMPARE,
  JAST_PRECEDENCE_LEVEL_SHIFT,
  JAST_PRECEDENCE_LEVEL_ADD,
  JAST_PRECEDENCE_LEVEL_MULTIPLY,
  JAST_PRECEDENCE_LEVEL_UNARY,
  JAST_PRECEDENCE_LEVEL_DOT,
} JAST_PrecedenceLevel;

typedef enum
{
  JAST_OPERATOR_TAG_PREFIX,
  JAST_OPERATOR_TAG_INFIX,
  JAST_OPERATOR_TAG_POSTFIX,
} JAST_OperatorTag;

typedef struct {
  JS_Boolean is_op;
  JAST_Position position;
  union {
    struct {
      JAST_TokenType token_type;
      JAST_PrecedenceLevel precedence;
      JAST_OperatorTag tag;
    } op;
    JAST_Expr *expr;
  } info;
} ExprOpChainPiece;

static inline JS_Boolean
is_precedence_level_left_associative(JAST_PrecedenceLevel level)
{
  (void) level;
  return JS_TRUE;
}

static JAST_Expr *
create_binary_expr (JAST_Expr *a,
                    JAST_TokenType tt,
                    JAST_Expr *b)
{
  JAST_BinaryOp_Type binop=0;
  switch (tt)
    {
    case JAST_TOKEN_PLUS: binop = JAST_BINARY_OP_ADD; break;
    case JAST_TOKEN_MINUS: binop = JAST_BINARY_OP_SUBTRACT; break;
    case JAST_TOKEN_MULTIPLY: binop = JAST_BINARY_OP_MULTIPLY; break;
    case JAST_TOKEN_DIVIDE: binop = JAST_BINARY_OP_DIVIDE; break;
    case JAST_TOKEN_MOD: binop = JAST_BINARY_OP_MOD; break;
    case JAST_TOKEN_LOGICAL_OR: binop = JAST_BINARY_OP_LOGICAL_OR; break;
    case JAST_TOKEN_LOGICAL_AND: binop = JAST_BINARY_OP_LOGICAL_AND; break;
    case JAST_TOKEN_BITWISE_OR: binop = JAST_BINARY_OP_BITWISE_OR; break;
    case JAST_TOKEN_BITWISE_AND: binop = JAST_BINARY_OP_BITWISE_AND; break;
    case JAST_TOKEN_BITWISE_XOR: binop = JAST_BINARY_OP_BITWISE_XOR; break;

    case JAST_TOKEN_ASSIGN: binop = JAST_BINARY_OP_ASSIGN; break;

    case JAST_TOKEN_PLUS_ASSIGN: binop = JAST_BINARY_OP_ADD_ASSIGN; break;
    case JAST_TOKEN_MINUS_ASSIGN: binop = JAST_BINARY_OP_SUBTRACT_ASSIGN; break;
    case JAST_TOKEN_MULTIPLY_ASSIGN: binop = JAST_BINARY_OP_MULTIPLY_ASSIGN; break;
    case JAST_TOKEN_DIVIDE_ASSIGN: binop = JAST_BINARY_OP_DIVIDE_ASSIGN; break;
    case JAST_TOKEN_MOD_ASSIGN: binop = JAST_BINARY_OP_MOD_ASSIGN; break;
    //case JAST_TOKEN_LOGICAL_OR_ASSIGN: binop = JAST_BINARY_OP_LOGICAL_OR; break;
    //case JAST_TOKEN_LOGICAL_AND_ASSIGN: binop = JAST_BINARY_OP_LOGICAL_AND; break;
    case JAST_TOKEN_BITWISE_OR_ASSIGN: binop = JAST_BINARY_OP_BITWISE_OR_ASSIGN; break;
    case JAST_TOKEN_BITWISE_AND_ASSIGN: binop = JAST_BINARY_OP_BITWISE_AND_ASSIGN; break;
    case JAST_TOKEN_BITWISE_XOR_ASSIGN: binop = JAST_BINARY_OP_BITWISE_XOR_ASSIGN; break;
    case JAST_TOKEN_CMP_EXACT_EQ: binop = JAST_BINARY_OP_CMP_EXACT_EQ; break;
    case JAST_TOKEN_CMP_NUM_EQ: binop = JAST_BINARY_OP_CMP_NUM_EQ; break;
    case JAST_TOKEN_CMP_EXACT_NE: binop = JAST_BINARY_OP_CMP_EXACT_NE; break;
    case JAST_TOKEN_CMP_NUM_NE: binop = JAST_BINARY_OP_CMP_NUM_NE; break;
    case JAST_TOKEN_CMP_LT: binop = JAST_BINARY_OP_CMP_LT; break;
    case JAST_TOKEN_CMP_LE: binop = JAST_BINARY_OP_CMP_LE; break;
    case JAST_TOKEN_CMP_GT: binop = JAST_BINARY_OP_CMP_GT; break;
    case JAST_TOKEN_CMP_GE: binop = JAST_BINARY_OP_CMP_GE; break;
    default:
      assert(JS_FALSE);
      return NULL;
    }
  JAST_BinaryOp_Expr *rv = alloc_expr (JAST_EXPR_BINARY_OP, sizeof (JAST_BinaryOp_Expr));
  rv->op = binop;
  rv->subs[0] = a;
  rv->subs[1] = b;
  return (JAST_Expr *) rv;
}

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
          && pieces[in_at+subsize].info.op.precedence != lowest_prec)
        {
          subsize += 2;
        }
      if (subsize == 1)
        {
          pieces[out_at++] = pieces[in_at++];
        }
      else
        {
          JAST_Expr *expr = parse_normalized_op_chain (subsize, pieces + in_at, err_out);
          if (expr == NULL)
            {
              for (size_t k = 0; k < out_at; k++)
                if (!pieces[k].is_op)
                  jast_expr_free (pieces[k].info.expr);
              for (size_t k = in_at + subsize; k < n_pieces; k++)
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

  n_pieces = out_at;

  /* Evaluate from L-to-R or R-to-L depending on associativity. */
  if (lowest_prec == JAST_PRECEDENCE_LEVEL_TERNARY)
    {
      /* ternary op requires special parsing */
      for (unsigned i = 1; i < n_pieces; i += 2)
        {
          assert (pieces[i].is_op);
          JS_Boolean expect_colon = i / 2 % 2;
          JAST_TokenType expected_tt = expect_colon ? JAST_TOKEN_COLON : JAST_TOKEN_QUESTION_MARK;
          if (pieces[i].info.op.token_type != expected_tt)
            {
              *err_out = new_parse_error (JAST_PARSE_ERROR_SEMANTICS,
                                          &pieces[i].position,
                                          "expected %s got %s in ternary-operator",
                                          jast_token_type_name (expected_tt),
                                          jast_token_type_name (pieces[i].info.op.token_type));
              goto error_cleanup;
            }
        }
      if (n_pieces % 4 != 1)
        {
          *err_out = new_parse_error (JAST_PARSE_ERROR_SEMANTICS,
                                      &pieces[n_pieces-1].position,
                                      "expected : as last ternary operator");
          goto error_cleanup;
        }
      size_t n_exprs = (n_pieces + 1) / 2;
      size_t rv_size = sizeof (JAST_Cond_Expr)
                     + sizeof (JAST_Expr *) * n_exprs;
      JAST_Cond_Expr *rv = alloc_expr (JAST_EXPR_COND, rv_size);
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

error_cleanup:
  for (size_t k = 0; k < out_at; k++)
    if (!pieces[k].is_op)
      jast_expr_free (pieces[k].info.expr);
  for (size_t k = in_at; k < n_pieces; k++)
    if (!pieces[k].is_op)
      jast_expr_free (pieces[k].info.expr);
  return NULL;
}

static JS_Boolean
is_expression_terminator_token_type (JAST_TokenType type, ParseExprFlags flags)
{
  switch (type)
    {
    case JAST_TOKEN_RBRACE:
    case JAST_TOKEN_RBRACKET:
    case JAST_TOKEN_RPAREN:
    case JAST_TOKEN_SEMICOLON:
      return JS_TRUE;
    case JAST_TOKEN_COMMA:
      return (flags & PARSE_EXPR_ALLOW_COMMA) != PARSE_EXPR_ALLOW_COMMA;
    case JAST_TOKEN_QUESTION_MARK:
    case JAST_TOKEN_COLON:
      return (flags & PARSE_EXPR_ALLOW_TERNARY) != PARSE_EXPR_ALLOW_TERNARY;
    default:
      return JS_FALSE;
    }
}

static JS_Boolean
is_normal_operator (JAST_TokenType type,
                    JAST_PrecedenceLevel *prec_out)
{
  switch (type)
    {
    case JAST_TOKEN_COMMA:
      *prec_out = JAST_PRECEDENCE_LEVEL_NONBINDING;
      return JS_TRUE;

    case JAST_TOKEN_PLUS:
    case JAST_TOKEN_MINUS:
      *prec_out = JAST_PRECEDENCE_LEVEL_ADD;
      return JS_TRUE;

    case JAST_TOKEN_MULTIPLY:
    case JAST_TOKEN_DIVIDE:
    case JAST_TOKEN_MOD:
      *prec_out = JAST_PRECEDENCE_LEVEL_MULTIPLY;
      return JS_TRUE;

    case JAST_TOKEN_LOGICAL_NOT:
    case JAST_TOKEN_BITWISE_NOT:
    case JAST_TOKEN_PLUS_PLUS:
    case JAST_TOKEN_MINUS_MINUS:
      *prec_out = JAST_PRECEDENCE_LEVEL_UNARY;
      return JS_TRUE;

    case JAST_TOKEN_BITWISE_AND:
    case JAST_TOKEN_BITWISE_OR:
    case JAST_TOKEN_BITWISE_XOR:
      *prec_out = JAST_PRECEDENCE_LEVEL_BITWISE;
      return JS_TRUE;

    case JAST_TOKEN_LOGICAL_AND:
    case JAST_TOKEN_LOGICAL_OR:
      *prec_out = JAST_PRECEDENCE_LEVEL_LOGICAL;
      return JS_TRUE;

    case JAST_TOKEN_COLON:
    case JAST_TOKEN_QUESTION_MARK:
      *prec_out = JAST_PRECEDENCE_LEVEL_TERNARY;
      return JS_TRUE;

    case JAST_TOKEN_DOT:
      *prec_out = JAST_PRECEDENCE_LEVEL_DOT;
      return JS_TRUE;

    case JAST_TOKEN_SHIFT_LEFT:
    case JAST_TOKEN_SHIFT_RIGHT:
    case JAST_TOKEN_SHIFT_UNSIGNED_RIGHT:
      *prec_out = JAST_PRECEDENCE_LEVEL_SHIFT;
      return JS_TRUE;

    case JAST_TOKEN_ASSIGN:
    case JAST_TOKEN_PLUS_ASSIGN:
    case JAST_TOKEN_MINUS_ASSIGN:
    case JAST_TOKEN_MULTIPLY_ASSIGN:
    case JAST_TOKEN_DIVIDE_ASSIGN:
    case JAST_TOKEN_MOD_ASSIGN:
    case JAST_TOKEN_SHIFT_LEFT_ASSIGN:
    case JAST_TOKEN_SHIFT_RIGHT_ASSIGN:
    case JAST_TOKEN_SHIFT_UNSIGNED_RIGHT_ASSIGN:
    case JAST_TOKEN_BITWISE_AND_ASSIGN:
    case JAST_TOKEN_BITWISE_OR_ASSIGN:
    case JAST_TOKEN_BITWISE_XOR_ASSIGN:
      *prec_out = JAST_PRECEDENCE_LEVEL_ASSIGNMENT;
      return JS_TRUE;

    case JAST_TOKEN_CMP_EXACT_EQ:
    case JAST_TOKEN_CMP_NUM_EQ:
    case JAST_TOKEN_CMP_EXACT_NE:
    case JAST_TOKEN_CMP_NUM_NE:
    case JAST_TOKEN_CMP_LT:
    case JAST_TOKEN_CMP_LE:
    case JAST_TOKEN_CMP_GT:
    case JAST_TOKEN_CMP_GE:
      *prec_out = JAST_PRECEDENCE_LEVEL_COMPARE;
      return JS_TRUE;
    default:
      return JS_FALSE;
    }
}

static JAST_Expr *
parse_array_literal_expr (JAST_Lexer *lexer)
{
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_ERROR:
      return NULL;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "after '['");
      break;
    }
  DEFINE_STACK_STARTED_ARRAY(JAST_Expr *, exprs, 32);
  while (lexer->cur.type != JAST_TOKEN_RBRACKET)
    {
      JAST_Expr *e = parse_expr (lexer, PARSE_EXPR_ALLOW_TERNARY);
      if (e == NULL)
        goto error_cleanup;
      APPEND_TO_STACK_STARTED_ARRAY (JAST_Expr *, exprs, e);
      if (!lexer->has_cur)
        {
          set_premature_eof_parse_error (lexer, "in array literal");
          goto error_cleanup;
        }
      if (lexer->cur.type == JAST_TOKEN_COMMA)
        {
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error(lexer, "in array literal");
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
            }
        }
    }
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_ERROR:
      goto error_cleanup;
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_EOF:
      break;
    }
  size_t sub_size = sizeof (JAST_Expr *) * n_exprs;
  size_t rv_size = sizeof (JAST_ArrayValue_Expr) + sub_size;
  JAST_ArrayValue_Expr *rv = alloc_expr (JAST_EXPR_ARRAY_VALUE, rv_size);
  rv->n_values = n_exprs;
  rv->values = (JAST_Expr **) (rv + 1);
  memcpy (rv->values, exprs, sub_size);
  MAYBE_CLEAR_STACK_STARTED_ARRAY (exprs);
  return (JAST_Expr *) rv;

error_cleanup:
  for (size_t k = 0; k < n_exprs; k++)
    jast_expr_free (exprs[k]);
  MAYBE_CLEAR_STACK_STARTED_ARRAY (exprs);
  return NULL;
}

static JAST_Expr *
parse_object_literal_expr (JAST_Lexer *lexer)
{
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_ERROR:
      return NULL;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "after '['");
      break;
    }
  DEFINE_STACK_STARTED_ARRAY(JAST_ObjectFieldValue, fields, 32);
  while (lexer->cur.type != JAST_TOKEN_RBRACE)
    {
      JS_String *key = NULL;
      JAST_Expr *computed_key = NULL;
      JS_Boolean do_advance = JS_FALSE;
      switch (convert_reserved_word_to_bareword (lexer->cur.type))
        {
          case JAST_TOKEN_NUMBER:
            key = number_literal_to_key_string (lexer, &lexer->cur);
            do_advance = JS_TRUE;
            break;
          case JAST_TOKEN_BAREWORD:
            key = bareword_token_to_string (lexer, &lexer->cur);
            do_advance = JS_TRUE;
            break;
          case JAST_TOKEN_STRING:
            key = string_literal_to_string (lexer, &lexer->cur);
            do_advance = JS_TRUE;
            break;
          case JAST_TOKEN_LBRACKET:   // computed key
            {
              static struct MatcherPiece computed_key_pieces[] = {
                MATCHER_PIECE_BY_TOKEN_TYPE(LBRACKET),
                MATCHER_PIECE_VIRTUAL(matcher_virtual_parse_expr),
                MATCHER_PIECE_BY_TOKEN_TYPE(RBRACKET),
              };
              void *pad_expr[1];
              if (!DO_STATIC_MATCHES (lexer, computed_key_pieces, pad_expr))
                goto error_cleanup;
              computed_key = pad_expr[0];
            }
            break;
          default:
            set_bad_token_parse_error (lexer, &lexer->cur, "expected key in object-literal");
            goto error_cleanup;
        }
      if (do_advance)
        switch (jast_lexer_advance (lexer))
          {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error (lexer, "after key in object-literal");
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
          }
      if (lexer->cur.type != JAST_TOKEN_COLON)
        {
          set_expected_token_parse_error (lexer, JAST_TOKEN_COLON, "in object-literal");
          goto error_cleanup;
        }
      switch (jast_lexer_advance (lexer))
        {
          case JAST_LEXER_ADVANCE_OK:
            break;
          case JAST_LEXER_ADVANCE_EOF:
            set_premature_eof_parse_error (lexer, "after key: in object-literal");
            /* fallthrough */
          case JAST_LEXER_ADVANCE_ERROR:
            js_string_maybe_unref (key);
            if (computed_key)
              jast_expr_free (computed_key);
            goto error_cleanup;
        }
      JAST_Expr *value = parse_expr (lexer, PARSE_EXPR_ALLOW_TERNARY);
      if (value == NULL)
        {
          js_string_maybe_unref (key);
          if (computed_key)
            jast_expr_free (computed_key);
          goto error_cleanup;
        }
      JAST_ObjectFieldValue field = {
        key,
        computed_key,
        value
      };
      APPEND_TO_STACK_STARTED_ARRAY (JAST_ObjectFieldValue, fields, field);
      if (lexer->cur.type == JAST_TOKEN_COMMA)
        {
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error(lexer, "in object literal");
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
            }
        }
      else if (lexer->cur.type != JAST_TOKEN_RBRACE)
        {
          set_bad_token_parse_error (lexer, &lexer->cur, "after expression in object-literal");
          goto error_cleanup;
        }
    }
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_ERROR:
      goto error_cleanup;
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_EOF:
      break;
    }
  size_t sub_size = sizeof (JAST_ObjectFieldValue) * n_fields;
  size_t rv_size = sizeof (JAST_ObjectValue_Expr) + sub_size;
  JAST_ObjectValue_Expr *rv = alloc_expr (JAST_EXPR_OBJECT_VALUE, rv_size);
  rv->n_fields = n_fields;
  rv->fields = (JAST_ObjectFieldValue *) (rv + 1);
  memcpy (rv->fields, fields, sub_size);
  MAYBE_CLEAR_STACK_STARTED_ARRAY (fields);
  return (JAST_Expr *) rv;

error_cleanup:
  for (size_t k = 0; k < n_fields; k++)
    {
      js_string_maybe_unref (fields[k].key);
      if (fields[k].computed_key)
        jast_expr_free (fields[k].computed_key);
      jast_expr_free (fields[k].value);
    }
  MAYBE_CLEAR_STACK_STARTED_ARRAY (fields);
  return NULL;
}

/* Takes ownership of function_expr, even on error. */
static JAST_Expr *
parse_invoke_expr (JAST_Expr *function_expr,
                   JAST_Lexer *lexer)
{
  assert(lexer->cur.type == JAST_TOKEN_LPAREN);
  switch (jast_lexer_advance(lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_ERROR:
      jast_expr_free (function_expr);
      return NULL;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "parsing function call");
      jast_expr_free (function_expr);
      return NULL;
    }

  DEFINE_STACK_STARTED_ARRAY(JAST_Expr *, exprs, 8);
  while (lexer->has_cur && lexer->cur.type != JAST_TOKEN_RPAREN)
    {
      JAST_Expr *e = parse_expr (lexer, PARSE_EXPR_ALLOW_TERNARY);
      if (e == NULL)
        goto error_cleanup;
      APPEND_TO_STACK_STARTED_ARRAY (JAST_Expr *, exprs, e);
      if (lexer->has_cur && lexer->cur.type == JAST_TOKEN_COMMA)
        {
          switch (jast_lexer_advance(lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error (lexer, "parsing function call");
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
            }

          if (lexer->cur.type == JAST_TOKEN_RPAREN)
            {
              set_parse_error (lexer, JAST_PARSE_ERROR_BAD_TOKEN,
                               &lexer->cur.position,
                               "comma not allowed at end of function-call argument list");
              goto error_cleanup;
            }
        }
    }
  if (!lexer->has_cur)
    {
      set_premature_eof_parse_error (lexer, "expected ) at end of function-call");
      goto error_cleanup;
    }
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_ERROR:
      goto error_cleanup;
    case JAST_LEXER_ADVANCE_EOF:
      break;
    }

  size_t subsize = sizeof (JAST_Expr *) * n_exprs;
  size_t rv_size = sizeof (JAST_Invoke_Expr) + subsize;
  JAST_Invoke_Expr *rv = alloc_expr (JAST_EXPR_INVOKE, rv_size);
  rv->function = function_expr;
  rv->n_args = n_exprs;
  rv->args = (JAST_Expr **) (rv + 1);
  memcpy (rv->args, exprs, subsize);
  MAYBE_CLEAR_STACK_STARTED_ARRAY (exprs);
  return (JAST_Expr *) rv;

error_cleanup:
  jast_expr_free (function_expr);
  for (size_t k = 0; k < n_exprs; k++)
    jast_expr_free (exprs[k]);
  MAYBE_CLEAR_STACK_STARTED_ARRAY (exprs);
  return NULL;
}

static JAST_Expr *
parse_function_expression (JAST_Lexer *lexer)
{
  assert(lexer->cur.type == JAST_TOKEN_FUNCTION);
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_ERROR:
      return NULL;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "after 'function'");
      return NULL;
    }
  JS_Boolean is_generator = JS_FALSE;

  if (lexer->cur.type == JAST_TOKEN_MULTIPLY)
    {
      // Generator function
      is_generator = JS_TRUE;
      switch (jast_lexer_advance (lexer))
        {
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_ERROR:
          return NULL;
        case JAST_LEXER_ADVANCE_EOF:
          set_premature_eof_parse_error (lexer, "after 'function*'");
          return NULL;
        }
    }

  JS_String *opt_name = NULL;
  if (lexer->cur.type == JAST_TOKEN_BAREWORD)
    {
      opt_name = bareword_token_to_string (lexer, &lexer->cur);
      switch (jast_lexer_advance (lexer))
        {
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_EOF:
          set_premature_eof_parse_error (lexer, "after function name");
          return NULL;
        case JAST_LEXER_ADVANCE_ERROR:
          return NULL;
        }
    }
  if (lexer->cur.type != JAST_TOKEN_LPAREN)
    {
      set_expected_token_parse_error (lexer, JAST_TOKEN_LPAREN, "after function");
      return NULL;
    }

  // Scan formal parameter names.
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "after function(");
      if (opt_name)
        js_string_unref (opt_name);
      return NULL;
    case JAST_LEXER_ADVANCE_ERROR:
      if (opt_name)
        js_string_unref (opt_name);
      return NULL;
    }
  DEFINE_STACK_STARTED_ARRAY(JAST_FormalParam, args, 8);
  while (lexer->cur.type != JAST_TOKEN_RPAREN)
    {
      if (lexer->cur.type == JAST_TOKEN_BAREWORD)
        {
          JS_String *bw = bareword_token_to_string (lexer, &lexer->cur);
          JAST_FormalParam fp = { bw };
          APPEND_TO_STACK_STARTED_ARRAY(JAST_FormalParam, args, fp);
        }
      else
        {
          set_expected_token_parse_error (lexer, JAST_TOKEN_BAREWORD, "in function parameter list");
          goto arg_error_cleanup;
        }
      switch (jast_lexer_advance (lexer))
        {
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_EOF:
          set_premature_eof_parse_error (lexer, "after formal-parameters");
          goto arg_error_cleanup;
        case JAST_LEXER_ADVANCE_ERROR:
          goto arg_error_cleanup;
        }

      if (lexer->cur.type == JAST_TOKEN_COMMA)
        {
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error (lexer, "in formal-parameters");
              goto arg_error_cleanup;
            case JAST_LEXER_ADVANCE_ERROR:
              goto arg_error_cleanup;
            }
        }
      else if (lexer->cur.type != JAST_TOKEN_RPAREN)
        {
          set_expected_token_parse_error (lexer, JAST_TOKEN_RPAREN, "in function parameter list");
          goto arg_error_cleanup;
        }
    }
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "after formal-parameters");
      goto arg_error_cleanup;
    case JAST_LEXER_ADVANCE_ERROR:
      goto arg_error_cleanup;
    }
  JAST_Statement *stmt = parse_compound_stmt (lexer);
  size_t arg_size = sizeof (JAST_FormalParam) * n_args;
  size_t rv_size = sizeof (JAST_FunctionValue_Expr) + arg_size;
  JAST_FunctionValue_Expr *fv = alloc_expr (JAST_EXPR_FUNCTION_VALUE, rv_size);
  fv->opt_name = opt_name;
  fv->n_args = n_args;
  fv->args = (JAST_FormalParam *) (fv + 1);
  memcpy (fv->args, args, arg_size);
  fv->body = stmt;
  MAYBE_CLEAR_STACK_STARTED_ARRAY (args);
  return (JAST_Expr *) fv;


arg_error_cleanup:
  if (opt_name)
    js_string_unref (opt_name);
  for (size_t k = 0; k < n_args; k++)
    js_string_unref (args[k].name);
  MAYBE_CLEAR_STACK_STARTED_ARRAY (args);
  return NULL;
}

static JAST_Expr *
parse_opfree_expr (JAST_Lexer *lexer)
{
  JAST_Expr *expr = NULL;
  switch (lexer->cur.type)
    {
    case JAST_TOKEN_FUNCTION:
      expr = parse_function_expression (lexer);
      if (expr == NULL)
        return NULL;
      break;

    case JAST_TOKEN_LBRACKET:
      expr = parse_array_literal_expr (lexer);
      if (expr == NULL)
        return NULL;
      break;

    case JAST_TOKEN_LBRACE:
      expr = parse_object_literal_expr (lexer);
      if (expr == NULL)
        return NULL;
      break;

    case JAST_TOKEN_STRING:
      {
        JS_String *value = string_literal_to_string (lexer, &lexer->cur);
        JAST_StringValue_Expr *rv = alloc_expr (JAST_EXPR_STRING_VALUE, sizeof (JAST_StringValue_Expr));
        rv->value = value;
        expr = (JAST_Expr *) rv;
      }
      switch (jast_lexer_advance (lexer))
        {
        case JAST_LEXER_ADVANCE_ERROR:
          jast_expr_free (expr);
          return NULL;
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_EOF:
          return expr;
        }
      break;

    case JAST_TOKEN_LPAREN:
      switch (jast_lexer_advance (lexer))
        {
        case JAST_LEXER_ADVANCE_ERROR:
          jast_expr_free (expr);
          return NULL;
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_EOF:
          set_premature_eof_parse_error (lexer, "after (");
          return NULL;
        }
      expr = parse_expr (lexer, PARSE_EXPR_ALLOW_COMMA|PARSE_EXPR_ALLOW_TERNARY);
      if (expr == NULL)
        return NULL;
      if (!lexer->has_cur)
        {
          jast_expr_free (expr);
          set_premature_eof_parse_error (lexer, "expected )");
          return NULL;
        }
      if (lexer->cur.type != JAST_TOKEN_RPAREN)
        {
          jast_expr_free (expr);
          set_expected_token_parse_error (lexer, JAST_TOKEN_RPAREN, "parenthesized expression");
          return NULL;
        }
      switch (jast_lexer_advance (lexer))
        {
        case JAST_LEXER_ADVANCE_ERROR:
          jast_expr_free (expr);
          return NULL;
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_EOF:
          return expr;
        }
      break;

    case JAST_TOKEN_NUMBER:
      {
        char *end;
        const char *str = lexer->data + lexer->cur.offset;
        double value = strtod (str, &end);
        if (end != str + lexer->cur.length)
          {
            set_bad_character_parse_error(lexer, end);
            return NULL;
          }
        JAST_NumberValue_Expr *rv = alloc_expr (JAST_EXPR_NUMBER_VALUE, sizeof (JAST_NumberValue_Expr));
        rv->value = value;
        expr = (JAST_Expr *) rv;
      }
      switch (jast_lexer_advance (lexer))
        {
        case JAST_LEXER_ADVANCE_ERROR:
          jast_expr_free (expr);
          return NULL;
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_EOF:
          return expr;
        }
      break;

    case JAST_TOKEN_BAREWORD:
      {
        JS_String *bw = bareword_token_to_string (lexer, &lexer->cur);
        JAST_Identifier_Expr *id_expr = alloc_expr(JAST_EXPR_IDENTIFIER, sizeof(JAST_Identifier_Expr));
        id_expr->symbol = bw;
        expr = (JAST_Expr *) id_expr;
      }
      switch (jast_lexer_advance (lexer))
        {
        case JAST_LEXER_ADVANCE_ERROR:
          jast_expr_free (expr);
          return NULL;
        case JAST_LEXER_ADVANCE_OK:
          break;
        case JAST_LEXER_ADVANCE_EOF:
          return expr;
        }
      break;

    default:
      set_bad_token_parse_error (lexer, &lexer->cur, "expected expression");
      return NULL;
    }
  assert (expr != NULL);
  while (lexer->has_cur)
    {
      if (lexer->cur.type == JAST_TOKEN_LPAREN)
        {
          /* invocation expression */
          expr = parse_invoke_expr (expr, lexer);
          if (expr == NULL)
            return NULL;
        }
      else if (lexer->cur.type == JAST_TOKEN_LBRACKET)
        {
          /* index expression */
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_ERROR:
              jast_expr_free (expr);
              return NULL;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error (lexer, "end-of-file after [");
              jast_expr_free (expr);
              return NULL;
            }
          JAST_Expr *index_expr = parse_expr (lexer, PARSE_EXPR_ALLOW_COMMA|PARSE_EXPR_ALLOW_TERNARY);
          if (index_expr == NULL)
            {
              jast_expr_free (expr);
              return NULL;
            }
          if (!lexer->has_cur)
            {
              set_premature_eof_parse_error (lexer, "expecting ]");
              jast_expr_free (expr);
              jast_expr_free (index_expr);
              return NULL;
            }
          if (lexer->cur.type != JAST_TOKEN_RBRACKET)
            {
              set_expected_token_parse_error (lexer, JAST_TOKEN_RBRACKET, "in array index");
              jast_expr_free (expr);
              jast_expr_free (index_expr);
              return NULL;
            }
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              break;
            case JAST_LEXER_ADVANCE_ERROR:
              jast_expr_free (expr);
              jast_expr_free (index_expr);
              return NULL;
            }
          JAST_Index_Expr *ie = alloc_expr(JAST_EXPR_INDEX, sizeof (JAST_Index_Expr));
          ie->container = expr;
          ie->index = index_expr;
          expr = (JAST_Expr *) ie;
        }
      else if (lexer->cur.type == JAST_TOKEN_DOT)
        {
          /* dot expression */
          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error (lexer, "after .");
              jast_expr_free (expr);
              return NULL;
            case JAST_LEXER_ADVANCE_ERROR:
              jast_expr_free (expr);
              return NULL;
            }
          if (lexer->cur.type != JAST_TOKEN_BAREWORD)
            {
              set_bad_token_parse_error (lexer, &lexer->cur, "expected bareword after .");
              jast_expr_free (expr);
              return NULL;
            }
          JAST_Dot_Expr *de = alloc_expr (JAST_EXPR_DOT, sizeof (JAST_Dot_Expr));
          de->container = expr;
          de->member_name = bareword_token_to_string (lexer, &lexer->cur);
          expr = (JAST_Expr *) de;

          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error (lexer, "after .");
              jast_expr_free (expr);
              return NULL;
            case JAST_LEXER_ADVANCE_ERROR:
              jast_expr_free (expr);
              return NULL;
            }
        }
      else
        return expr;
    }
  return expr;
}

static JS_Boolean
token_type_is_prefix_op (JAST_TokenType type)
{
  switch (type)
    {
    case JAST_TOKEN_PLUS_PLUS:
    case JAST_TOKEN_MINUS_MINUS:
    case JAST_TOKEN_LOGICAL_NOT:
    case JAST_TOKEN_BITWISE_NOT:
    case JAST_TOKEN_MINUS:
    case JAST_TOKEN_PLUS:
      return JS_TRUE;
    default:
      return JS_FALSE;
    }
}
static JS_Boolean
token_type_is_postfix_op (JAST_TokenType type)
{
  switch (type)
    {
    case JAST_TOKEN_PLUS_PLUS:
    case JAST_TOKEN_MINUS_MINUS:
      return JS_TRUE;
    default:
      return JS_FALSE;
    }
}

static JAST_UnaryOp_Type token_type_to_prefix_op (JAST_TokenType tt)
{
  switch (tt)
    {
    case JAST_TOKEN_PLUS_PLUS: return JAST_UNARY_OP_PRE_INCR;
    case JAST_TOKEN_MINUS_MINUS: return JAST_UNARY_OP_PRE_DECR;
    case JAST_TOKEN_LOGICAL_NOT: return JAST_UNARY_OP_LOGICAL_NOT;
    case JAST_TOKEN_BITWISE_NOT: return JAST_UNARY_OP_BITWISE_NOT;
    case JAST_TOKEN_MINUS: return JAST_UNARY_OP_NEGATE;
    case JAST_TOKEN_PLUS: return JAST_UNARY_OP_PLUS;
    default: assert(0); return 0;
    }
}
static inline JS_Boolean is_op_token_infix (JAST_TokenType tt)
{
  return tt != JAST_TOKEN_PLUS_PLUS && tt != JAST_TOKEN_MINUS_MINUS;
}


static JAST_UnaryOp_Type token_type_to_postfix_op (JAST_TokenType tt)
{
  switch (tt)
    {
    case JAST_TOKEN_PLUS_PLUS: return JAST_UNARY_OP_POST_INCR;
    case JAST_TOKEN_MINUS_MINUS: return JAST_UNARY_OP_POST_DECR;
    default: assert(0); return 0;
    }
}

static JAST_Expr *
create_unary (JAST_UnaryOp_Type type, JAST_Expr *expr)
{
  JAST_UnaryOp_Expr *rv = alloc_expr (JAST_EXPR_UNARY_OP, sizeof (JAST_UnaryOp_Expr));
  rv->op = type;
  rv->sub = expr;
  return (JAST_Expr *) rv;
}

static JAST_Expr *
parse_expr(JAST_Lexer *lexer, ParseExprFlags flags)
{
  DEFINE_STACK_STARTED_ARRAY(ExprOpChainPiece, pieces, 16);

  while (lexer->has_cur
      && !is_expression_terminator_token_type (lexer->cur.type, flags))
    {
      ExprOpChainPiece piece;
      JAST_PrecedenceLevel prec;
      piece.position = lexer->cur.position;
      if (is_normal_operator (lexer->cur.type, &prec))
        {
          piece.is_op = JS_TRUE;
          piece.info.op.token_type = lexer->cur.type;
          piece.info.op.precedence = prec;

          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_EOF:
              break;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_in_pieces_cleanup;
            }
        }
      else
        {
          JAST_Expr *expr = parse_opfree_expr (lexer);
          if (expr == NULL)
            goto error_in_pieces_cleanup;
          piece.is_op = JS_FALSE;
          piece.info.expr = expr;
        }
      APPEND_TO_STACK_STARTED_ARRAY(ExprOpChainPiece, pieces, piece);
    }


  if (0)
    {
      error_in_pieces_cleanup:
        for (size_t i = 0; i < n_pieces; i++)
          if (!pieces[i].is_op)
            jast_expr_free (pieces[i].info.expr);
        if (pieces != pieces_stack)
          free (pieces);
        return NULL;
    }


  /* Handle initial prefix operators. */
  unsigned piece_i = 0;
  while (piece_i < n_pieces && pieces[piece_i].is_op)
    {
      JAST_TokenType tt = pieces[piece_i].info.op.token_type;
      if (!token_type_is_prefix_op (tt))
        {
          set_parse_error (lexer, JAST_PARSE_ERROR_BAD_TOKEN,
                           &lexer->position, "non-prefix operator %s",
                           jast_token_type_name (tt));
          goto free_pieces_error;
        }
      pieces[piece_i].info.op.tag = JAST_OPERATOR_TAG_PREFIX;
    }
  if (piece_i == n_pieces)
    {
      set_parse_error (lexer, JAST_PARSE_ERROR_BAD_TOKEN,
                       &lexer->position, "missing expression");
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
          for (size_t rem_i = piece_i + 1; rem_i < n_pieces; rem_i++)
            {
              JAST_TokenType tt = pieces[rem_i].info.op.token_type;
              if (!token_type_is_postfix_op (tt))
                {
                  set_parse_error (lexer, JAST_PARSE_ERROR_BAD_TOKEN,
                                   &lexer->position, "non-postfix operator %s",
                                   jast_token_type_name (tt));
                  goto free_pieces_error;
                }
              pieces[rem_i].info.op.tag = JAST_OPERATOR_TAG_POSTFIX;
            }
        }
      else 
        {
          /* see if we can assign prefix/infix/postfix to the op list */
          unsigned n_ops = next_piece_i - piece_i - 1;
          if (n_ops == 0)
            {
              /* error: two adjacent expressions. */
              set_parse_error (lexer, JAST_PARSE_ERROR_BAD_TOKEN,
                               &lexer->position, "missing operator");
              goto free_pieces_error;
            }
          else
            {
              unsigned max_prefix_ops = 0;
              unsigned max_postfix_ops = 0;
              while (max_postfix_ops < n_ops - 1
                 && token_type_is_postfix_op (pieces[piece_i + 1 + max_postfix_ops].info.op.token_type))
                max_postfix_ops++;
              while (max_prefix_ops < n_ops - 1
                 && token_type_is_prefix_op (pieces[next_piece_i - 1 - max_prefix_ops].info.op.token_type))
                max_prefix_ops++;
              if (max_prefix_ops + max_postfix_ops + 1 < n_ops)
                {
                  /* at pieces[piece_i+1+max_prefix_ops] */
                  set_parse_error (lexer, JAST_PARSE_ERROR_BAD_TOKEN,
                                   &lexer->position, "unexpected operator");
                  goto free_pieces_error;
                }
              int n_next_prefix_ops, n_postfix_ops;
              for (n_next_prefix_ops = max_prefix_ops;
                   n_next_prefix_ops >= 0;
                   n_next_prefix_ops--)
                {
                  n_postfix_ops = n_ops - 1 - n_next_prefix_ops;
                  if (n_postfix_ops < 0 || n_postfix_ops > (int) max_postfix_ops)
                    continue;
                  if (!is_op_token_infix (pieces[next_piece_i - n_postfix_ops - 1].info.op.token_type))
                    continue;
                  break;
                }
              if (n_next_prefix_ops < 0)
                {
                  set_parse_error (lexer, JAST_PARSE_ERROR_BAD_TOKEN,
                                   &lexer->position, "bad operator sequence");
                  goto free_pieces_error;
                }
              for (int i = 0; i < n_postfix_ops; i++)
                pieces[piece_i + 1 + i].info.op.tag = JAST_OPERATOR_TAG_POSTFIX;
              pieces[piece_i + 1 + n_postfix_ops].info.op.tag = JAST_OPERATOR_TAG_INFIX;
              for (int i = 0; i < n_next_prefix_ops; i++)
                pieces[piece_i + 1 + n_postfix_ops + 1].info.op.tag = JAST_OPERATOR_TAG_PREFIX;
            }
        }
      piece_i = next_piece_i;
    }

#if 0
    for (size_t i = 0; i < n_pieces; i++)
      if (pieces[i].is_op)
        printf("pieces[%u] = (op) %s tag=%u\n", (unsigned)i, jast_token_type_name(pieces[i].info.op.token_type), pieces[i].info.op.tag);
      else
        printf("pieces[%u] = (expr) %s\n", (unsigned)i, jast_expr_type_name(pieces[i].info.expr->type));
#endif

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
          set_parse_error (lexer, JAST_PARSE_ERROR_BAD_TOKEN,
                           &lexer->position, "expected expression");
          n_pieces = out_at;
          goto free_pieces_error;
        }
      JAST_Expr *expr = pieces[next_expr].info.expr;
      size_t n_prefix_ops = 0;
      ExprOpChainPiece *p = pieces + next_expr - n_prefix_ops - 1;
      while (in_at + 1 < next_expr - n_prefix_ops
          && p->info.op.tag == JAST_OPERATOR_TAG_PREFIX)
        {
          JAST_TokenType tt = p->info.op.token_type;
          JAST_UnaryOp_Type uot = token_type_to_prefix_op (tt);
          expr = create_unary (uot, expr);
          n_prefix_ops++;
          p--;
        }
      p = pieces + next_expr + 1;
      unsigned n_postfix_ops = 0;
      while (next_expr + n_postfix_ops + 1 < n_pieces
          && p->info.op.tag == JAST_OPERATOR_TAG_POSTFIX)
        {
          JAST_TokenType tt = p->info.op.token_type;
          JAST_UnaryOp_Type uot = token_type_to_postfix_op (tt);
          expr = create_unary (uot, expr);
          n_postfix_ops++;
          p++;
        }
      while (in_at < next_expr - n_prefix_ops)
        pieces[out_at++] = pieces[in_at++];
      pieces[out_at].is_op = JS_FALSE;
      pieces[out_at].info.expr = expr;
      out_at++;
      in_at = next_expr + n_postfix_ops + 1;
    }
  n_pieces = out_at;

  /* Assert that the new state of pieces
   * is EXPR OP EXPR OP ... OP EXPR
   *
   * Should be guaranteed by above code, but
   * verify for sanity's sake.
   */
  assert(n_pieces % 2 == 1);
  for (size_t j = 0; j < n_pieces; j++)
    assert(pieces[j].is_op == (j % 2 == 1));

  /* Find the lowest prec level in use,
   * and recurse on the remaining expressions.
   * Then make binary expressions l-r or r-l 
   */
  JAST_ParseError *op_chain_err = NULL;
  JAST_Expr *expr = parse_normalized_op_chain(n_pieces, pieces, &op_chain_err);
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

static inline void
clear_expr_array (size_t N, JAST_Expr **arr)
{
  for (size_t i = 0; i < N; i++)
    jast_expr_free (arr[i]);
}

void
jast_expr_free (JAST_Expr *expr)
{
  switch (expr->type)
    {
      case JAST_EXPR_UNARY_OP:
        jast_expr_free (expr->unary_op_expr.sub);
        break;
      case JAST_EXPR_BINARY_OP:
        jast_expr_free (expr->binary_op_expr.subs[0]);
        jast_expr_free (expr->binary_op_expr.subs[1]);
        break;
      case JAST_EXPR_COND:
        clear_expr_array (expr->cond_expr.n_terms, expr->cond_expr.terms);
        break;
      case JAST_EXPR_DOT:
        jast_expr_free (expr->dot_expr.container);
        js_string_unref (expr->dot_expr.member_name);
        break;
      case JAST_EXPR_INDEX:
        jast_expr_free (expr->index_expr.container);
        jast_expr_free (expr->index_expr.index);
        break;
      case JAST_EXPR_INVOKE:
        jast_expr_free (expr->invoke_expr.function);
        clear_expr_array (expr->invoke_expr.n_args, expr->invoke_expr.args);
        break;
      case JAST_EXPR_FUNCTION_VALUE:
        {
          js_string_maybe_unref (expr->function_value_expr.opt_name);
          size_t n_args = expr->function_value_expr.n_args;
          JAST_FormalParam *args = expr->function_value_expr.args;
          for (size_t i = 0; i < n_args; i++)
            {
              js_string_unref (args[i].name);
            }
          jast_statement_free (expr->function_value_expr.body);
        }
        break;
      case JAST_EXPR_OBJECT_VALUE:
        {
          JAST_ObjectFieldValue *fields = expr->object_value_expr.fields;
          size_t n_fields = expr->object_value_expr.n_fields;
          for (size_t i = 0; i < n_fields; i++)
            {
              js_string_maybe_unref (fields[i].key);
              if (fields[i].computed_key != NULL)
                jast_expr_free (fields[i].computed_key);
              jast_expr_free (fields[i].value);
            }
        }
        break;
      case JAST_EXPR_ARRAY_VALUE:
        clear_expr_array (expr->array_value_expr.n_values, expr->array_value_expr.values);
        break;
      case JAST_EXPR_STRING_VALUE:
        js_string_unref (expr->string_value_expr.value);
        break;
      case JAST_EXPR_NUMBER_VALUE:
        break;
      case JAST_EXPR_BOOLEAN_VALUE:
        break;
      case JAST_EXPR_UNDEFINED_VALUE:
        break;
      case JAST_EXPR_NULL_VALUE:
        break;
      case JAST_EXPR_IDENTIFIER:
        js_string_unref (expr->identifier_expr.symbol);
        break;
    }
  free (expr);
}

static JAST_Statement *
jast_statement_from_expr (JAST_Expr *expr)
{
  JAST_Expression_Statement *es = alloc_statement (JAST_STATEMENT_EXPRESSION, sizeof (JAST_Expression_Statement));
  es->expr = expr;
  return (JAST_Statement *) es;
}

static JAST_Statement *
parse_stmt (JAST_Lexer *lexer)
{
  if (!lexer->has_cur)
    return NULL;

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
        return parse_with_statement(lexer);

      case JAST_TOKEN_SWITCH:
        return parse_switch_statement(lexer);

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
              JAST_Expr *expr = parse_object_literal_expr(lexer);
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
              JS_String *label = bareword_token_to_string(lexer, &lexer->cur);
              if (jast_lexer_advance(lexer) == JAST_LEXER_ADVANCE_ERROR
               || jast_lexer_advance(lexer) == JAST_LEXER_ADVANCE_ERROR)
                {
                  return NULL;
                }
              JAST_Label_Statement *rv = alloc_statement(JAST_STATEMENT_LABEL, sizeof (JAST_Label_Statement));
              rv->label = label;
              return (JAST_Statement *) rv;
            }
          else
            {
              JAST_Expr *expr = parse_expr(lexer, PARSE_EXPR_ALLOW_COMMA|PARSE_EXPR_ALLOW_TERNARY);
              if (expr == NULL)
                {
                  printf("error parsing expression stmt: %s\n",lexer->error->message);
                  return NULL;
                }
              else
                {
                  if (lexer->has_cur && lexer->cur.type == JAST_TOKEN_SEMICOLON)
                    {
                      switch (jast_lexer_advance (lexer))
                        {
                        case JAST_LEXER_ADVANCE_OK:
                        case JAST_LEXER_ADVANCE_EOF:
                          break;
                        case JAST_LEXER_ADVANCE_ERROR:
                          jast_expr_free (expr);
                          return NULL;
                        }
                    }
                  return jast_statement_from_expr(expr);
                }
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
        {
          JAST_Expr *expr = parse_expr(lexer, PARSE_EXPR_ALLOW_COMMA|PARSE_EXPR_ALLOW_TERNARY);
          if (expr == NULL)
            return NULL;
          else
            return jast_statement_from_expr(expr);
        }

      case JAST_TOKEN_CONTINUE:
      case JAST_TOKEN_BREAK:
        {
          JAST_StatementType type = lexer->cur.type == JAST_TOKEN_CONTINUE ? JAST_STATEMENT_CONTINUE : JAST_STATEMENT_BREAK;
          switch (jast_lexer_advance (lexer))
            {
              case JAST_LEXER_ADVANCE_OK:
                break;
              case JAST_LEXER_ADVANCE_EOF:
                set_premature_eof_parse_error(lexer, "break/continue");
                return NULL;
              case JAST_LEXER_ADVANCE_ERROR:
                return NULL;
            }
          JS_String *opt_label = NULL;
          if (lexer->cur.type == JAST_TOKEN_BAREWORD)
            {
              opt_label = bareword_token_to_string (lexer, &lexer->cur);
              switch (jast_lexer_advance (lexer))
                {
                  case JAST_LEXER_ADVANCE_OK:
                    break;
                  case JAST_LEXER_ADVANCE_EOF:
                    set_premature_eof_parse_error(lexer, "labelled break/continue");
                    return NULL;
                  case JAST_LEXER_ADVANCE_ERROR:
                    return NULL;
                }
            }
          if (lexer->cur.type != JAST_TOKEN_SEMICOLON)
            {
              set_bad_token_parse_error (lexer, &lexer->cur, "expected ; after break/continue");
              js_string_maybe_unref (opt_label);
              return NULL;
            }
          switch (jast_lexer_advance (lexer))
            {
              case JAST_LEXER_ADVANCE_OK:
                break;
              case JAST_LEXER_ADVANCE_EOF:
                break;
              case JAST_LEXER_ADVANCE_ERROR:
                js_string_maybe_unref (opt_label);
                return NULL;
            }
          JAST_Break_Statement *rv = alloc_statement (type, sizeof (JAST_Break_Statement));
          rv->label = opt_label;
          return (JAST_Statement *) rv;
        }

      default:
        fprintf(stderr, "unexpected token type %s\n", jast_token_type_name(lexer->cur.type));
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
      *error = lexer.error;
      lexer.error = NULL;
    }
  return rv;
}

static inline void
clear_statement_array (size_t N, JAST_Statement **arr)
{
  for (size_t i = 0; i < N; i++)
    jast_statement_free (arr[i]);
}


void jast_statement_free (JAST_Statement *stmt)
{
  switch (stmt->type)
    {
      case JAST_STATEMENT_COMPOUND:
        clear_statement_array (stmt->compound_statement.n_subs, stmt->compound_statement.subs);
        break;
      case JAST_STATEMENT_IF:
        {
          JAST_If_Statement *istmt = (JAST_If_Statement *) stmt;
          size_t nc = istmt->n_conditional_statements;
          JAST_ConditionalStatement_Clause *c = istmt->conditional_statements;
          for (size_t i = 0; i < nc; i++)
            {
              jast_expr_free (c[i].expr);
              jast_statement_free (c[i].statement);
            }
          if (istmt->else_statement)
            jast_statement_free (istmt->else_statement);
        }
        break;
      case JAST_STATEMENT_SWITCH:
        {
          JAST_Switch_Statement *sstmt = (JAST_Switch_Statement *) stmt;
          jast_expr_free (sstmt->expr);
          size_t nc = sstmt->n_clauses;
          JAST_Switch_Clause *c = sstmt->clauses;
          for (size_t i = 0; i < nc; i++)
            switch (c[i].clause_type)
              {
              case JAST_SWITCH_CLAUSE_CASE:
                jast_expr_free (c[i].info.case_value);
                break;
              case JAST_SWITCH_CLAUSE_STATEMENT:
                jast_statement_free (c[i].info.statement);
                break;
              case JAST_SWITCH_CLAUSE_DEFAULT:
                break;
              }
        }
        break;
      case JAST_STATEMENT_FOR:
        {
          JAST_For_Statement *fstmt = (JAST_For_Statement *) stmt;
          if (fstmt->initial)
            jast_statement_free (fstmt->initial);
          if (fstmt->condition)
            jast_expr_free (fstmt->condition);
          if (fstmt->advance)
            jast_statement_free (fstmt->advance);
          if (fstmt->body)
            jast_statement_free (fstmt->body);
        }
        break;
      case JAST_STATEMENT_FOR_IN:
        {
          JAST_ForIn_Statement *fstmt = (JAST_ForIn_Statement *) stmt;
          binding_pattern_clear (&fstmt->binding);
          jast_expr_free (fstmt->container);
          jast_statement_free (fstmt->body);
        }
        break;
      case JAST_STATEMENT_WHILE:
        {
          JAST_While_Statement *wstmt = (JAST_While_Statement *) stmt;
          jast_expr_free (wstmt->condition);
          jast_statement_free (wstmt->body);
        }
        break;
      case JAST_STATEMENT_DO_WHILE:
        {
          JAST_DoWhile_Statement *dwstmt = (JAST_DoWhile_Statement *) stmt;
          jast_statement_free (dwstmt->body);
          jast_expr_free (dwstmt->condition);
        }
        break;
      case JAST_STATEMENT_WITH:
        {
          JAST_With_Statement *wstmt = (JAST_With_Statement *) stmt;
          jast_expr_free (wstmt->expr);
          jast_statement_free (wstmt->body);
        }
        break;
      case JAST_STATEMENT_VARIABLE_DECLARATIONS:
        {
          JAST_VariableDeclarations_Statement *vdstmt = (JAST_VariableDeclarations_Statement *) stmt;
          for (size_t i = 0; i < vdstmt->n_vars; i++)
            binding_pattern_clear (&vdstmt->vars[i]);
        }
        break;
      case JAST_STATEMENT_TRY_CATCH:
        {
          JAST_TryCatch_Statement *tcstmt = (JAST_TryCatch_Statement *) stmt;
          jast_statement_free (tcstmt->body);
          js_string_unref (tcstmt->exception_var);
          if (tcstmt->catch_body)
            jast_statement_free (tcstmt->catch_body);
          if (tcstmt->finally_body)
            jast_statement_free (tcstmt->finally_body);
        }
        break;
      case JAST_STATEMENT_LABEL:
        js_string_unref (stmt->label_statement.label);
        break;
      case JAST_STATEMENT_EXPRESSION:
        jast_expr_free (stmt->expr_statement.expr);
        break;
      case JAST_STATEMENT_BREAK:
        js_string_maybe_unref (stmt->break_statement.label);
        break;
      case JAST_STATEMENT_CONTINUE:
        js_string_maybe_unref (stmt->continue_statement.label);
        break;
    }
  free (stmt);
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
      set_premature_eof_parse_error (lexer, "after left-brace in binding-pattern");
      return JS_FALSE;
    }
  DEFINE_STACK_STARTED_ARRAY(JAST_FieldBindingPattern, fields, 16);
  while (lexer->cur.type != JAST_TOKEN_RBRACE)
    {
      JS_String *key = NULL;
      JAST_Expr *computed_key = NULL;
      JAST_BindingPattern binding_pattern;
      switch (convert_reserved_word_to_bareword (lexer->cur.type))
        {
          case JAST_TOKEN_BAREWORD:
            key = bareword_token_to_string (lexer, &lexer->cur);
            switch (jast_lexer_advance (lexer))
              {
              case JAST_LEXER_ADVANCE_OK: break;
              case JAST_LEXER_ADVANCE_ERROR: goto error_cleanup;
              case JAST_LEXER_ADVANCE_EOF: goto premature_eof;
              }
            break;
          case JAST_TOKEN_STRING:
            key = string_literal_to_string (lexer, &lexer->cur);
            switch (jast_lexer_advance (lexer))
              {
              case JAST_LEXER_ADVANCE_OK: break;
              case JAST_LEXER_ADVANCE_ERROR: goto error_cleanup;
              case JAST_LEXER_ADVANCE_EOF: goto premature_eof;
              }
          case JAST_TOKEN_NUMBER:
            key = number_literal_to_key_string (lexer, &lexer->cur);
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
            computed_key = parse_expr (lexer, PARSE_EXPR_ALLOW_COMMA|PARSE_EXPR_ALLOW_TERNARY);
            if (computed_key == NULL)
              goto error_cleanup;
            if (lexer->cur.type != JAST_TOKEN_RBRACKET)
              {
                set_expected_token_parse_error (lexer, JAST_TOKEN_RBRACKET, "in computed-key");
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
            set_bad_token_parse_error (lexer, &lexer->cur, "in object binding pattern");
            goto error_cleanup;
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
          binding_pattern.info.simple = js_string_ref (key);
          binding_pattern.initializer = NULL;
          if (lexer->cur.type == JAST_TOKEN_ASSIGN)
            {
              switch (jast_lexer_advance (lexer))
                {
                case JAST_LEXER_ADVANCE_OK: break;
                case JAST_LEXER_ADVANCE_ERROR: goto error_cleanup;
                case JAST_LEXER_ADVANCE_EOF: goto premature_eof;
                }
              binding_pattern.initializer = parse_expr (lexer, PARSE_EXPR_ALLOW_TERNARY);
              if (binding_pattern.initializer == NULL)
                {
                  js_string_unref (binding_pattern.info.simple);
                  goto error_cleanup;
                }
            }
        }
      else
        {
          set_bad_token_parse_error(lexer, &lexer->cur, "expected key in object-binding pattern");
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
      else if (lexer->cur.type != JAST_TOKEN_RBRACE)
        {
          set_bad_token_parse_error (lexer, &lexer->cur, "in object-binding pattern");
          goto error_cleanup;
        }

      JAST_FieldBindingPattern field = {
        key,
        computed_key,
        binding_pattern
      };
      APPEND_TO_STACK_STARTED_ARRAY(JAST_FieldBindingPattern, fields, field);
    }
  switch (jast_lexer_advance (lexer))
    {
      case JAST_LEXER_ADVANCE_OK:
        break;
      case JAST_LEXER_ADVANCE_EOF:
        break;
      case JAST_LEXER_ADVANCE_ERROR:
        goto error_cleanup;
    }
  out->type = JAST_BINDING_PATTERN_OBJECT;
  out->info.object.n_fields = n_fields;
  out->info.object.fields = malloc (n_fields * sizeof (JAST_FieldBindingPattern));
  memcpy (out->info.object.fields, fields, sizeof (JAST_FieldBindingPattern) * n_fields);
  MAYBE_CLEAR_STACK_STARTED_ARRAY(fields);
  return JS_TRUE;

premature_eof:
  set_premature_eof_parse_error (lexer, "in object binding-pattern");
  /* fallthrough */

error_cleanup:
  for (size_t k = 0; k < n_fields; k++)
    {
      if (fields[k].name)
        js_string_unref (fields[k].name);
      if (fields[k].computed_name)
        jast_expr_free (fields[k].computed_name);
      binding_pattern_clear (&fields[k].binding);
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
        BindingPattern Initializer(opt)
  BindingRestElement :
        ... BindingIdentifier
 */
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_ERROR:
      return JS_FALSE;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "after [ in binding-pattern");
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
          APPEND_TO_STACK_STARTED_ARRAY (JAST_BindingPattern, patterns, bp);

          switch (jast_lexer_advance (lexer))
            {
            case JAST_LEXER_ADVANCE_OK:
              break;
            case JAST_LEXER_ADVANCE_ERROR:
              goto error_cleanup;
            case JAST_LEXER_ADVANCE_EOF:
              set_premature_eof_parse_error (lexer, "in array binding-part");
              goto error_cleanup;
            }
        }
      else
        {
          if (!parse_binding_pattern (lexer, &bp))
            goto error_cleanup;
          APPEND_TO_STACK_STARTED_ARRAY (JAST_BindingPattern, patterns, bp);
          if (!lexer->has_cur)
            {
              set_premature_eof_parse_error (lexer, "in array binding-pattern");
              goto error_cleanup;
            }
          if (lexer->cur.type == JAST_TOKEN_COMMA)
            {
              switch (jast_lexer_advance (lexer))
                {
                case JAST_LEXER_ADVANCE_OK:
                  break;
                case JAST_LEXER_ADVANCE_ERROR:
                  goto error_cleanup;
                case JAST_LEXER_ADVANCE_EOF:
                  set_premature_eof_parse_error (lexer, "in array binding-part");
                  goto error_cleanup;
                }
            }
          else if (lexer->cur.type != JAST_TOKEN_RBRACKET)
            {
              set_bad_token_parse_error (lexer, &lexer->cur, "expected ] or expression or ','");
              goto error_cleanup;
            }
        }
    }

  // skip right-bracket
  switch (jast_lexer_advance (lexer))
    {
    case JAST_LEXER_ADVANCE_OK:
      break;
    case JAST_LEXER_ADVANCE_ERROR:
      goto error_cleanup;
    case JAST_LEXER_ADVANCE_EOF:
      set_premature_eof_parse_error (lexer, "in array binding-part");
      goto error_cleanup;
    }

  out->type = JAST_BINDING_PATTERN_ARRAY;
  out->info.array.n_subs = n_patterns;
  size_t s = n_patterns * sizeof (JAST_BindingPattern);
  out->info.array.subs = memcpy (malloc (s), patterns, s);
  MAYBE_CLEAR_STACK_STARTED_ARRAY (patterns);
  return JS_TRUE;

error_cleanup:
  for (size_t k = 0; k < n_patterns; k++)
    binding_pattern_clear (patterns + k);
  MAYBE_CLEAR_STACK_STARTED_ARRAY (patterns);
  return JS_FALSE;
}

static JS_Boolean
parse_binding_pattern (JAST_Lexer *lexer, JAST_BindingPattern *out)
{
  out->initializer = NULL;
  if (!lexer->has_cur)
    {
      set_premature_eof_parse_error (lexer, "parsing binding pattern");
      return JS_FALSE;
    }
  if (lexer->cur.type == JAST_TOKEN_BAREWORD)
    {
      out->type = JAST_BINDING_PATTERN_SIMPLE;
      out->info.simple = bareword_token_to_string (lexer, &lexer->cur);
      switch (jast_lexer_advance (lexer))
        {
          case JAST_LEXER_ADVANCE_OK:
            break;
          case JAST_LEXER_ADVANCE_EOF:
            return JS_TRUE;
          case JAST_LEXER_ADVANCE_ERROR:
            return JS_FALSE;
        }
    }
  else if (lexer->cur.type == JAST_TOKEN_LBRACE)
    {
      if (!parse_object_binding_pattern (lexer, out))
        return JS_FALSE;
    }
  else if (lexer->cur.type == JAST_TOKEN_LBRACKET)
    {
      if (!parse_array_binding_pattern (lexer, out))
        return JS_FALSE;
    }
  else
    {
      set_bad_token_parse_error (lexer, &lexer->cur, "parsing variable binding");
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
      out->initializer = parse_expr (lexer, PARSE_EXPR_ALLOW_TERNARY);
      if (out->initializer == NULL)
        {
          binding_pattern_clear (out);
          return JS_FALSE;
        }
    }
  return JS_TRUE;

premature_eof:
  set_premature_eof_parse_error (lexer, "in binding pattern");
  return JS_FALSE;
}

static void            binding_pattern_clear (JAST_BindingPattern *pattern)
{
  switch (pattern->type)
    {
    case JAST_BINDING_PATTERN_NONE:
      return;

    case JAST_BINDING_PATTERN_SIMPLE:
      js_string_unref (pattern->info.simple);
      return;

    case JAST_BINDING_PATTERN_ARRAY:
      {
        size_t n_subs = pattern->info.array.n_subs;
        JAST_BindingPattern *subs = pattern->info.array.subs;
        for (size_t i = 0; i < n_subs; i++)
          binding_pattern_clear (subs + i);
        free (subs);
        return;
      }

    case JAST_BINDING_PATTERN_OBJECT:
      {
        size_t n_fields = pattern->info.object.n_fields;
        JAST_FieldBindingPattern *fields = pattern->info.object.fields;
        for (size_t i = 0; i < n_fields; i++)
          {
            if (fields[i].name)
              js_string_unref (fields[i].name);
            if (fields[i].computed_name)
              jast_expr_free (fields[i].computed_name);
            binding_pattern_clear (&fields[i].binding);
          }
        free (fields);
        return;
      }
    }
  assert (0);
}
