
unsigned jutf8_scan_idstart_char (const char *utf8);
unsigned jutf8_scan_idcontinue_char (const char *utf8);
int jutf8_is_post_number_character (const char *utf8);

#include <stdint.h>
#include <stddef.h>

/* These return -1 if the character is invalid.
   Valid digit codepoint are always ASCII hence 1 byte. */
static inline int jutf8_hex_digit_value (const char *utf8);
static inline int jutf8_octal_digit_value (const char *utf8);
static inline int jutf8_binary_digit_value (const char *utf8);
static inline int jutf8_digit_value (const char *utf8);

typedef enum
{
  JUTF8_STRLEN_RESULT_OK,
  JUTF8_STRLEN_RESULT_BAD_ENCODING,
  JUTF8_STRLEN_RESULT_PREMATURE_EOF,
} JUTF8_Strlen_Result;

JUTF8_Strlen_Result jutf8_strlen (size_t length, const char *text,
                                  size_t *n_codepoints_out,
                                  const char **optional_end_out);


/* These functions return the length of the utf8 encoded data, in bytes.
   If jutf8_decode1 fails, it returns 0.  jutf8_encode1 will return a number 1..6. */
static inline unsigned jutf8_decode1 (size_t utf8_len, const char *utf8, unsigned *unicode_codepoint_out);
static inline unsigned jutf8_encode1 (unsigned unicode_codepoint, char *utf8_out);

#if 0
#define LBRACE_CHAR   '{'
#define LBRACE_STR    "{"
#define RBRACE_CHAR   '}'
#define RBRACE_STR    "}"

#define IS_DECIMAL_DIGIT(c)       \
  ('0' <= (c) && (c) <= '9')
#define IS_OCTAL_DIGIT(c)         \
  (((c) & 0xf8) == '0')
#define IS_BINARY_DIGIT(c)        \
  (((c) & 0xfe) == '0')
#define IS_HEXIDECIMAL_DIGIT(c)   \
  (  IS_DECIMAL_DIGIT(c)          \
  || ('a' <= (c) && (c) <= 'f')   \
  || ('A' <= (c) && (c) <= 'F') )
#define HEXDIGIT_VALUE(c) \
  ( IS_DECIMAL_DIGIT (c) ? ((c) - '0') \
  : ('a' <= (c) && (c) <= 'f') ? (((c) - 'a') + 10) \
  : (((c) - 'A') + 10) )
#endif

static inline unsigned jutf8_encode1 (uint32_t c, char *outbuf)
{
  /* stolen from glib */
  unsigned len = 0;    
  int first;
  int i;

  if (c < 0x80)
    {
      first = 0;
      len = 1;
    }
  else if (c < 0x800)
    {
      first = 0xc0;
      len = 2;
    }
  else if (c < 0x10000)
    {
      first = 0xe0;
      len = 3;
    }
   else if (c < 0x200000)
    {
      first = 0xf0;
      len = 4;
    }
  else if (c < 0x4000000)
    {
      first = 0xf8;
      len = 5;
    }
  else
    {
      first = 0xfc;
      len = 6;
    }

  if (outbuf)
    {
      for (i = len - 1; i > 0; --i)
	{
	  outbuf[i] = (c & 0x3f) | 0x80;
	  c >>= 6;
	}
      outbuf[0] = c | first;
    }

  return len;
}

static inline unsigned jutf8_decode1 (size_t    buf_len,
                                     const char *buf,
                                     uint32_t   *unicode_value_out)
{
  if (buf_len == 0)
    return 0;
  uint8_t d = *buf;
  if (d < 0x80)
    {
      *unicode_value_out = d;
      return 1;
    }
  else if (d < 0xc0)
    return 0;
  else if (d < 0xe0)
    {
      /* two byte sequence */
      if (buf_len < 2)
        return 0;
      *unicode_value_out = ((d & 0x1f) << 6) | (((uint8_t*)buf)[1] & 0x3f);
      return 2;
    }
  else if (d < 0xf0)
    {
      /* three byte sequence */
      if (buf_len < 3)
        return 0;
      *unicode_value_out = ((d & 0x0f) << 12)
                         | ((((uint8_t*)buf)[1] & 0x3f) << 6)
                         | ((((uint8_t*)buf)[2] & 0x3f) << 0);
      return 3;
    }
  else if (d < 0xf8)
    {
      /* four byte sequence */
      if (buf_len < 4)
        return 0;
      *unicode_value_out = ((d & 0x07) << 18)
                         | ((((uint8_t*)buf)[1] & 0x3f) << 12)
                         | ((((uint8_t*)buf)[2] & 0x3f) << 6)
                         | ((((uint8_t*)buf)[3] & 0x3f) << 0);
      return 4;
    }
  else
    return 0;
}

static inline int jutf8_hex_digit_value (const char *utf8)
{
  char c = *utf8;
  if ('0' <= c && c <= '9')
    return c - '0';
  else if ('a' <= c && c <= 'f')
    return c - 'a' + 10;
  else 
    return c - 'A' + 10;
}

static inline int jutf8_octal_digit_value (const char *utf8)
{
  return *utf8 - '0';
}
static inline int jutf8_binary_digit_value (const char *utf8)
{
  return *utf8 - '0';
}
static inline int jutf8_digit_value (const char *utf8)
{
  return *utf8 - '0';
}
