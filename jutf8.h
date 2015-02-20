
unsigned jutf8_scan_idstart_char (const char *utf8);
unsigned jutf8_scan_idcontinue_char (const char *utf8);
int jutf8_is_post_number_character (const char *utf8);

/* These return -1 if the character is invalid.
   Valid digit codepoint are always ASCII hence 1 byte. */
int jutf8_hex_digit_value (const char *utf8);
int jutf8_octal_digit_value (const char *utf8);
int jutf8_binary_digit_value (const char *utf8);
int jutf8_digit_value (const char *utf8);

/* These functions return the length of the utf8 encoded data, in bytes.
   If jutf8_decode1 fails, it returns 0.  jutf8_encode1 will return a number 1..6. */
static inline unsigned jutf8_decode1 (const char *utf8, unsigned *unicode_codepoint_out);
static inline unsigned jutf8_encode1 (unsigned unicode_codepoint, char *utf8_out);

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
#define HEXDIGIT_VALUE(c) \
  ( IS_DECIMAL_DIGIT (c) ? ((c) - '0') \
  : ('a' <= (c) && (c) <= 'f') ? (((c) - 'a') + 10) \
  : (((c) - 'A') + 10) )
