#include "js-string.h"
#include "jutf8.h"

JS_String *js_string_new_utf8        (const char *literal)
{
  return js_string_new_utf8_len (strlen (literal), literal);
}

JS_String *js_string_new_utf8_len    (size_t      length,
                                      const char *literal)
{
  size_t rv_size = sizeof (JS_String *) + length;

  /* padded_size ensures there's 1..4 bytes of padding */
  size_t padded_size = ((rv / 4) + 1) * 4;

  switch (jutf8_strlen (length, literal, &n_codepoints, &end_out))
    {
    case JUTF8_STRLEN_RESULT_BAD_ENCODING:
      ...
    case JUTF8_STRLEN_RESULT_PREMATURE_EOF:
      ...
    case JUTF8_STRLEN_RESULT_OK
      ...
    }

  JS_String *rv = malloc (padded_size);
  rv->n_bytes = length;
  rv->n_chars = n_codepoints;
  memcpy (rv + 1, literal, length);
  memset ((char*)(rv + 1) + length, padded_size - rv_size);
  rv->hash = hash32 (...);
  return rv;
}

JS_String *js_string_new_from_number (double      value);
JS_String *js_string_ref             (JS_String  *str);
void       js_string_unref           (JS_String  *str);
void       js_string_maybe_unref     (JS_String  *optional_str);
JS_String *js_string_new_format      (const char *format,
                                      ...);
JS_String *js_string_new_format_v    (const char *format,
                                      va_list     args);
JS_String *js_string_join_cstrings   (JS_String  *separator,
                                      size_t      n_strs,
                                      char      **strs);
JS_String *js_string_join_strings    (JS_String  *separator,
                                      size_t      n_strs,
                                      char      **strs);
JS_String *js_string_concat          (size_t      n_strs,
                                      char      **strs);
JS_String *js_string_substring       (size_t      n_strs,
                                      char      **strs);

typedef struct {
  JS_String *string;
  size_t     byte_offset;
  size_t     char_offset;
} JS_String_Iterator;

#define JS_STRING_ITERATOR_INIT(string)   { (string), 0, 0 }

int        js_string_iterator_peek         (const JS_String_Iterator *iterator);
JS_Boolean js_string_iterator_move_bytes   (JS_String_Iterator *iterator,
                                            int                 direction,
                                            size_t              delta_bytes);
JS_Boolean js_string_iterator_setpos_bytes (JS_String_Iterator *iterator,
                                            size_t              bytes);
JS_Boolean js_string_iterator_move_chars   (JS_String_Iterator *iterator,
                                            int                 direction,
                                            size_t              delta_chars);
JS_Boolean js_string_iterator_setpos_chars (JS_String_Iterator *iterator,
                                            size_t              chars);
JS_Boolean js_string_iterator_search       (JS_String_Iterator *iterator,
                                            int                 direction,
                                            JS_String *needle);


#endif
