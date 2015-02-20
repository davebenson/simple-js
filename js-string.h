#ifndef __JS_STRING_H_
#define __JS_STRING_H_

#include <stddef.h>
#include <stdarg.h>

/* --- numbers --- */

/* --- booleans --- */
typedef unsigned char JS_Boolean;
#define JS_FALSE 0
#define JS_TRUE  1

/* --- strings --- */
typedef struct _JS_String JS_String;
struct _JS_String {
  unsigned ref_count;
  unsigned hash;
  unsigned byte_length;
  unsigned char_length;
  /* string data follows */
};


JS_String *js_string_new_utf8        (const char *literal);
JS_String *js_string_new_utf8_len    (size_t      length,
                                      const char *literal);
JS_String *js_string_ref             (JS_String  *str);
void       js_string_unref           (JS_String  *str);
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
