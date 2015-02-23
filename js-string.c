#include "js-string.h"
#include "jutf8.h"
#include "jhash.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

JS_String *js_string_new_utf8        (const char *literal)
{
  return js_string_new_utf8_len (strlen (literal), literal);
}

JS_String *js_string_new_utf8_len    (size_t      length,
                                      const char *literal)
{
  size_t rv_size = sizeof (JS_String *) + length;

  /* padded_size ensures there's 1..4 bytes of padding */
  size_t padded_size = ((rv_size / 4) + 1) * 4;
  const char *end_out;
  size_t n_codepoints;

  switch (jutf8_strlen (length, literal, &n_codepoints, &end_out))
    {
    case JUTF8_STRLEN_RESULT_BAD_ENCODING:
      return NULL;
    case JUTF8_STRLEN_RESULT_PREMATURE_EOF:
      return NULL;
    case JUTF8_STRLEN_RESULT_OK:
      break;
    }

  JS_String *rv = malloc (padded_size);
  rv->n_bytes = length;
  rv->n_chars = n_codepoints;
  memcpy (rv + 1, literal, length);
  memset ((char*)(rv + 1) + length, 0, padded_size - rv_size);
  rv->hash = jhash32_aligned_padded (padded_size/4, (uint32_t*)(rv + 1));
  return rv;
}

JS_String *js_string_new_from_number (double      value)
{
  char buf[80];
  if (-4503599627370496. < value
     && value < -4503599627370496
     && value == floor(value))
    {
      int64_t v = value;
      snprintf(buf,sizeof(buf), "%lld", v);
    }
  else
    {
      snprintf(buf,sizeof(buf), "%f", value);
    }
  return js_string_new_utf8 (buf);
}

JS_String *js_string_ref             (JS_String  *str)
{
  str->ref_count += 1;
  return str;
}
void       js_string_unref           (JS_String  *str)
{
  if (--(str->ref_count) == 0)
    free (str);
}
void       js_string_maybe_unref     (JS_String  *str)
{
  if (str != NULL && --(str->ref_count) == 0)
    free (str);
}

JS_String *js_string_new_format      (const char *format,
                                      ...)
{
  va_list args;
  va_start (args, format);
  JS_String *rv = js_string_new_format_v (format, args);
  va_end (args);
  return rv;
}

JS_String *js_string_new_format_v    (const char *format,
                                      va_list     args)
{
  va_list copy;
  va_copy (copy, args);
  size_t len = vsnprintf (NULL, 0, format, args);
  size_t rv_len = sizeof (JS_String) + len;
  size_t padded_size = ((rv_len / 4) + 1) * 4;
  JS_String *rv = malloc (padded_size);
  rv->n_bytes = len;
  rv->n_chars = len;
  vsnprintf ((char*)(rv+1), len + 1, format, copy);
  va_end (copy);
  memset ((char*)(rv + 1) + len, 0, padded_size - rv_len);
  rv->hash = jhash32_aligned_padded (padded_size/4, (uint32_t*)(rv + 1));
  return rv;
}

