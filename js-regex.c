#include "js-regex.h"
#include <pcre.h>
#include <assert.h>
#include <string.h>

struct JS_Regex
{
  unsigned ref_count;
  pcre *regex;
};

static inline JS_Regex_ParseResult
translate_pcre_error_code_to_js_regex_error_code (int errcode)
{
  (void) errcode;
  return JS_REGEX_PARSE_RESULT_BAD_CHAR;
}

JS_Regex_ParseResult js_regex_parse  (size_t         length,
                                      const char    *str,
                                      size_t        *err_loc_out,
                                      JS_Regex     **regex_out)
{
  const char *init_str = str;

  // strip off initial '/'
  assert(*str == '/');
  length--;
  str++;

  // find terminal '/'
  const char *end = str + length - 1;
  while (end > str && *end != '/')
    end--;
  if (end == str)
    {
      *err_loc_out = str - init_str;
      return JS_REGEX_PARSE_RESULT_ZERO_LENGTH;
    }

  // handle options
  int options = PCRE_JAVASCRIPT_COMPAT;
  for (const char *o_at = end + 1; o_at < str + length; o_at++)
    switch (*o_at)
      {
      case 'i':
        options |= PCRE_CASELESS;
        break;
      case 's':
        break;
      case 'g':
        break;
      }

  // copy bulk of RE onto heap to NUL-terminate it.
  char *re_string = malloc (end - str + 1);
  memcpy (re_string, str, end - str);
  re_string[end - str] = 0;

  // parse RE
  int perrcode;
  const char *errptr;
  int erroffset;
  pcre *r = pcre_compile2 (re_string, options, &perrcode, &errptr, &erroffset, NULL);
  if (r == NULL)
    {
      *err_loc_out = errptr - re_string + (str - init_str);
      free (re_string);
      return translate_pcre_error_code_to_js_regex_error_code (perrcode);
    }
  free (re_string);

  JS_Regex *rv = malloc (sizeof(JS_Regex));
  rv->ref_count = 1;
  rv->regex = r;
  *regex_out = rv;
  return JS_REGEX_PARSE_RESULT_OK;
}

JS_Regex *
js_regex_ref    (JS_Regex      *regex)
{
  assert (regex->ref_count > 0);
  regex->ref_count += 1;
  return regex;
}

void
js_regex_unref  (JS_Regex      *regex)
{
  assert (regex->ref_count > 0);
  if (--(regex->ref_count) == 0)
    {
      pcre_free (regex->regex);
      free (regex);
    }
}

const char *js_regex_parse_result_get_message (JS_Regex_ParseResult result)
{
  switch (result)
    {
      case JS_REGEX_PARSE_RESULT_OK:
        return "success";
      case JS_REGEX_PARSE_RESULT_PREMATURE_EOF:
        return "regex ended prematurely";
      case JS_REGEX_PARSE_RESULT_BAD_CHAR:
        return "unexpected character in regex";
      case JS_REGEX_PARSE_RESULT_ZERO_LENGTH:
        return "zero-length regex not allowed";
    }
  assert(0);
  return NULL;
}
