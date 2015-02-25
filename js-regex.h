
typedef struct JS_Regex JS_Regex;

#include <stddef.h>

typedef enum
{
  JS_REGEX_PARSE_RESULT_OK,
  JS_REGEX_PARSE_RESULT_PREMATURE_EOF,
  JS_REGEX_PARSE_RESULT_BAD_CHAR,
  JS_REGEX_PARSE_RESULT_ZERO_LENGTH,
} JS_Regex_ParseResult;

JS_Regex_ParseResult js_regex_parse  (size_t         length,
                                      const char    *str,
                                      size_t        *err_loc_out,
                                      JS_Regex     **regex_out);
JS_Regex            *js_regex_ref    (JS_Regex      *regex);
void                 js_regex_unref  (JS_Regex      *regex);

const char *js_regex_parse_result_get_message (JS_Regex_ParseResult result);
