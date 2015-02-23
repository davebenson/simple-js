#include "jutf8.h"
#include <stdlib.h>

unsigned jutf8_scan_idstart_char (const char *utf8)
{
  const uint8_t *buf = (const uint8_t *) utf8;
#include "generated/scan-id-start.c"
}

unsigned jutf8_scan_idcontinue_char (const char *utf8)
{
  const uint8_t *buf = (const uint8_t *) utf8;
#include "generated/scan-id-continue.c"
}

int jutf8_is_post_number_character (const char *utf8)
{
  unsigned char u = *utf8;
  return u <= '/'
      || (':' <= u && u <= '@')
      || ('[' <= u && u <= '^')
      || u == '`'
      || ('{' <= u && u <= '~');
}

JUTF8_Strlen_Result jutf8_strlen (size_t length, const char *text,
                                  size_t *n_codepoints_out,
                                  const char **optional_end_out)
{
  static const int8_t lens_8[256/8] = {
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,2,3,3,4,-1
  };
  size_t n_cp = 0;
  const uint8_t *t = (const uint8_t *) text;
  int l = lens_8[*t / 8];
  while (length > 0)
    {
      if (l < 0)
        {
          if (optional_end_out)
            *optional_end_out = (const char *) t;
          *n_codepoints_out = n_cp;
          return JUTF8_STRLEN_RESULT_BAD_ENCODING;
        }
      else if ((size_t) l > length)
        {
          if (optional_end_out)
            *optional_end_out = (const char *) t;
          *n_codepoints_out = n_cp;
          return JUTF8_STRLEN_RESULT_PREMATURE_EOF;
        }
      length -= l;
      t += l;
      n_cp += 1;
    }
  if (optional_end_out)
    *optional_end_out = (const char *) t;
  *n_codepoints_out = n_cp;
  return JUTF8_STRLEN_RESULT_OK;
}
