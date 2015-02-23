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
