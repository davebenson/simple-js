#include <stdio.h>
#include <assert.h>
#include <dsk.h>

struct Pair
{
  unsigned start, end;
};

typedef struct {
  unsigned size;
  unsigned char b[4];
} Accept;

Accept accepts[1000000];

static int
is_complete_range(unsigned na, Accept *a, unsigned p)
{
  unsigned complete_size = (0xbf - 0x80 + 1);
  if (p + 2 == a->size)
    return na == complete_size * complete_size;
  else if (p + 1 == a->size)
    return na == complete_size;
  return 0;
}

/* "p" == number of common character in the prefix;
 * all n_accepts/accepts agree on this many chars, and the input
 * has been checked for agreement as well. */
static void
implement_accept_range (unsigned n_accepts,
                        Accept *accepts,
                        unsigned p)
{
  assert(n_accepts > 0);
  unsigned indent = p * 4 + 4;

  if (accepts[0].size == p + 1)
    {
      /* on the last character */
      unsigned n_ranges = 1;
      for (unsigned i = 1; i < n_accepts; i++)
        if (accepts[i - 1].b[p] + 1 != accepts[i].b[p])
          n_ranges++;
      if (n_ranges < 6)
        {
          for (unsigned i = 0; i < n_accepts; )
            {
              unsigned range_size;
              Accept *a = accepts + i;
              for (range_size = 1; i + range_size < n_accepts; range_size++, a++)
                if (a->b[p] + 1 != (a+1)->b[p])
                  break;
              if (range_size == 1)
                printf("%*sif (buf[%u] == 0x%02x) return %u;\n",
                       indent, "",
                       p,
                       accepts[i].b[p],
                       p + 1);
              else
                printf("%*sif (0x%02x <= buf[%u] && buf[%u] <= 0x%02x) return %u;\n",
                       indent, "",
                       accepts[i].b[p],
                       p,
                       p,
                       accepts[i].b[p] + range_size - 1,
                       p + 1);
              i += range_size;
            }
        }
      else
        {
          /* Too many ranges to use if-then-elses: use a switch stmt. */
          printf("%*sswitch ((uint8_t) buf[%u]) {\n", indent, "", p);
            for (unsigned i = 0; i < n_accepts; i++)
              printf("%*scase 0x%02x: return %u;\n", indent, "", accepts[i].b[p], accepts[i].size);
          printf("%*s}\n", indent, "");
        }
    }
  else
    {
      unsigned ai = 0;
      printf("%*sswitch ((uint8_t ) buf[%u]) {\n",indent, "", p);
      for (unsigned b = 0; ai < n_accepts && b < 256; b++)
        {
          unsigned n_suba = 0;
          while (ai + n_suba < n_accepts && accepts[ai + n_suba].b[p] == b)
            n_suba++;
          if (n_suba == 0)
            continue;
          printf("%*scase 0x%02x:\n", indent+2, "", b);
          if (is_complete_range (n_suba, accepts + ai, p + 1))
            printf("%*sreturn %u;\n", indent+4, "", accepts[ai].size);
          else
            implement_accept_range(n_suba, accepts + ai, p + 1);

          ai += n_suba;
        }
      printf("%*s}\n", indent, "");
    }
  printf("%*sreturn 0;\n", indent, "");
}

int main(int argc, char **argv)
{
  if (argc == 1) {
    fprintf(stderr, "usage: %s CHAR_CLASS_FILE\n\n");
    return 1;
  }

  FILE *fp = fopen(argv[1], "r");
  char buf[256];
  struct Pair pairs[10000];
  unsigned n_pairs = 0;
  unsigned n_accepts = 0;
  while (fgets(buf, sizeof(buf), fp))
    {
      struct Pair p;
      if (sscanf(buf, "%x..%x", &p.start, &p.end) == 2)
        {
        }
      else if (sscanf(buf, "%x", &p.start) == 1)
        {
          p.end = p.start;
        }
      else
        assert(0);
      pairs[n_pairs++] = p;

      for (unsigned i = p.start; i <= p.end; i++)
        {
          char buf[10];
          Accept *a = accepts + n_accepts++;
          a->size = dsk_utf8_encode_unichar ((char*) a->b, i);
          //for (unsigned j = 0; j < n; j++)
            //printf("%02x%c", (uint8_t) buf[j], (j == n - 1) ? '\n' : ' ');
        }
    }

  printf("  switch ((uint8_t) buf[0]) {\n");
  unsigned aa = 0;
  for (unsigned p = 0; p < 256 && aa < n_accepts; p++)
    {
      unsigned n_p = 0;
      while (n_p + aa < n_accepts && accepts[n_p + aa].b[0] == p)
        n_p++;
      if (n_p == 0)
        continue;
      printf("    case 0x%02x:\n", p);
      if (n_p == 1 && accepts[aa].size == 1)
        printf("      return 1;\n");
      else
        implement_accept_range (n_p, accepts + aa, 1);
      aa += n_p;
    }


  printf("  }\n");
  printf("  return 0;\n");

  return 0;
}

