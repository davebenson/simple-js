#include "jhash.h"

//-----------------------------------------------------------------------------
// Finalization mix - force all bits of a hash block to avalanche

static inline uint32_t fmix32 ( uint32_t h )
{
  h ^= h >> 16;
  h *= 0x85ebca6b;
  h ^= h >> 13;
  h *= 0xc2b2ae35;
  h ^= h >> 16;

  return h;
}
static inline uint32_t rotl32 ( uint32_t x, int8_t r )
{
  return (x << r) | (x >> (32 - r));
}

#define	ROTL32(x,y)	rotl32(x,y)

//-----------------------------------------------------------------------------

uint32_t jhash32_aligned_padded_seeded (size_t N,
                                    const uint32_t *blocks,
                                    uint32_t seed)
{
  //const uint8_t * data = (const uint8_t*)key;

  uint32_t h1 = seed;

  const uint32_t c1 = 0xcc9e2d51;
  const uint32_t c2 = 0x1b873593;

  //----------
  // body

  for (size_t i = 0; i < N; i++)
  {
    uint32_t k1 = blocks[i];

    k1 *= c1;
    k1 = ROTL32(k1,15);
    k1 *= c2;
    
    h1 ^= k1;
    h1 = ROTL32(h1,13); 
    h1 = h1*5+0xe6546b64;
  }

  h1 ^= N * 4;
  h1 = fmix32(h1);

  return h1;
} 

uint32_t jhash32_aligned_padded (size_t N, const uint32_t *blocks)
{
  return jhash32_aligned_padded_seeded (N, blocks, 0x3524623e);
}
