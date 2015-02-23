/* Using Murmurhash3 */

#include <stdint.h>
#include <stddef.h>

uint32_t jhash32_aligned_padded (size_t N, const uint32_t *vals);
