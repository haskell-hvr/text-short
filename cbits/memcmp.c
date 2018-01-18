#include <string.h>

int
hs_text_short_memcmp(const void *s1, const size_t s1ofs, const void *s2, const size_t s2ofs, const size_t n)
{
  if (!n) return 0;

  const void *s1_ = s1+s1ofs;
  const void *s2_ = s2+s2ofs;

  return (s1_ == s2_) ? 0 : memcmp(s1_, s2_, n);
}
