/*
 * Copyright (c) 2017, Herbert Valerio Riedel
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of Herbert Valerio Riedel nor the names of other
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#if !defined(NDEBUG)
# warning assert(3) checks enabled
#endif

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <HsFFI.h>

#if !defined(SIZEOF_VOID_P)
# error <HsFFI.h> SIZEOF_VOID_P not defined
#endif

#if (SIZEOF_VOID_P) == 8
const static bool is_64bit = true;
#elif (SIZEOF_VOID_P) == 4
const static bool is_64bit = false;
#else
# error unexpected SIZEOF_VOID_P value
#endif

#if (WORDS_BIGENDIAN)
const static bool is_bigendian = true;
#else
const static bool is_bigendian = false;
#endif

#if defined(__GNUC__)
# define likely(x)     __builtin_expect(!!(x),1)
# define unlikely(x)   __builtin_expect(!!(x),0)
#else
# define likely(x)     (x)
# define unlikely(x)   (x)
#endif

/* test whether octet in UTF-8 steam is not a continuation byte, i.e. a leading byte */
#define utf8_lead_p(octet) (((octet) & 0xc0) != 0x80)

/* 0 <= x <= 0x110000 */
typedef HsWord codepoint_t;

/* Count number of code-points in well-formed utf8 string */
size_t
hs_text_short_length(const uint8_t buf[], const size_t n)
{
  size_t j = 0;
  size_t l = 0;

  /* Both GCC & Clang are able to optimise the code below quite well at -O3 */
  for (j = 0; j < n; j++)
    if (utf8_lead_p(buf[j]))
      l++;

  return l;
}

/* Locate offset of j-th code-point in well-formed utf8 string
 *
 */
size_t
hs_text_short_index_ofs(const uint8_t buf[], const size_t n, const size_t i)
{
  if (!n)
    return n;

  size_t m = 0;
  size_t j = 0;

  for (;;) {
    assert(m >= 0);
    assert(j <= i);
    assert(j <= m);

    if (j == i)
      return m; /* found */

    if (i-j >= n-m)
      return n; /* i-th char is >= buf+n */

    assert(m < n);
    const uint8_t b0 = buf[m];

    if (!(b0 & 0x80))
      m += 1;   /* 0_______ */
    else
      switch(b0 >> 4) {
      case 0xf: /* 11110___ */
        m += 4;
        break;
      case 0xe: /* 1110____ */
        m += 3;
        break;
      default:  /* 110_____ */
        m += 2;
        break;
      }

    j += 1;
  }

  assert(0);
}

/* Locate offset of j-th code-point (in reverse direction) in
 * well-formed utf8 string starting at end of buffer.
 *
 * The 0-th character from the end is the last character in the utf8
 * string (if it exists).
 *
 * Returns original 'n' if out of bounds.
 *
 */
size_t
hs_text_short_index_ofs_rev(const uint8_t buf[], const size_t n, const size_t i)
{
  size_t m = n;
  size_t j = i;

  for (;;) {
    assert(m <= n);
    assert(j >= 0);

    if (j >= m)
      return n; /* i-th char is < buf */

    /* if (m == i-j) /\* suffix is made up only of ASCII chars, so we can shortcut *\/ */
    /*   return 0; */

    /* scan until octet does not match 10_ */
    assert(m > 0);
    if (!(buf[--m] & 0x80))
      goto l_cont;

    assert(m > 0);
    if (utf8_lead_p(buf[--m])) {
      assert ((buf[m] & 0xe0) == 0xc0); /* 110_ */
      goto l_cont;
    }

    assert(m > 0);
    if (utf8_lead_p(buf[--m])) {
      assert ((buf[m] & 0xf0) == 0xe0); /* 1110_ */
      goto l_cont;
    }

    /* this must be a non-10_ octet in a well-formed stream */
    assert(m > 0);
    m -= 1;

    assert ((buf[m] & 0xf8) == 0xf0); /* 11110_ */

  l_cont:
    assert(utf8_lead_p(buf[m]));

    if (!j)
      return m; /* found */

    j -= 1;
  }

  assert(0);
}

/* Decode UTF8 code units into code-point
 * Assumes buf[] points to start of a valid UTF8-encoded code-point
 */
static inline uint32_t
hs_text_short_decode_cp(const uint8_t buf[])
{
  /*  7 bits | 0xxxxxxx
   * 11 bits | 110yyyyx  10xxxxxx
   * 16 bits | 1110yyyy  10yxxxxx  10xxxxxx
   * 21 bits | 11110yyy  10yyxxxx  10xxxxxx  10xxxxxx
   */

  const uint8_t b0 = buf[0];

  if (!(b0 & 0x80))
    return b0;

  uint32_t cp = 0;

  switch(b0 >> 4) {
  case 0xf: /* 11110___ */
    assert((b0 & 0xf8) == 0xf0);
    assert(!utf8_lead_p(buf[1]));
    assert(!utf8_lead_p(buf[2]));
    assert(!utf8_lead_p(buf[3]));
    cp  = ((uint32_t)(b0     & 0x07)) << (6+6+6);
    cp |= ((uint32_t)(buf[1] & 0x3f)) << (6+6);
    cp |= ((uint32_t)(buf[2] & 0x3f)) << 6;
    cp |=             buf[3] & 0x3f;
    assert (cp > 0xffff); assert (cp < 0x110000);
    return cp;

  case 0xe: /* 1110____ */
    assert(!utf8_lead_p(buf[1]));
    assert(!utf8_lead_p(buf[2]));
    cp  = ((uint32_t)(b0     & 0x0f)) << (6+6);
    cp |= ((uint32_t)(buf[1] & 0x3f)) << 6;
    cp |=             buf[2] & 0x3f;
    assert (cp > 0x7ff); assert (cp < 0x10000);
    assert (cp < 0xd800 || cp > 0xdfff);
    return cp;

  default:  /* 110_____ */
    assert((b0 & 0xe0) == 0xc0);
    assert(!utf8_lead_p(buf[1]));
    cp  = ((uint32_t)(b0     & 0x1f)) << 6;
    cp |=             buf[1] & 0x3f;
    assert (cp > 0x7f); assert (cp < 0x800);
    return cp;
  }
}

/* decode codepoint starting at buf[ofs] */
codepoint_t
hs_text_short_ofs_cp(const uint8_t buf[], const size_t ofs)
{
  return hs_text_short_decode_cp(buf+ofs);
}

/* reverse-decode codepoint starting at offset right after a code-point */
codepoint_t
hs_text_short_ofs_cp_rev(const uint8_t *buf, const size_t ofs)
{
  /*  7 bits | 0xxxxxxx
   * 11 bits | 110yyyyx  10xxxxxx
   * 16 bits | 1110yyyy  10yxxxxx  10xxxxxx
   * 21 bits | 11110yyy  10yyxxxx  10xxxxxx  10xxxxxx
   */

  buf = buf + ofs - 1;

  /* this octet is either 10_ or 0_ */
  uint32_t cp = *buf;

  if (!(cp & 0x80))
    return cp;

  assert (!utf8_lead_p(cp));
  cp &= 0x3f;

  /* this octet is either 10_ or 110_ */
  {
    const uint8_t b = *(--buf);
    assert (!utf8_lead_p(b) || ((b & 0xe0) == 0xc0));

    cp |=  (b & 0x3f) << 6;

    if (b & 0x40) {
      assert (cp > 0x7f); assert (cp < 0x800);
      return cp;
    }
  }

  /* this octet is either 10_ or 1110_ */
  {
    const uint8_t b = *(--buf);
    assert (!utf8_lead_p(b) || ((b & 0xf0) == 0xe0));

    if (b & 0x40) {
      cp |= (b & 0xf) << 12;

      assert (cp > 0x7ff); assert (cp < 0x10000);
      assert (cp < 0xd800 || cp > 0xdfff);
      return cp;
    }

    cp |= (b & 0x3f) << 12;
  }

  /* this octet must be 11110_ */
  const uint8_t b = *(buf-1);
  assert ((b & 0xf8) == 0xf0);

  cp |= (b & 0x7) << 18;

  assert (cp > 0xffff); assert (cp < 0x110000);
  return cp;
}

/* Retrieve i-th code-point in (valid) UTF8 stream
 *
 * Returns -1 if out of bounds
 */
codepoint_t
hs_text_short_index_cp(const uint8_t buf[], const size_t n, const size_t i)
{
  const size_t ofs = hs_text_short_index_ofs(buf, n, i);

  if (ofs >= n)
    return -1;

  return hs_text_short_decode_cp(&buf[ofs]);
}

/* Retrieve i-th code-point in (valid) UTF8 stream
 *
 * Returns -1 if out of bounds
 */
codepoint_t
hs_text_short_index_cp_rev(const uint8_t buf[], const size_t n, const size_t i)
{
  const size_t ofs = hs_text_short_index_ofs_rev(buf, n, i);

  if (ofs >= n)
    return -1;

  return hs_text_short_decode_cp(&buf[ofs]);
}

/* Validate UTF8 encoding

 7 bits | 0xxxxxxx
11 bits | 110yyyyx  10xxxxxx
16 bits | 1110yyyy  10yxxxxx  10xxxxxx
21 bits | 11110yyy  10yyxxxx  10xxxxxx  10xxxxxx

Valid code-points:

 [U+0000 .. U+D7FF] + [U+E000 .. U+10FFFF]

Return values:

  0 -> ok

  1 -> invalid byte/code-point

 -1 -> truncated (1 byte missing)
 -2 -> truncated (2 byte missing)
 -3 -> truncated (3 byte missing)

*/

int
hs_text_short_is_valid_utf8(const uint8_t buf[], const size_t n)
{
  size_t j = 0;

  while (j < n) {
    const uint8_t b0 = buf[j++];

    if (!(b0 & 0x80))
      continue; /* b0 elem [ 0x00 .. 0x7f ] */

    if ((b0 & 0xe0) == 0xc0) { /* [ 0xc0 .. 0xdf ] */
      if (!(b0 & 0x1e)) return 1; /* 0xc0 or 0xc1; denorm */
      if (j >= n) return -1;

      goto l_trail1; /* b1 */
    }

    if ((b0 & 0xf0) == 0xe0) { /* [ 0xe0 .. 0xef ] */
      if ((j+1) >= n) return (n-(j+2));

      const uint8_t b1 = buf[j++];
      if (utf8_lead_p(b1)) return 1; /* b1 elem [ 0x80 .. 0xbf ] */

      /* if b0==0xe0: b1 elem [ 0xa0 .. 0xbf ] */
      if (!((b0 & 0x0f) | (b1 & 0x20))) return 1; /* denorm */

      /* UTF16 Surrogate pairs [U+D800 .. U+DFFF] */
      /* if b0==0xed: b1 elem [ 0x80 .. 0x9f ] */
      if ((b0 == 0xed) && (b1 & 0x20)) return 1;

      goto l_trail1; /* b2 */
    }

    if ((b0 & 0xfc) == 0xf0) { /* [ 0xf0 .. 0xf3 ] */
      if ((j+2) >= n) return (n-(j+3));

      const uint8_t b1 = buf[j++];

      if (utf8_lead_p(b1))         /* b1 elem [ 0x80 .. 0xbf ] */
        return 1;

      if (!((b0 & 0x03) | (b1 & 0x30))) /* if b0==0xf0: b1 elem [ 0x90 .. 0xbf ] */
        return 1;

      goto l_trail2; /* b1, b2 */
    }

    if (b0 == 0xf4) {
      if ((j+2) >= n) return (n-(j+3));

      /* b1 */
      if ((buf[j++] & 0xf0) != 0x80) return 1;
      /* b1 elem [ 0x80 .. 0x8f ] */

    l_trail2:
      /* b2 */
      if (utf8_lead_p(buf[j++])) return 1;
      /* b2 elem [ 0x80 .. 0xbf ] */

    l_trail1:
      /* b3 */
      if (utf8_lead_p(buf[j++])) return 1;
      /* b3 elem [ 0x80 .. 0xbf ] */

      continue;
    }

    /* invalid b0 byte */
    return 1;
  }

  assert(j == n);

  return 0;
}


/* Returns length of longest ASCII-code-point prefix.
 */
size_t
hs_text_short_ascii_length(const uint8_t buf[], const size_t n)
{
  size_t j = 0;

  if (is_64bit) {
    /* "vectorized" optimisation checking 8 octets at once
     *
     * NB: A 64-bit aligned buffer is assumed. This is assumption is
     * justified when the buffer is the payload of a `ByteArray#`.
     */
    const uint64_t *buf64 = (const uint64_t*)buf;

    for (; (j+7) < n; j+=8, ++buf64)
      if (*buf64 & UINT64_C(0x8080808080808080))
        break;
  } else {
    /* "vectorized" optimisation checking 4 octets at once */
    const uint32_t *buf32 = (const uint32_t*)buf;

    for (; (j+3) < n; j+=4, ++buf32)
      if (*buf32 & UINT64_C(0x80808080))
        break;
  }

  for (; j < n; ++j)
    if (buf[j] & 0x80)
      return j;

  return j;
}

/* Test whether well-formed UTF8 string contains only ASCII code-points
 * returns 0 if not ASCII
 *
 * This code assumes a naturally aligned buf[]
 */
int
hs_text_short_is_ascii(const uint8_t buf[], const size_t n)
{
  size_t j = 0;

  if (n < 2)
    return 1;

  if (is_64bit) {
    /* "vectorized" optimisation checking 8 octets at once
     *
     * NB: A 64-bit aligned buffer is assumed. This is assumption is
     * justified when the buffer is the payload of a `ByteArray#`.
     *
     */
    const uint64_t *buf64 = (const uint64_t*)buf;

    for (; (j+7) < n; j+=8, ++buf64)
      if (*buf64 & UINT64_C(0x8080808080808080))
        return 0;

    if (j < n) {
      const int maskshift = (8 - (n - j)) << 3;
      const uint64_t mask = is_bigendian ? (UINT64_C(0x8080808080808080) << maskshift)  /* big endian */
                                         : (UINT64_C(0x8080808080808080) >> maskshift); /* little endian */

      if (*buf64 & mask)
        return 0;
    }
  } else {
    /* "vectorized" optimisation checking 4 octets at once */
    const uint32_t *buf32 = (const uint32_t*)buf;

    for (; (j+3) < n; j+=4, ++buf32)
      if (*buf32 & UINT64_C(0x80808080))
        return 0;

    for (; j < n; ++j)
      if (buf[j] & 0x80)
        return 0;
  }

  return 1;
}

/*
 * Compute length of (transcoded) mutf8 literal
 *
 * If the mutf8 literal does not contain either surrogates nor escaped
 * NULs, a positive length is returned which matches what strlen(3)
 * would have returned.
 *
 * Otherwise, a negated size is returned which corresponds to the size
 * of a the mutf8->utf8 transcoded string.
 *
 */
HsInt
hs_text_short_mutf8_strlen(const uint8_t buf[])
{
  size_t j = 0;
  size_t nulls = 0;
  bool surr_seen = false;

  for (;;) {
    const uint8_t b0 = buf[j];

    if (unlikely(!b0))
      break;

    if (likely(!(b0 & 0x80)))
      j += 1;   /* 0_______ */
    else
      switch(b0 >> 4) {
      case 0xf: /* 11110___ */
        j += 4;
        break;
      case 0xe: /* 1110____ */
        /* UTF16 Surrogate pairs [U+D800 .. U+DFFF] */
        if (unlikely(!surr_seen && (b0 == 0xed) && (buf[j+1] & 0x20)))
          surr_seen = true;
        j += 3;
        break;
      default:  /* 110_____ */
        /* escaped NUL */
        if (unlikely((b0 == 0xc0) && (buf[j+1] == 0x80)))
          nulls += 1;
        j += 2;
        break;
      }
  } /* for */


  if ((nulls > 0) || surr_seen)
    return -(HsInt)(j - nulls);

  return j;
}

/* Transcode Modified UTF-8 to proper UTF-8
 *
 * This involves
 *
 *  1. Unescape denormal 2-byte NULs (0xC0 0x80)
 *  2. Rewrite surrogate pairs to U+FFFD
 */
void
hs_text_short_mutf8_trans(const uint8_t src0[], uint8_t dst0[])
{
  const uint8_t *src = src0;
  uint8_t *dst = dst0;

  for (;;) {
    const uint8_t b0 = *src++;
    assert(utf8_lead_p(b0));

    if (likely(!(b0 & 0x80))) { /* 0_______ */
      if (unlikely(!b0))
        break;

      *dst++ = b0;
      continue;
    }

    switch(b0 >> 4) {
    case 0xf: /* 11110___ */
      assert(!utf8_lead_p(src[0]));
      assert(!utf8_lead_p(src[1]));
      assert(!utf8_lead_p(src[2]));
      *dst++ = b0;
      *dst++ = *src++;
      *dst++ = *src++;
      *dst++ = *src++;
      break;

    case 0xe: { /* 1110____ */
      const uint8_t b1 = *src++;
      const uint8_t b2 = *src++;
      assert(!utf8_lead_p(b1));
      assert(!utf8_lead_p(b2));
      if (unlikely((b0 == 0xed) && (b1 & 0x20))) {
        /* UTF16 Surrogate pairs [U+D800 .. U+DFFF]
         * -> translate into U+FFFD
         */
        *dst++ = 0xef;
        *dst++ = 0xbf;
        *dst++ = 0xbd;
      } else {
        *dst++ = b0;
        *dst++ = b1;
        *dst++ = b2;
      }
      break;
    }
    default: { /* 110_____ */
      const uint8_t b1 = *src++;
      assert(!utf8_lead_p(b1));
      if (unlikely((b0 == 0xc0) && (b1 == 0x80))) {
        /* escaped/denormal U+0000 -> normalize */
        *dst++ = 0x00;
      } else {
        *dst++ = b0;
        *dst++ = b1;
      }
      break;
    }
    } /* switch */
  } /* for */

  assert(labs(hs_text_short_mutf8_strlen(src0)) == (dst - dst0));
}
