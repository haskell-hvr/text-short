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

#include <stdint.h>
#include <string.h>
#include <assert.h>

/* Count number of code-points in well-formed utf8 string */
size_t
hs_text_short_length(const uint8_t buf[], const size_t n)
{
  size_t j,l = 0;
  for (j = 0; j < n; j++)
    if ((buf[j] & 0xc0) != 0x80)
      l++;

  return l;
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

      /* b1 */
      if ((buf[j++] & 0xc0) != 0x80) return 1;
      /* b1 elem [ 0x80 .. 0xbf ] */

      continue;
    }

    if ((b0 & 0xf0) == 0xe0) { /* [ 0xe0 .. 0xef ] */
      if ((j+1) >= n) return (n-(j+2));

      const uint8_t b1 = buf[j++];
      if ((b1 & 0xc0) != 0x80) return 1; /* b1 elem [ 0x80 .. 0xbf ] */

      /* if b0==0xe0: b1 elem [ 0xa0 .. 0xbf ] */
      if (!((b0 & 0x0f) | (b1 & 0x20))) return 1; /* denorm */

      /* UTF16 Surrogate pairs [U+D800 .. U+DFFF] */
      /* if b0==0xed: b1 elem [ 0x80 .. 0x9f ] */
      if ((b0 == 0xed) && (b1 & 0x20)) return 1;

      /* b2 */
      if ((buf[j++] & 0xc0) != 0x80) return 1;
      /* b2 elem [ 0x80 .. 0xbf ] */

      continue;
    }

    if ((b0 & 0xfc) == 0xf0) { /* [ 0xf0 .. 0xf3 ] */
      if ((j+2) >= n) return (n-(j+3));

      const uint8_t b1 = buf[j++];

      if ((b1 & 0xc0) != 0x80)         /* b1 elem [ 0x80 .. 0xbf ] */
        return 1;

      if (!((b0 & 0x03) | (b1 & 0x30))) /* if b0==0xf0: b1 elem [ 0x90 .. 0xbf ] */
        return 1;

      /* b2 */
      if ((buf[j++] & 0xc0) != 0x80) return 1;
      /* b2 elem [ 0x80 .. 0xbf ] */

      /* b3 */
      if ((buf[j++] & 0xc0) != 0x80) return 1;
      /* b3 elem [ 0x80 .. 0xbf ] */

      continue;
    }

    if (b0 == 0xf4) {
      if ((j+2) >= n) return (n-(j+3));

      /* b1 */
      if ((buf[j++] & 0x80) != 0x80) return 1;
      /* b1 elem [ 0x80 .. 0x8f ] */

      /* b2 */
      if ((buf[j++] & 0xc0) != 0x80) return 1;
      /* b2 elem [ 0x80 .. 0xbf ] */

      /* b3 */
      if ((buf[j++] & 0xc0) != 0x80) return 1;
      /* b3 elem [ 0x80 .. 0xbf ] */

      continue;
    }

    /* invalid b0 byte */
    return 1;
  }

  assert(j == n);

  return 0;
}


/* Test whether well-formed UTF8 string contains only ASCII code-points
 * Returns length of longest ASCII-code-point prefix.
 */
size_t
hs_text_short_is_ascii(const uint8_t buf[], const size_t n)
{
  size_t j = 0;

  if (sizeof(long) == 8) {
    /* "vectorized" optimisation checking 8 octets at once
     *
     * NB: A 64-bit aligned buffer is assumed. This is assumption is
     * justified when the buffer is the payload of a `ByteArray#`.
     */
    const uint64_t *buf64 = (const uint64_t*)buf;

    for (; (j+7) < n; j+=8, ++buf64)
      if (*buf64 & UINT64_C(0x8080808080808080))
        break;
  }

  for (; j < n; ++j)
    if (buf[j] & 0x80)
      return j;

  return j;
}
