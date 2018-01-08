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
 2 -> truncated

*/

int
hs_text_short_is_valid_utf8(const uint8_t buf[], const size_t n)
{
  size_t j = 0;

  while (j < n) {
    const uint8_t b0 = buf[j++];

    if (!(b0 & 0x80))
      continue;

    if ((b0 & 0xe0) == 0xc0) {
      if (!(b0 & 0x1e)) return 1; /* denorm */
      if (j >= n) return 2;
      
      /* b1 */
      if ((buf[j++] & 0xc0) != 0x80) return 1;
      continue;
    }

    if ((b0 & 0xf0) == 0xe0) {
      if ((j+1) >= n) return 2;

      const uint8_t b1 = buf[j++];
      if ((b1 & 0xc0) != 0x80) return 1;
      if (!((b0 & 0x0f) | (b1 & 0x20))) return 1; /* denorm */
      /* UTF16 Surrogate pairs [U+D800 .. U+DFFF] */
      if ((b0 == 0xed) && (b1 & 0x20)) return 1;
      
      /* b2 */
      if ((buf[j++] & 0xc0) != 0x80) return 1;
        
      continue;
    }

    if ((b0 & 0xf8) == 0xf0) {
      if ((j+2) >= n) return 2;
      
      const uint8_t b1 = buf[j++];
      if ((b1 & 0xc0) != 0x80) return 1;
      if (!((b0 & 0x07) | (b1 & 0x30))) return 1; /* denorm */
      /* make sure we're below U+10FFFF */
      if (b0 > 0xf4) return 1;
      if ((b0 == 0xf4) && (b1 & 0x30)) return 1;
      
      /* b2 */
      if ((buf[j++] & 0xc0) != 0x80) return 1;
      /* b3 */
      if ((buf[j++] & 0xc0) != 0x80) return 1;

      continue;
    }
    
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
  size_t j;
  for (j = 0; j < n; j++)
    if (buf[j] & 0x80)
      return j;
  return j;
}
