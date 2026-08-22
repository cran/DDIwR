/*
Copyright (c) 2026, Adrian Dusa
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, in whole or in part, are permitted provided that the
following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * The names of its contributors may NOT be used to endorse or promote
      products derived from this software without specific prior written
      permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#define R_NO_REMAP
#include <R.h>
#include <ctype.h>

// Scalar operators -------------------------------------------------------

// IEEE 754 defines binary64 as
// * 1  bit : sign
// * 11 bits: exponent
// * 52 bits: significand
//
// R stores the value "1954" in the last 32 bits: this payload marks
// the value as a NA, not a regular NaN.
//
// (Note that this discussion like most discussion of FP on the web, assumes
// a big-endian architecture - in little endian the sign bit is the last
// bit)

typedef union {
  double value;           // 8 bytes
  char byte[8];           // 8 * 1 bytes
} ieee_double;


#ifdef WORDS_BIGENDIAN
// First two bytes are sign & expoonent
// Last four bytes are 1954
const int TAG_BYTE = 3;
#else
const int TAG_BYTE = 4;
#endif

double make_tagged_na(char x) {
  ieee_double y;

  y.value = NA_REAL;
  y.byte[TAG_BYTE] = x;

  return y.value;
}

char tagged_na_value(double x) {
  ieee_double y;
  y.value = x;

  return y.byte[TAG_BYTE];
}
