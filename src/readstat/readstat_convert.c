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


#include <errno.h>
#include "readstat.h"
#include "readstat_iconv.h"
#include "readstat_convert.h"

readstat_error_t readstat_convert(char *dst, size_t dst_len, const char *src, size_t src_len, iconv_t converter) {
    /* strip off spaces from the input because the programs use ASCII space
     * padding even with non-ASCII encoding. */
    while (src_len && (src[src_len-1] == ' ' || src[src_len-1] == '\0')) {
        src_len--;
    }
    if (dst_len == 0) {
        return READSTAT_ERROR_CONVERT_LONG_STRING;
    } else if (converter) {
        size_t dst_left = dst_len - 1;
        char *dst_end = dst;
        size_t status = iconv(converter, (readstat_iconv_inbuf_t)&src, &src_len, &dst_end, &dst_left);
        if (status == (size_t)-1) {
            if (errno == E2BIG) {
                return READSTAT_ERROR_CONVERT_LONG_STRING;
            } else if (errno == EILSEQ) {
                return READSTAT_ERROR_CONVERT_BAD_STRING;
            } else if (errno != EINVAL) { /* EINVAL indicates improper truncation; accept it */
                return READSTAT_ERROR_CONVERT;
            }
        }
        dst[dst_len - dst_left - 1] = '\0';
    } else if (src_len + 1 > dst_len) {
        return READSTAT_ERROR_CONVERT_LONG_STRING;
    } else {
        memcpy(dst, src, src_len);
        dst[src_len] = '\0';
    }
    return READSTAT_OK;
}
