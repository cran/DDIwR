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

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

void readstat_copy(char *buf, size_t buf_len, const char *str_start, size_t str_len) {
    size_t this_len = str_len;
    if (this_len >= buf_len) {
        this_len = buf_len - 1;
    }
    memcpy(buf, str_start, this_len);
    buf[this_len] = '\0';
}

void readstat_copy_lower(char *buf, size_t buf_len, const char *str_start, size_t str_len) {
    int i;
    readstat_copy(buf, buf_len, str_start, str_len);
    for (i=0; i<buf_len && buf[i]; i++)
        buf[i] = tolower(buf[i]);
}

void readstat_copy_quoted(char *buf, size_t buf_len, const char *str_start, size_t str_len) {
    size_t this_len = str_len;
    if (this_len >= buf_len) {
        this_len = buf_len - 1;
    }
    size_t i=0;
    size_t j=0;
    int slash = 0;
    for (i=0; i<this_len; i++) {
        if (slash) {
            if (str_start[i] == 't') {
                buf[j++] = '\t';
            } else {
                buf[j++] = str_start[i];
            }
            slash = 0;
        } else {
            if (str_start[i] == '\\') {
                slash = 1;
            } else {
                buf[j++] = str_start[i];
            }
        }
    }
    buf[j] = '\0';
}

