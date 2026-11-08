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


typedef enum {
    LABEL_TYPE_NAN = -1,
    LABEL_TYPE_DOUBLE,
    LABEL_TYPE_STRING,
    LABEL_TYPE_RANGE,
    LABEL_TYPE_OTHER
} label_type_t;

readstat_error_t submit_columns(readstat_parser_t *parser, readstat_schema_t *dct, void *user_ctx);
/* A value label range such as "1 - 10" is expanded into one label per
 * integer. Cap the total number of labels a single file may expand to so a
 * range like "0 - 99999999999999999" cannot spin for hours. */
#define MAX_VALUE_LABEL_RANGE_VALUES (1 << 20)

readstat_error_t submit_value_label(readstat_parser_t *parser, const char *labelset,
        label_type_t label_type, int64_t first_integer, int64_t last_integer,
        double double_value, const char *string_value, const char *buf,
        uint64_t *range_values_remaining, void *user_ctx);
