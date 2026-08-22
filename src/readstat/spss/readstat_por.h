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


extern int8_t   por_ascii_lookup[256];
extern uint16_t por_unicode_lookup[256];

typedef struct por_ctx_s {
    readstat_callbacks_t    handle;
    size_t                  file_size;
    void                   *user_ctx;

    int            pos;
    readstat_io_t *io;
    char           space;
    long           num_spaces;
    time_t         timestamp;
    long           version;
    char           fweight_name[9];
    char           file_label[21];
    uint16_t       byte2unicode[256];
    size_t         base30_precision;
    iconv_t        converter;
    unsigned char *string_buffer;
    size_t         string_buffer_len;
    int            labels_offset;
    int            obs_count;
    int            var_count;
    int            var_offset;
    int            row_limit;
    int            row_offset;
    readstat_variable_t **variables;
    spss_varinfo_t *varinfo;
    ck_hash_table_t *var_dict;
} por_ctx_t;

por_ctx_t *por_ctx_init(void);
void por_ctx_free(por_ctx_t *ctx);
ssize_t por_utf8_encode(const unsigned char *input, size_t input_len, 
        char *output, size_t output_len, uint16_t lookup[256]);
ssize_t por_utf8_decode(
        const char *input, size_t input_len,
        char *output, size_t output_len,
        uint8_t *lookup, size_t lookup_len);
