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


typedef struct zsav_block_s {
    int32_t        uncompressed_size;
    int32_t        compressed_size;

    z_stream       stream;

    unsigned char *compressed_data;
    size_t         compressed_data_capacity;
} zsav_block_t;

typedef struct zsav_ctx_s {
    void           *buffer;
    zsav_block_t  **blocks;
    int             blocks_count;
    int             blocks_capacity;

    int64_t         uncompressed_block_size;
    int64_t         zheader_ofs;

    int             compression_level;
} zsav_ctx_t;

zsav_ctx_t *zsav_ctx_init(size_t max_row_len, int64_t offset);
void zsav_ctx_free(zsav_ctx_t *ctx);

zsav_block_t *zsav_add_block(zsav_ctx_t *ctx);
zsav_block_t *zsav_current_block(zsav_ctx_t *ctx);
int zsav_compress_row(void *input, size_t input_len, int finish, zsav_ctx_t *zctx);
