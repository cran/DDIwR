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

enum sav_row_stream_status {
    SAV_ROW_STREAM_NEED_DATA,
    SAV_ROW_STREAM_HAVE_DATA,
    SAV_ROW_STREAM_FINISHED_ROW,
    SAV_ROW_STREAM_FINISHED_ALL
};
struct sav_row_stream_s {
    const unsigned char  *next_in;
    size_t                avail_in;

    unsigned char        *next_out;
    size_t                avail_out;

    uint64_t              missing_value;
    double                bias;

    unsigned char         chunk[8];
    int                   i;
    int                   bswap;

    enum sav_row_stream_status status;
};

size_t sav_compressed_row_bound(size_t uncompressed_length);
size_t sav_compress_row(void *output_row, void *input_row, size_t input_len,
        readstat_writer_t *writer);
void sav_decompress_row(struct sav_row_stream_s *state);
