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


#define READSTAT_PRODUCT_NAME       "ReadStat"
#define READSTAT_PRODUCT_URL        "https://github.com/WizardMac/ReadStat"

readstat_error_t readstat_begin_writing_file(readstat_writer_t *writer, void *user_ctx, long row_count);

readstat_error_t readstat_write_bytes(readstat_writer_t *writer, const void *bytes, size_t len);
readstat_error_t readstat_write_bytes_as_lines(readstat_writer_t *writer,
        const void *bytes, size_t len, size_t line_len, const char *line_sep);
readstat_error_t readstat_write_line_padding(readstat_writer_t *writer, char pad,
        size_t line_len, const char *line_sep);

readstat_error_t readstat_write_zeros(readstat_writer_t *writer, size_t len);
readstat_error_t readstat_write_spaces(readstat_writer_t *writer, size_t len);
readstat_error_t readstat_write_string(readstat_writer_t *writer, const char *bytes);
readstat_error_t readstat_write_space_padded_string(readstat_writer_t *writer, const char *string, size_t max_len);
readstat_value_label_t *readstat_get_value_label(readstat_label_set_t *label_set, int index);
readstat_label_set_t *readstat_get_label_set(readstat_writer_t *writer, int index);
readstat_variable_t *readstat_get_label_set_variable(readstat_label_set_t *label_set, int index);
void readstat_sort_label_set(readstat_label_set_t *label_set,
        int (*compare)(const readstat_value_label_t *, const readstat_value_label_t *));
