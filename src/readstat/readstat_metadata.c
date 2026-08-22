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

#include "readstat.h"

int readstat_get_row_count(readstat_metadata_t *metadata) {
    return metadata->row_count;
}

int readstat_get_var_count(readstat_metadata_t *metadata) {
    return metadata->var_count;
}

time_t readstat_get_creation_time(readstat_metadata_t *metadata) {
    return metadata->creation_time;
}

time_t readstat_get_modified_time(readstat_metadata_t *metadata) {
    return metadata->modified_time;
}

int readstat_get_file_format_version(readstat_metadata_t *metadata) {
    return metadata->file_format_version;
}

int readstat_get_file_format_is_64bit(readstat_metadata_t *metadata) {
    return metadata->is64bit;
}

readstat_compress_t readstat_get_compression(readstat_metadata_t *metadata) {
    return metadata->compression;
}

readstat_endian_t readstat_get_endianness(readstat_metadata_t *metadata) {
    return metadata->endianness;
}

const char *readstat_get_file_label(readstat_metadata_t *metadata) {
    return metadata->file_label;
}

const char *readstat_get_file_encoding(readstat_metadata_t *metadata) {
    return metadata->file_encoding;
}

const char *readstat_get_table_name(readstat_metadata_t *metadata) {
    return metadata->table_name;
}

size_t readstat_get_multiple_response_sets_length(readstat_metadata_t *metadata) {
    return metadata->multiple_response_sets_length;
}

const mr_set_t *readstat_get_multiple_response_sets(readstat_metadata_t *metadata) {
    return metadata->mr_sets;
}
