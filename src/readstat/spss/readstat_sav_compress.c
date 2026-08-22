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

#include <string.h>
#include <stdint.h>

#include "../readstat.h"
#include "../readstat_bits.h"
#include "../readstat_iconv.h"
#include "readstat_sav.h"
#include "readstat_sav_compress.h"

size_t sav_compressed_row_bound(size_t uncompressed_length) {
    return uncompressed_length + (uncompressed_length/8 + 8)/8*8;
}

size_t sav_compress_row(void *output_row, void *input_row, size_t input_len,
        readstat_writer_t *writer) {
    unsigned char *output = output_row;
    unsigned char *input = input_row;
    off_t input_offset = 0;

    off_t output_offset = 8;
    off_t control_offset = 0;
    int i;

    memset(&output[control_offset], 0, 8);

    for (i=0; i<writer->variables_count; i++) {
        readstat_variable_t *variable = readstat_get_variable(writer, i);
        if (variable->type == READSTAT_TYPE_STRING) {
            size_t width = variable->storage_width;
            while (width > 0) {
                if (memcmp(&input[input_offset], SAV_EIGHT_SPACES, 8) == 0) {
                    output[control_offset++] = 254;
                } else {
                    output[control_offset++] = 253;
                    memcpy(&output[output_offset], &input[input_offset], 8);
                    output_offset += 8;
                }
                if (control_offset % 8 == 0) {
                    control_offset = output_offset;
                    memset(&output[control_offset], 0, 8);
                    output_offset += 8;
                }
                input_offset += 8;
                width -= 8;
            }
        } else {
            uint64_t int_value;
            memcpy(&int_value, &input[input_offset], 8);
            if (int_value == SAV_MISSING_DOUBLE) {
                output[control_offset++] = 255;
            } else {
                double fp_value;
                memcpy(&fp_value, &input[input_offset], 8);
                if (fp_value > -100 && fp_value < 152 && (int)fp_value == fp_value) {
                    output[control_offset++] = (int)fp_value + 100;
                } else {
                    output[control_offset++] = 253;
                    memcpy(&output[output_offset], &input[input_offset], 8);
                    output_offset += 8;
                }
            }
            if (control_offset % 8 == 0) {
                control_offset = output_offset;
                memset(&output[control_offset], 0, 8);
                output_offset += 8;
            }
            input_offset += 8;
        }
    }

    if (writer->current_row + 1 == writer->row_count)
        output[control_offset] = 252;

    return output_offset;
}

void sav_decompress_row(struct sav_row_stream_s *state) {
    double fp_value;
    uint64_t missing_value = state->bswap ? byteswap8(state->missing_value) : state->missing_value;
    int i = 8 - state->i;
    while (1) {
        if (i == 8) {
            if (state->avail_in < 8) {
                state->status = SAV_ROW_STREAM_NEED_DATA;
                goto done;
            }

            memcpy(state->chunk, state->next_in, 8);
            state->next_in += 8;
            state->avail_in -= 8;
            i = 0;
        }

        while (i<8) {
            switch (state->chunk[i]) {
                case 0:
                    break;
                case 252:
                    state->status = SAV_ROW_STREAM_FINISHED_ALL;
                    goto done;
                case 253:
                    if (state->avail_in < 8) {
                        state->status = SAV_ROW_STREAM_NEED_DATA;
                        goto done;
                    }
                    memcpy(state->next_out, state->next_in, 8);
                    state->next_out += 8;
                    state->avail_out -= 8;
                    state->next_in += 8;
                    state->avail_in -= 8;
                    break;
                case 254:
                    memset(state->next_out, ' ', 8);
                    state->next_out += 8;
                    state->avail_out -= 8;
                    break;
                case 255:
                    memcpy(state->next_out, &missing_value, sizeof(uint64_t));
                    state->next_out += 8;
                    state->avail_out -= 8;
                    break;
                default:
                    fp_value = state->chunk[i] - state->bias;
                    fp_value = state->bswap ? byteswap_double(fp_value) : fp_value;
                    memcpy(state->next_out, &fp_value, sizeof(double));
                    state->next_out += 8;
                    state->avail_out -= 8;
                    break;
            }
            i++;
            if (state->avail_out < 8) {
                state->status = SAV_ROW_STREAM_FINISHED_ROW;
                goto done;
            }
        }
    }
done:
    state->i = 8 - i;
}
