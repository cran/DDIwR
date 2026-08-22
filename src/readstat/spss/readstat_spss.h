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


#define SPSS_FORMAT_TYPE_A        1
#define SPSS_FORMAT_TYPE_AHEX     2
#define SPSS_FORMAT_TYPE_COMMA    3
#define SPSS_FORMAT_TYPE_DOLLAR   4
#define SPSS_FORMAT_TYPE_F        5
#define SPSS_FORMAT_TYPE_IB       6
#define SPSS_FORMAT_TYPE_PIBHEX   7
#define SPSS_FORMAT_TYPE_P        8
#define SPSS_FORMAT_TYPE_PIB      9
#define SPSS_FORMAT_TYPE_PK       10
#define SPSS_FORMAT_TYPE_RB       11
#define SPSS_FORMAT_TYPE_RBHEX    12
#define SPSS_FORMAT_TYPE_Z        15
#define SPSS_FORMAT_TYPE_N        16
#define SPSS_FORMAT_TYPE_E        17
#define SPSS_FORMAT_TYPE_DATE     20
#define SPSS_FORMAT_TYPE_TIME     21
#define SPSS_FORMAT_TYPE_DATETIME 22
#define SPSS_FORMAT_TYPE_ADATE    23
#define SPSS_FORMAT_TYPE_JDATE    24
#define SPSS_FORMAT_TYPE_DTIME    25
#define SPSS_FORMAT_TYPE_WKDAY    26
#define SPSS_FORMAT_TYPE_MONTH    27
#define SPSS_FORMAT_TYPE_MOYR     28
#define SPSS_FORMAT_TYPE_QYR      29
#define SPSS_FORMAT_TYPE_WKYR     30
#define SPSS_FORMAT_TYPE_PCT      31
#define SPSS_FORMAT_TYPE_DOT      32
#define SPSS_FORMAT_TYPE_CCA      33
#define SPSS_FORMAT_TYPE_CCB      34
#define SPSS_FORMAT_TYPE_CCC      35
#define SPSS_FORMAT_TYPE_CCD      36
#define SPSS_FORMAT_TYPE_CCE      37
#define SPSS_FORMAT_TYPE_EDATE    38
#define SPSS_FORMAT_TYPE_SDATE    39
#define SPSS_FORMAT_TYPE_MTIME    40
#define SPSS_FORMAT_TYPE_YMDHMS   41

#define SPSS_DOC_LINE_SIZE  80

#define SAV_HIGHEST_DOUBLE   0x7FEFFFFFFFFFFFFFUL
#define SAV_MISSING_DOUBLE   0xFFEFFFFFFFFFFFFFUL
#define SAV_LOWEST_DOUBLE    0xFFEFFFFFFFFFFFFEUL

#define SAV_MEASURE_UNKNOWN     0
#define SAV_MEASURE_NOMINAL     1
#define SAV_MEASURE_ORDINAL     2
#define SAV_MEASURE_SCALE       3

#define SAV_ALIGNMENT_LEFT      0
#define SAV_ALIGNMENT_RIGHT     1
#define SAV_ALIGNMENT_CENTER    2

#include <iconv.h>

typedef struct spss_format_s {
    int          type;
    int          width;
    int          decimal_places;
} spss_format_t;

// The reason some fields are stored unconverted is that some versions of SPSS
// store truncated UTF-8 in the fields, and also use the truncated strings for
// internal logic (such as matching names). If we convert them too early, the
// last character of a truncated string will be dropped, and some of the column
// information won't be found (e.g. in the key=value long variable record).
typedef struct spss_varinfo_s {
    readstat_type_t  type;
    int              labels_index;
    int              index;
    int              offset;
    int              width;
    unsigned int     string_length;
    spss_format_t    print_format;
    spss_format_t    write_format;
    int              n_segments;
    int              n_missing_values;
    int              missing_range;
    double           missing_double_values[3];
    char             missing_string_values[3][8*4+1]; // stored UTF-8
    char             name[8+1]; // stored UNCONVERTED
    char             longname[64+1]; // stored UNCONVERTED
    char            *label; // stored UTF-8
    readstat_measure_t      measure;
    readstat_alignment_t    alignment;
    int                     display_width;
} spss_varinfo_t;

int spss_format(char *buffer, size_t len, spss_format_t *format);
int spss_varinfo_compare(const void *elem1, const void *elem2);

void spss_varinfo_free(spss_varinfo_t *info);

readstat_missingness_t spss_missingness_for_info(spss_varinfo_t *info);
readstat_variable_t *spss_init_variable_for_info(spss_varinfo_t *info,
        int index_after_skipping, iconv_t converter);

uint64_t spss_64bit_value(readstat_value_t value);

uint32_t spss_measure_from_readstat_measure(readstat_measure_t measure);
readstat_measure_t spss_measure_to_readstat_measure(uint32_t sav_measure);
uint32_t spss_alignment_from_readstat_alignment(readstat_alignment_t alignment);
readstat_alignment_t spss_alignment_to_readstat_alignment(uint32_t sav_alignment);
readstat_error_t spss_format_for_variable(readstat_variable_t *r_variable,
        spss_format_t *spss_format);
