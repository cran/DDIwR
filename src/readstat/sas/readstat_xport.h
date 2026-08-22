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


typedef struct xport_header_record_s {
    char    name[9];

    int     num1;
    int     num2;
    int     num3;
    int     num4;
    int     num5;
    int     num6;
} xport_header_record_t;

extern char _xport_months[12][4];

#pragma pack(push, 1)
typedef struct xport_namestr_s {
    uint16_t    ntype;
    uint16_t    nhfun;
    uint16_t    nlng;
    uint16_t    nvar0;
    char        nname[8];
    char        nlabel[40];
    char        nform[8];
    uint16_t    nfl;
    uint16_t    nfd;
    uint16_t    nfj;
    char        nfill[2];
    char        niform[8];
    uint16_t    nifl;
    uint16_t    nifd;
    uint32_t    npos;
    char        longname[32];
    uint16_t    labeln;
    char        rest[18];
} xport_namestr_t;
#pragma pack(pop)

typedef struct xport_format_s {
    char         name[32];
    int          width;
    int          decimals;
} xport_format_t;

#define XPORT_MIN_DOUBLE_SIZE   3
#define XPORT_MAX_DOUBLE_SIZE   8

void xport_namestr_bswap(xport_namestr_t *namestr);
