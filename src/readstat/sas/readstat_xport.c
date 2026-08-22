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

#include <stdint.h>

#include "readstat_xport.h"
#include "../readstat_bits.h"

char _xport_months[12][4] = {
    "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" };

void xport_namestr_bswap(xport_namestr_t *namestr) {
    if (!machine_is_little_endian())
        return;

    namestr->ntype = byteswap2(namestr->ntype);
    namestr->nhfun = byteswap2(namestr->nhfun);
    namestr->nlng = byteswap2(namestr->nlng);
    namestr->nvar0 = byteswap2(namestr->nvar0);

    namestr->nfl = byteswap2(namestr->nfl);
    namestr->nfd = byteswap2(namestr->nfd);
    namestr->nfj = byteswap2(namestr->nfj);

    namestr->nifl = byteswap2(namestr->nifl);
    namestr->nifd = byteswap2(namestr->nifd);
    namestr->npos = byteswap4(namestr->npos);

    namestr->labeln  = byteswap2(namestr->labeln);
}
