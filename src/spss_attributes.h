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

#ifndef DDIWR_SPSS_ATTRIBUTES_H
#define DDIWR_SPSS_ATTRIBUTES_H

#include <ctype.h>
#include <string.h>

/* SPSS names are case insensitive. Preserve the attribute spelling while
 * matching scopes to the names supplied by the variable dictionary. */
static int ddiwr_spss_same_name(const char *first, const char *second) {
    while (*first && *second) {
        unsigned char a = (unsigned char)*first++;
        unsigned char b = (unsigned char)*second++;
        if (a < 128 && b < 128) {
            a = (unsigned char)toupper(a);
            b = (unsigned char)toupper(b);
        }
        if (a != b) {
            return 0;
        }
    }
    return *first == *second;
}

static void ddiwr_attach_spss_attributes(SEXP output, SEXP column_names,
        const readstat_attribute_t *attributes) {
    const readstat_attribute_t *attribute;
    for (attribute = attributes; attribute; attribute = attribute->next) {
        SEXP target = output;
        SEXP previous, values, names, combined;
        R_xlen_t i, n;
        if (attribute->variable) {
            target = R_NilValue;
            for (i = 0; i < XLENGTH(column_names); i++) {
                if (ddiwr_spss_same_name(attribute->variable, CHAR(STRING_ELT(column_names, i)))) {
                    target = VECTOR_ELT(output, i);
                    break;
                }
            }
            /* A deliberately skipped column does not receive attributes. */
            if (target == R_NilValue) {
                continue;
            }
        }
        previous = Rf_getAttrib(target, Rf_install("spss.attributes"));
        n = previous == R_NilValue ? 0 : XLENGTH(previous);
        PROTECT(combined = Rf_allocVector(VECSXP, n + 1));
        PROTECT(names = Rf_allocVector(STRSXP, n + 1));
        PROTECT(values = Rf_allocVector(STRSXP, attribute->count));
        for (i = 0; i < n; i++) {
            SET_VECTOR_ELT(combined, i, VECTOR_ELT(previous, i));
            SET_STRING_ELT(names, i, STRING_ELT(Rf_getAttrib(previous, R_NamesSymbol), i));
        }
        for (i = 0; i < (R_xlen_t)attribute->count; i++) {
            SET_STRING_ELT(values, i, Rf_mkCharCE(attribute->values[i], CE_UTF8));
        }
        SET_VECTOR_ELT(combined, n, values);
        SET_STRING_ELT(names, n, Rf_mkCharCE(attribute->name, CE_UTF8));
        Rf_setAttrib(combined, R_NamesSymbol, names);
        Rf_setAttrib(target, Rf_install("spss.attributes"), combined);
        UNPROTECT(3);
    }
}

#endif
