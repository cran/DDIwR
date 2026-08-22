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

#ifndef DECLARED_TYPES_H
#define DECLARED_TYPES_H

#include <R.h>
#include <Rinternals.h>

typedef enum {
    DECLARED_SPSS = 0,
    DECLARED_STATA = 1,
    DECLARED_SAS = 2
} FileVendor;

typedef enum {
    DECLARED_SAV = 0,
    DECLARED_POR = 1,
    DECLARED_DTA = 2,
    DECLARED_SAS7BDAT = 3,
    DECLARED_SAS7BCAT = 4,
    DECLARED_XPT = 5
} FileExt;

typedef enum {
    DECLARED_DEFAULT = 0,
    DECLARED_DATE = 1,
    DECLARED_TIME = 2,
    DECLARED_DATETIME = 3
} VarType;

FileVendor extVendor(FileExt ext);
const char *formatAttribute(FileVendor vendor);
int hasPrefix(const char *x, const char *prefix);
VarType numTypeFromSEXP(SEXP x);
VarType numTypeFromFormat(FileVendor vendor, const char *var_format);
double adjustDatetimeToR(FileVendor vendor, VarType var, double value);
double adjustDatetimeFromR(FileVendor vendor, SEXP col, double value);

#endif
