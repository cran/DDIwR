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

readstat_type_class_t readstat_type_class(readstat_type_t type) {
    if (type == READSTAT_TYPE_STRING || type == READSTAT_TYPE_STRING_REF)
        return READSTAT_TYPE_CLASS_STRING;

    return READSTAT_TYPE_CLASS_NUMERIC;
}

readstat_type_t readstat_value_type(readstat_value_t value) {
    return value.type;
}

readstat_type_class_t readstat_value_type_class(readstat_value_t value) {
    return readstat_type_class(value.type);
}

char readstat_value_tag(readstat_value_t value) {
    return value.tag;
}

int readstat_value_is_missing(readstat_value_t value, readstat_variable_t *variable) {
    if (value.is_system_missing || value.is_tagged_missing)
        return 1;

    if (variable)
        return readstat_value_is_defined_missing(value, variable);

    return 0;
}

int readstat_value_is_system_missing(readstat_value_t value) {
    return (value.is_system_missing);
}

int readstat_value_is_tagged_missing(readstat_value_t value) {
    return (value.is_tagged_missing);
}

static int readstat_double_is_defined_missing(double fp_value, readstat_variable_t *variable) {
    int count = readstat_variable_get_missing_ranges_count(variable);
    int i;
    for (i=0; i<count; i++) {
        double lo = readstat_double_value(readstat_variable_get_missing_range_lo(variable, i));
        double hi = readstat_double_value(readstat_variable_get_missing_range_hi(variable, i));
        if (fp_value >= lo && fp_value <= hi) {
            return 1;
        }
    }
    return 0;
}

static int readstat_string_is_defined_missing(const char *string, readstat_variable_t *variable) {
    if (string == NULL)
        return 0;

    int count = readstat_variable_get_missing_ranges_count(variable);
    int i;
    for (i=0; i<count; i++) {
        const char *lo = readstat_string_value(readstat_variable_get_missing_range_lo(variable, i));
        const char *hi = readstat_string_value(readstat_variable_get_missing_range_hi(variable, i));
        if (lo && hi && strcmp(string, lo) >= 0 && strcmp(string, hi) <= 0) {
            return 1;
        }
    }
    return 0;
}

int readstat_value_is_defined_missing(readstat_value_t value, readstat_variable_t *variable) {
    if (readstat_value_type_class(value) != readstat_variable_get_type_class(variable))
        return 0;

    if (readstat_value_type_class(value) == READSTAT_TYPE_CLASS_STRING)
        return readstat_string_is_defined_missing(readstat_string_value(value), variable);

    if (readstat_value_type_class(value) == READSTAT_TYPE_CLASS_NUMERIC)
        return readstat_double_is_defined_missing(readstat_double_value(value), variable);

    return 0;
}

char readstat_int8_value(readstat_value_t value) {
    if (readstat_value_is_system_missing(value))
        return 0;

    if (value.type == READSTAT_TYPE_DOUBLE)
        return (char)value.v.double_value;
    if (value.type == READSTAT_TYPE_FLOAT)
        return (char)value.v.float_value;
    if (value.type == READSTAT_TYPE_INT32)
        return (char)value.v.i32_value;
    if (value.type == READSTAT_TYPE_INT16)
        return (char)value.v.i16_value;
    if (value.type == READSTAT_TYPE_INT8)
        return value.v.i8_value;

    return 0;
}

int16_t readstat_int16_value(readstat_value_t value) {
    if (readstat_value_is_system_missing(value))
        return 0;

    if (value.type == READSTAT_TYPE_DOUBLE)
        return (int16_t)value.v.double_value;
    if (value.type == READSTAT_TYPE_FLOAT)
        return (int16_t)value.v.float_value;
    if (value.type == READSTAT_TYPE_INT32)
        return (int16_t)value.v.i32_value;
    if (value.type == READSTAT_TYPE_INT16)
        return value.v.i16_value;
    if (value.type == READSTAT_TYPE_INT8)
        return value.v.i8_value;

    return 0;
}

int32_t readstat_int32_value(readstat_value_t value) {
    if (readstat_value_is_system_missing(value))
        return 0;

    if (value.type == READSTAT_TYPE_DOUBLE)
        return (int32_t)value.v.double_value;
    if (value.type == READSTAT_TYPE_FLOAT)
        return (int32_t)value.v.float_value;
    if (value.type == READSTAT_TYPE_INT32)
        return value.v.i32_value;
    if (value.type == READSTAT_TYPE_INT16)
        return value.v.i16_value;
    if (value.type == READSTAT_TYPE_INT8)
        return value.v.i8_value;

    return 0;
}

float readstat_float_value(readstat_value_t value) {
    if (readstat_value_is_system_missing(value))
        return NAN;

    if (value.type == READSTAT_TYPE_DOUBLE)
        return (float)value.v.double_value;
    if (value.type == READSTAT_TYPE_FLOAT)
        return value.v.float_value;
    if (value.type == READSTAT_TYPE_INT32)
        return value.v.i32_value;
    if (value.type == READSTAT_TYPE_INT16)
        return value.v.i16_value;
    if (value.type == READSTAT_TYPE_INT8)
        return value.v.i8_value;

    return value.v.float_value;
}

double readstat_double_value(readstat_value_t value) {
    if (readstat_value_is_system_missing(value))
        return NAN;

    if (value.type == READSTAT_TYPE_DOUBLE)
        return value.v.double_value;
    if (value.type == READSTAT_TYPE_FLOAT)
        return value.v.float_value;
    if (value.type == READSTAT_TYPE_INT32)
        return value.v.i32_value;
    if (value.type == READSTAT_TYPE_INT16)
        return value.v.i16_value;
    if (value.type == READSTAT_TYPE_INT8)
        return value.v.i8_value;

    return NAN;
}

const char *readstat_string_value(readstat_value_t value) {
    if (readstat_value_type(value) == READSTAT_TYPE_STRING)
        return value.v.string_value;

    return NULL;
}
