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

#include <stdlib.h>

#include "../readstat.h"
#include "readstat_schema.h"
#include "readstat_copy.h"

void readstat_schema_free(readstat_schema_t *schema) {
    if (schema) {
        free(schema->entries);
        free(schema);
    }
}

readstat_schema_entry_t *readstat_schema_find_or_create_entry(readstat_schema_t *dct, const char *var_name) {
    readstat_schema_entry_t *entry = NULL;
    int i;
    /* linear search. this is shitty, but whatever */
    for (i=0; i<dct->entry_count; i++) {
        if (strcmp(dct->entries[i].variable.name, var_name) == 0) {
            entry = &dct->entries[i];
            break;
        }
    }
    if (!entry) {
        dct->entries = realloc(dct->entries, sizeof(readstat_schema_entry_t) * (dct->entry_count + 1));
        entry = &dct->entries[dct->entry_count];
        memset(entry, 0, sizeof(readstat_schema_entry_t));
        
        readstat_copy(entry->variable.name, sizeof(entry->variable.name), var_name, strlen(var_name));
        entry->decimal_separator = '.';
        entry->variable.index = dct->entry_count++;
    }
    return entry;
}

