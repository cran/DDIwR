# Copyright (c) 2021, Adrian Dusa
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, in whole or in part, are permitted provided that the
# following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * The names of its contributors may NOT be used to endorse or promote products
#       derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

`convertibble` <- function(x, dataDscr, ...) {

    pN <- unlist(lapply(x, function(x) admisc::possibleNumeric(x)))
    
    for (i in names(x)) {
        
        if (is.element("values", names(dataDscr[[i]]))) {

            na_values <- NULL
            if (is.element("missing", names(dataDscr[[i]]))) {
                na_values <- dataDscr[[i]]$missing
            }

            na_range <- NULL
            if (is.element("missrange", names(dataDscr[[i]]))) {
                na_range <- dataDscr[[i]]$missrange
            }

            labels <- dataDscr[[i]]$values
            
            var <- unname(unlist(unclass(x[, i])))

            if (pN[i]) {
                var <- admisc::asNumeric(var)
            }
            else {
                var <- as.character(var)
                labels <- unlist(lapply(labels, as.character))
                na_values <- unlist(lapply(na_values, as.character))
                na_range <- unlist(lapply(na_range, as.character))
            }
            
            x[[i]] <- haven::labelled_spss(var, labels, na_values, na_range)
        }

        attr(x[[i]], "label") <- dataDscr[[i]]$label

    }

    other.args <- list(...)
    if (is.element("spss", names(other.args))) {
        if (other.args$spss) {
            x[] <- lapply(x, function(x) {
                attr(x, "format.spss") <- getFormat(x)
                return(x)
            })
        }
    }

    return(x)
}
