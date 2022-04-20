# Copyright (c) 2022, Adrian Dusa
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

`getFormat` <- function(x) {

    if (all(is.na(x))) {
        return("A0")
    }
    
    string <- is.character(x)
    x2 <- as.character(x)

    while (TRUE) {
        nofchars <- tryCatch(nchar(x2), error = function(x) return(x))

        if (!is.list(nofchars)) break
        
        # if here, tryCatch caught an error, most likely a multibyte character
        error <- unlist(strsplit(nofchars[[1]], split = " "))
        x2 <- x2[-as.numeric(error[length(error)])]
        
    }

    if (length(nofchars) == 0) {
        nofchars <- 1
    }

    maxvarchar <- max(nofchars, na.rm = TRUE)

    if (haven::is.labelled(x)) {
        labels <- attr(x, "labels")
        maxvarchar <- max(maxvarchar, nchar(labels))
        if (is.character(labels)) string <- TRUE
    }

    decimals <- FALSE
    if (is.numeric(x)) {
        decimals <- any(x - floor(x) > 0)
    }

    return(sprintf("%s%s%s%d", 
        ifelse(string, "A", "F"), 
        maxvarchar, 
        ifelse(string, "", "."),
        ifelse(decimals, 2, 0)))
}
