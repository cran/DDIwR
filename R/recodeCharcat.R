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

`recodeCharcat` <- function(x, ...) {
    if (!is.character(x)) {
        return(x)
    }
    
    dots <- list(...)
    metadata <- dots$metadata
    labels <- metadata[["labels"]]
    attrx <- attributes(x)
    if (is.null(metadata)) {
        metadata <- attrx
    }

    # only character _categorical_ variables should be recoded
    if (
        is.null(labels) ||
        !(
            inherits(x, "declared") |
            inherits(x, "haven_labelled_spss")
        )
    ) {
        # nothing to recode, no information about categories
        return(x)
    }

    x <- declared::undeclare(x, drop = TRUE)
    
    label <- metadata[["label"]]
    na_values <- metadata[["na_values"]]

    x[x == ""] <- NA

    ### TODO: make sure the values of missing codes are way outside the range of the normal values

    labels <- sort(labels)
    ux <- unique(c(unname(labels), na_values, x[!is.na(x)]))
    pnux <- admisc::possibleNumeric(ux, each = TRUE)

    nums <- c()
    if (any(pnux)) {
        nums <- as.numeric(ux[pnux])
    }

    if (any(!pnux)) {
        nms_l <- names(labels)

        cux <- ux[!pnux]
        lux <- length(cux)
        n <- l <- 1
        
        while (l <= lux) {
            if (!is.element(n, nums)) {
                x[x == cux[l]] <- n
                labels[labels == cux[l]] <- n
                na_values[na_values == cux[l]] <- n
                l <- l + 1
            }

            n <- n + 1
        }

        labels <- setNames(as.numeric(labels), nms_l)
    }

    x <- as.numeric(x)
    if (length(na_values) > 0) {
        na_values <- as.numeric(na_values)
        na_values <- na_values[!is.na(na_values)]
    }
    else {
        na_values <- NULL
    }

    if (is.element("declared", attrx$class)) {
        return(declared::declared(
            x,
            label = label,
            labels = labels,
            na_values = na_values
        ))
    }
    
    return(haven::labelled_spss(
        x,
        label = label,
        labels = labels,
        na_values = na_values
    ))
}
