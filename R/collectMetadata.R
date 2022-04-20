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

`collectMetadata` <- function(dataset, ...) {
    dots <- list(...)

    error_null <- TRUE
    if (is.element("error_null", names(dots))) {
        error_null <- dots$error_null
    }

    if (is.data.frame(dataset)) {
        error <- TRUE
        i <- 1
        while (i <= ncol(dataset) & error) {
            attrx <- attributes(dataset[[i]])
            if (any(is.element(
                c("label", "labels", "na_value", "na_range"),
                names(attrx)
            ))) {
                error <- FALSE
            }
            i <- i + 1
        }

        if (error && error_null) {
            admisc::stopError(
                "The input does not seem to contain any metadata."
            )
        }
    }
    else {
        admisc::stopError(
            "The input should be a dataframe containing labelled variables."
        )
    }

    output <- lapply(dataset, function(x) {
        result <- list()
        
        label <- attr(x, "label", exact = TRUE)
        if (!is.null(label)) {
            result[["label"]] <- DDIwR::cleanup(label)
        }

        labels <- attr(x, "labels", exact = TRUE)
        if (!is.null(labels)) {
            tagged <- logical(length(labels))
            if (is.double(labels)) {
                tagged <- haven::is_tagged_na(labels)
            }

            labels <- labels[!is.na(labels) | tagged]
            if (length(labels) > 0) {
                names(labels) <- DDIwR::cleanup(names(labels))
                result[["labels"]] <- labels
            }
        }
        
        na_values <- attr(x, "na_values", exact = TRUE)
        if (is.null(na_values)) {
            if (is.double(x)) {
                natags <- unique(haven::na_tag(c(unclass(x), unclass(labels))))
                natags <- natags[!is.na(natags)]
                if (length(natags) > 0) {
                    result$na_values <- sort(natags)
                }
            }
        }
        else {
            # it should't have (tagged) NA values, but just in case
            na_values <- na_values[!is.na(na_values)]
            if (length(na_values) > 0) {
                result$na_values <- na_values
            }
        }
        
        result$na_range <- attr(x, "na_range", exact = TRUE)
        result$type <- DDIwR::checkType(
            x,
            labels,
            admisc::possibleNumeric(x)
        )

        return(result)
    })

    return(output)
}
