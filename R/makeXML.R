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

#' @description Write the study description part of the DDI XML file.
#' @return A character vector.
#' @noRd
`makeXML` <- function(x, space = 1, indent = 4, ns = "", enter = "\n") {
    sx <- paste(rep(" ", space*indent), collapse = "")
    result <- character(0)
    nmsx <- names(x)
    if (!is.null(nmsx)) {
        for (n in seq(length(nmsx))) {
            childnms <- names(x[[nmsx[n]]])
            attrx <- attributes(x[[nmsx[n]]])
            attrx$names <- NULL
            start <- paste(sx, "<", ns, nmsx[n], sep = "")
            
            if (length(attrx) > 0) {
                nms <- names(attrx)
                for (i in seq(length(attrx))) {
                    if (nms[i] == "lang") {
                        nms[i] <- "xml:lang"
                    }
                    start <- paste(
                        start,
                        paste(
                            nms[i],
                            paste("\"", attrx[[i]], "\"", sep = ""),
                            sep = "="
                        )
                    )
                }
            }

            start <- paste(
                start,
                ">",
                ifelse(is.null(childnms), "", enter),
                sep = ""
            )

            end <- paste(
                ifelse(is.null(childnms), "", sx),
                "</",
                ns,
                nmsx[n],
                ">",
                enter,
                sep = ""
            )

            result <- c(
                result,
                start,
                makeXML(x[[n]], space + 1),
                end
            )
        }
    }
    else {
        result <- c(result, x)
    }
    
    return(unlist(result))
}
