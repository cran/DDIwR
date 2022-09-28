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

#' @description Utility function to write the metadata part in the setup file.
#' @return Nothing.
#' @noRd
`writeMetadata` <- function(dataDscr, OS = "", indent = 4) {
    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    enter <- getEnter(OS = OS)
    
    rs <- function(x) {
        paste(rep(" ", x * indent), collapse="")
    }
    
    if (is.element("dataDscr", names(dataDscr))) {
        dataDscr <- dataDscr$dataDscr
    }
    
    for (i in seq(length(dataDscr))) {
        if (is.element("labels", names(dataDscr[[i]]))) {

            values <- dataDscr[[i]][["labels"]]
            labels <- names(values)
            notNum <- any(is.na(suppressWarnings(as.numeric(values))))
            quote <- ifelse(notNum, "\"", "")
            
            valstring <- paste(paste("\"", labels, "\"", sep = ""),
                               paste(quote, values, quote, sep = ""),
                               sep = " = ", collapse = ",\n               ")
            cat("rdatafile[[\"", names(dataDscr)[i], "\"]] <- declared(rdatafile[[\"", names(dataDscr)[i], "\"]],", enter, sep="")
            cat(paste0(rs(1), "labels = c(", valstring, "),", enter))
            
            if (is.element("na_values", names(dataDscr[[i]]))) {
                cat(paste0(rs(1), "na_values = c(", paste(quote, dataDscr[[i]]$na_values, quote, sep = "", collapse = ", "), "),", enter))
            }
            
            if (is.element("na_range", names(dataDscr[[i]]))) {
                cat(paste0(rs(1), "na_range = c(", paste(quote, dataDscr[[i]]$na_range, quote, sep = "", collapse = ", "), "),", enter))
            }

            cat(rs(1), "label = \"", dataDscr[[i]][["label"]], "\"", sep = "")
            cat(paste0(enter, ")", enter, enter, enter))

        }
    }
}
