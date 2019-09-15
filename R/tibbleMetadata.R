# Copyright (c) 2019, Adrian Dusa
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

`tibbleMetadata` <- function(dataDscr, OS = "", indent = 4) {
    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    enter <- getEnter(OS = OS)
    
    rs <- function(x) {
        paste(rep(" ", x*indent), collapse="")
    }
    
    if (is.element("dataDscr", names(dataDscr))) {
        dataDscr <- dataDscr$dataDscr
    }
    
    for (i in seq(length(dataDscr))) {
        if (is.element("values", names(dataDscr[[i]]))) {

            values <- dataDscr[[i]]$values
            labl <- names(values)
            notNum <- any(is.na(suppressWarnings(as.numeric(values))))
            quote <- ifelse(notNum, "\"", "")
            
            valstring <- paste(paste("\"", labl, "\"", sep = ""),
                               paste(quote, values, quote, sep = ""),
                               sep = " = ", collapse = ",\n               ")
            cat("rdatafile[[\"", names(dataDscr)[i], "\"]] <- haven::labelled_spss(rdatafile[[\"", names(dataDscr)[i], "\"]],", enter, sep="")
            cat(rs(1), "labels = c(", valstring, ")", sep = "")
            if (is.element("missing", names(dataDscr[[i]]))) {
                cat(",", enter, rs(1), "na_values = c(", paste(quote, dataDscr[[i]]$missing, quote, sep = "", collapse = ", "), ")", sep = "")
            }
            cat(enter, ")", enter, sep = "")
        }
        cat("attr(rdatafile[[\"", names(dataDscr)[i], "\"]], \"label\") <- \"", dataDscr[[i]]$label, "\"", enter, enter, sep="")
    }
}
