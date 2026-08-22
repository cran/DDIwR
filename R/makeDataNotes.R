# Copyright (c) 2026, Adrian Dusa
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
#     * The names of its contributors may NOT be used to endorse or promote
#       products derived from this software without specific prior written
#       permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#' @name makeDataNotes
#'
#' @title
#' Create a `notes` element for the dataset.
#'
#' @description
#' Create the `notes` element to embed a serialized, gzip-ed version of the data
#' in the `fileDscr` section of the `codeBook`.

#' @return A standard `notes` DDI element.
#'
#' @author Adrian Dusa
#'
#' @param data An R dataframe.
#'
#' @export
`makeDataNotes` <- function(data) {
    enter <- getEnter(OS = Sys.info()[['sysname']])

    notes <- makeElement("notes")
    addContent(
        paste0(
            enter,
            repeatSpace(3, indent = 2),
            base64enc::base64encode(
                memCompress(serialize(data, NULL), type = "gzip"),
                linewidth = 500,
                newline = paste(enter, repeatSpace(3, indent = 2), sep = "")
            ),
            enter,
            repeatSpace(2, indent = 2)
        ),
        to = notes
    )

    addAttributes(
        c(
            ID = "rawdata",
            level = "file",
            subject = "R dataset, serialized gzip"
        ),
        to = notes
    )

    return(notes)
}
