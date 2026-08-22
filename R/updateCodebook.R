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

#' @name updateCodebook
#'
#' @title
#' Update Codebook.
#'
#' @description
#' Update an XML file containing a DDI Codebook.
#'
#' @details
#' This function replaces entire Codebook sections. Any such section present in
#' the R object will replace the corresponding section from the XML document.
#'
#' @author Adrian Dusa
#'
#' @param xmlfile A path to a DDI Codebook XML document.
#' @param with An R object containing a root `codeBook` element.
#' @param ... Other internal arguments.
#'
#' @export
`updateCodebook` <- function(xmlfile, with, ...) {

    DDIC <- get("DDIC", envir = cacheEnv)

    dots <- list(...)
    indent <- checkDots(dots$indent, default = 2)
    enter <- checkDots(
        dots$enter,
        default = getEnter(OS = Sys.info()[['sysname']])
    )

    sections <- DDIC$codeBook$children
    xml <- getXML(xmlfile)
    # if this survives, it is a valid XML document

    if (
        !all(
            is.element(
                xml_name(xml_children(xml)),
                sections
            )
        )
    ) {
        admisc::stopError(
            "The XML file contains elements that are not Codebook sections."
        )
    }

    xml <- readLines(xmlfile)
    checkXMList(with)

    starts <- lapply(sections, function(s) {
        which(grepl(paste0("<", s), xml))
    })
    names(starts) <- sections

    ends <- lapply(sections, function(s) {
        which(grepl(paste0("</", s), xml))
    })
    names(ends) <- sections

    for (s in sections) {
        if (length(starts[[s]]) > 0 && length(ends[[s]]) == 0) {
            ends[[s]] <- starts[[s]]
        }
    }

    # TODO: make sure the sections are PROPERLY ordered

    tmp <- tempdir()

    monolang <- with$.extra$monolang
    if (is.null(monolang)) monolang <- FALSE

    codeBook <- removeExtra(changeXMLang(with, remove = monolang))
    children <- names(codeBook)

    for (child in unique(children[order(match(children, rev(sections)))])) {
        parts <- unlist(lapply(
            which(children == child),
            function(x) {
                xml2::write_xml(
                    xml2::as_xml_document(list(codeBook = codeBook[x])),
                    file = file.path(tmp, "part.xml")
                )
                part <- readLines(file.path(tmp, "part.xml"))
                return(part[seq(3, length(part) - 1)])
            }
        ))

        start <- starts[[child]]
        end <- ends[[child]]

        if (length(start) > 0) {
            xml <- xml[-seq(start[1], end[length(end)])]
            xml <- append(xml, parts, after = start[1] - 1)
        }
        else {
            # new element
            if (child == "docDscr") {
                xml <- append(xml, parts, after = unlist(starts)[1] - 1)
            }
            else {
                end <- unlist(ends[seq(1, match(child, sections) - 1)])
                xml <- append(xml, parts, after = end[length(end)])
            }
        }
    }

    writeLines(xml, con = xmlfile)
}
