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

#' @name searchFor
#'
#' @title Search for key words
#'
#' @description Search function to return elements that contain a certain
#' word or regular expression pattern.
#'
#' @param x Character, either word(s) or a regular expression.
#' @param where Character, in which section(s) to search for.
#' @param ... Other arguments to be passed to the grepl() function.
#'
#' @return Character vector of DDI element names.
#'
#' @author Adrian Dusa
#' @export
`searchFor` <- function(
    x,
    where = c("everywhere", "title", "description", "attributes", "examples"),
    ...
) {
    tryit <- admisc::tryCatchWEM(
        where <- match.arg(where, several.ok = TRUE)
    )

    `xmlcontent` <- function(x) {
        xml2::xml_text(
            xml2::xml_find_all(
                xml2::read_xml(paste0("<root>", x, "</root>")),
                ".//*"
            )
        )
    }

    if (!is.null(tryit$error)) {
        admisc::stopError(
            paste("Argument", gsub("arg", "where", tryit$error))
        )
    }

    if (is.element("everywhere", tolower(where))) {
        where <- c("title", "description", "attributes", "examples")
    }

    DDIC <- get("DDIC", envir = cacheEnv)

    hasword <- sapply(DDIC, function(element) {
        result <- FALSE
        if (is.element("title", tolower(where))) {
            result <- result | any(
                grepl(
                    paste(x, collapse = "|"),
                    element$title,
                    ... = ...
                )
            )
        }

        if (is.element("description", tolower(where))) {
            result <- result | any(
                grepl(
                    paste(x, collapse = "|"),
                    element$description,
                    ... = ...
                )
            )
        }

        if (is.element("attributes", tolower(where)) && length(element$attributes) > 0) {
            for (i in seq_along(element$attributes)) {
                result <- result | any(
                    grepl(
                        paste(x, collapse = "|"),
                        element$attributes[[i]]$description,
                        ... = ...
                    )
                )
            }
        }

        if (is.element("examples", tolower(where)) && length(element$examples) > 0) {
            for (i in seq_along(element$examples)) {
                result <- result | any(
                    grepl(
                        paste(x, collapse = "|"),
                        xmlcontent(element$examples[i]),
                        ... = ...
                    )
                )
            }
        }

        return(result)
    })

    return(names(hasword)[hasword])
}
