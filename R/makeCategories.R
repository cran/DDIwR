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

#' @name makeCategories
#'
#' @title
#' Create the `catgry` elements for a particular variable
#'
#' @description Utility function to create the `catgry` elements, as well as all
#' necessary sub-elements (e.g. `catValu`, `labl`, `varFormat`) along with their
#' associated XML attributes.

#' @return A list of standard `catgry` DDI elements.
#'
#' @author Adrian Dusa
#'
#' @param metadata A list of two or three components: `labels`,
#' `na_values` and/or `na_range`
#'
#' @export
`makeCategories` <- function(metadata) {
    if (!is.list(metadata) || is.null(names(metadata))) {
        admisc::stopError("The argument 'metadata' should be a names list.")
    }

    nms <- names(metadata)

    if (!is.element("labels", nms)) {
        admisc::stopError("Values and labels are necessary to create the `catgry` element")
    }

    values <- unname(getElement(metadata, "labels"))
    labels <- names(getElement(metadata, "labels"))
    ismiss <- logical(length(values))

    na_values <- metadata$na_values
    if (!is.null(na_values)) {
        ismiss <- is.element(values, na_values)
    }
    na_range <- metadata$na_range
    if (!is.null(na_range)) {
        ismiss <- ismiss | (values >= na_range[1] & values <= na_range[2])
    }

    return(lapply(seq_along(values), function(i) {
        icatgry <- makeElement("catgry")

        if (ismiss[i]) {
            addAttributes(c(missing = "Y"), to = icatgry)
        }

        addChildren(
            list(
                makeElement("labl", content = labels[i]),
                makeElement("catValu", content = values[i])
            ),
            to = icatgry
        )

        return(icatgry)
    }))
}
