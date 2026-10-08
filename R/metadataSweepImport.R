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

#' Import data with a prepared metadata sweep session
#'
#' @description
#' Import a dataset and immediately prepare metadata for a declared consumer
#' profile. The returned session remains reusable for every profile; `prepare`
#' controls only which projection is warm when the import returns.
#'
#' @param from Path to a dataset supported by [convert()].
#' @param revision One non-missing atomic revision token for the imported data.
#' @param prepare One of `"progressive"`, `"dialogr"`, or `"codebook"`.
#' @param columns Integer positions or unambiguous variable names to prepare.
#' `NULL` prepares every column. Progressive consumers should provide their
#' initial visible range.
#' @param fields Optional field groups passed to [metadataSweepRead()].
#' @param xml_options Named arguments passed to the variable XML generator.
#' @param ... Import arguments passed to [convert()]. The import destination is
#' always R; `to` must not be supplied.
#'
#' @return An object with `data`, `session`, and the prepared `projection`.
#' Close `session` with [metadataSweepClose()] when it is no longer needed.
#'
#' @examples
#' \dontrun{
#' imported <- metadataSweepImport(
#'     "survey.sav",
#'     revision = 1L,
#'     prepare = "progressive",
#'     columns = 1:20
#' )
#' metadataSweepClose(imported$session)
#' }
#'
#' @export
metadataSweepImport <- function(from, revision, prepare = "progressive",
    columns = NULL, fields = NULL, xml_options = list(), ...) {
    .metadataSessionValidateRevision(revision)

    profiles <- c("progressive", "dialogr", "codebook")
    if (!is.character(prepare) || length(prepare) != 1L || is.na(prepare) ||
        !is.element(prepare, profiles)) {
        stop("Prepare must be one supported metadata profile.")
    }

    dots <- list(...)
    if (is.element("to", names(dots))) {
        stop("Metadata sweep imports always import to R; do not supply 'to'.")
    }

    data <- do.call(convert, c(list(from = from, to = NULL), dots))
    if (!is.data.frame(data)) {
        stop("The imported object is not a data frame.")
    }

    session <- metadataSweepCapture(data, revision)
    complete <- FALSE

    on.exit({
        if (!complete) {
            metadataSweepClose(session)
        }
    })

    projection <- metadataSweepRead(
        session = session,
        revision = revision,
        columns = columns,
        fields = fields,
        xml_options = xml_options,
        profile = prepare
    )

    result <- structure(
        list(
            data = data,
            session = session,
            projection = projection
        ),
        class = "ddiwr_metadata_import"
    )
    complete <- TRUE

    return(result)
}
