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

.validateCodebookXML <- function(output, schema_path = system.file(
    "schemas", "ddi-codebook-2.6", "codebook.xsd", package = "DDIwR"
)) {
    if (!nzchar(schema_path) || !file.exists(schema_path)) {
        stop("Cannot validate DDI Codebook XML: the bundled DDI 2.6 schema is unavailable.",
            call. = FALSE)
    }

    result <- tryCatch({
        schema <- xml2::read_xml(schema_path, options = "NONET")
        document <- xml2::read_xml(charToRaw(enc2utf8(output)),
            options = c("NONET", "HUGE"))
        valid <- xml2::xml_validate(document, schema)
        diagnostics <- attr(valid, "errors")

        if (!isTRUE(valid) || length(diagnostics) > 0L) {
            if (length(diagnostics) == 0L) {
                diagnostics <- "The document does not conform to the schema."
            }

            stop(paste(diagnostics, collapse = "\n"), call. = FALSE)
        }

        TRUE
    }, error = function(error) {
        conditionMessage(error)
    })

    if (!isTRUE(result)) {
        stop(paste0("DDI Codebook 2.6 XML validation failed:\n", result),
            call. = FALSE)
    }

    return(invisible(TRUE))
}
