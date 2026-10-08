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

# Internal structured projection for browser and application consumers.
# XML consumes the same analysed columns through an explicit internal argument.
metadataProjection <- function(from, columns = seq_len(ncol(from)),
    fields = c("identity", "annotations", "classification", "weight_signal",
        "summaries", "categories"), revision = NULL, xml_options = list()) {
    if (!is.data.frame(from)) {
        stop("The input must be a data frame.")
    }

    if (!is.numeric(columns) || anyNA(columns) ||
        any(!is.finite(columns) | columns != trunc(columns) |
            columns < 1 | columns > ncol(from))) {
        stop("Columns must be valid integer positions.")
    }

    available <- c("identity", "annotations", "classification", "weight_signal",
        "summaries", "categories", "xml")

    if (!is.character(fields) || anyNA(fields) || anyDuplicated(fields) ||
        any(!is.element(fields, available))) {
        stop("Fields must be distinct supported projection names.")
    }

    if (!is.list(xml_options) || is.null(names(xml_options)) && length(xml_options) > 0L) {
        stop("XML options must be a named list.")
    }
    if (any(is.element(names(xml_options),
        c("variables", "data", ".analysis", "session", "revision", "columns")))) {
        stop("XML options cannot replace projection inputs.")
    }

    positions <- as.integer(columns)
    selected <- from[positions]
    names(selected) <- names(from)[positions]

    variables <- collectRMetadata(
        selected,
        infer_type = FALSE,
        include_formats = FALSE
    )

    value_fields <- c("classification", "weight_signal", "summaries", "categories", "xml")
    needs_analysis <- any(is.element(fields, value_fields))
    analysis <- NULL

    if (needs_analysis) {
        if (length(selected) == 0L) {
            dates <- logical()
            analysis <- list(
                var_dcml = numeric(),
                var_width = numeric(),
                range_units = character(),
                val_min = numeric(),
                val_max = numeric(),
                stat_min = numeric(),
                stat_max = numeric(),
                stat_mean = numeric(),
                stat_medn = numeric(),
                stat_stdev = numeric(),
                sum_valid = numeric(),
                sum_invalid = numeric(),
                cat_counts = integer(),
                cat_values = character(),
                cat_labels = character(),
                cat_missing = logical(),
                cat_freq = numeric(),
                variable_type = character(),
                weight_numeric_compatible = logical(),
                weight_has_labels = logical(),
                weight_has_observed = logical(),
                weight_has_negative = logical()
            )
        }
        else {
            dates <- unname(vapply(variables, function(variable) {
                return(
                    identical(variable$varFormat, "date") ||
                    is.element("Date", getElement(variable, "classes"))
                )
            }, logical(1)))

            analysis <- collectDataDscrStatsC(
                selected,
                variables,
                dates,
                include_projection = TRUE
            )
        }
    }

    return(.metadataProjectionResult(
        variables = variables,
        selected = selected,
        positions = positions,
        fields = fields,
        analysis = analysis,
        revision = revision,
        xml_options = xml_options
    ))
}

.metadataProjectionResult <- function(variables, selected, positions, fields,
    analysis, revision, xml_options, identity_storage = NULL,
    identity_length = NULL, dates = NULL) {
    projection <- list()

    if (is.null(identity_storage)) {
        identity_storage <- unname(vapply(selected, typeof, character(1)))
    }

    if (is.null(identity_length)) {
        identity_length <- unname(vapply(selected, length, integer(1)))
    }

    if (is.element("xml", fields) && is.null(dates)) {
        dates <- unname(vapply(variables, function(variable) {
            return(
                identical(variable$varFormat, "date") ||
                is.element("Date", getElement(variable, "classes"))
            )
        }, logical(1)))
    }

    if (is.element("identity", fields)) {
        projection$identity <- list(
            position = positions,
            name = names(selected),
            ID = lapply(variables, getElement, name = "ID"),
            storage = identity_storage,
            classes = lapply(variables, getElement, name = "classes"),
            length = identity_length
        )
    }

    if (is.element("annotations", fields)) {
        projection$annotations <- list(
            label = lapply(variables, getElement, name = "label"),
            measurement = lapply(variables, getElement, name = "measurement"),
            xmlang = lapply(variables, getElement, name = "xmlang")
        )
    }

    if (is.element("classification", fields)) {
        projection$classification <- analysis$variable_type
    }

    if (is.element("weight_signal", fields)) {
        projection$weight_signal <- list(
            numeric_compatible = analysis$weight_numeric_compatible,
            has_labels = analysis$weight_has_labels,
            has_observed_value = analysis$weight_has_observed,
            has_negative_value = analysis$weight_has_negative
        )
    }

    if (is.element("summaries", fields)) {
        projection$summaries <- list(
            valid = analysis$sum_valid,
            invalid = analysis$sum_invalid,
            minimum = analysis$stat_min,
            maximum = analysis$stat_max,
            mean = analysis$stat_mean,
            median = analysis$stat_medn,
            standard_deviation = analysis$stat_stdev,
            value_minimum = analysis$val_min,
            value_maximum = analysis$val_max,
            width = analysis$var_width,
            decimals = analysis$var_dcml,
            range_units = analysis$range_units
        )
    }

    if (is.element("categories", fields)) {
        projection$categories <- list(
            counts = analysis$cat_counts,
            values = analysis$cat_values,
            labels = analysis$cat_labels,
            missing = analysis$cat_missing,
            frequencies = analysis$cat_freq
        )
    }

    if (is.element("xml", fields)) {
        if (length(selected) == 0L) {
            projection$xml <- character()
            projection$xml_metadata <- list(
                id = character(),
                display_type = character(),
                measurement = character(),
                width = numeric(),
                decimals = numeric()
            )
        }
        else {
            xml_result <- do.call(
                makeXMLvars,
                c(list(variables = variables, data = selected,
                    .analysis = analysis), xml_options)
            )
            projection$xml <- xml_result$xml

            if (isTRUE(xml_options$return_hashes)) {
                projection$hashes <- xml_result$hashes
            }

            xml_stats <- xml_result$stats
            display_type <- rep("", length(selected))
            if (!is.null(xml_stats) && nrow(xml_stats) == length(selected)) {
                format_type <- tolower(as.character(xml_stats$varformat_type))
                format_value <- tolower(as.character(xml_stats$varformat_value))
                display_type[grepl("char", format_type)] <- "String"
                display_type[grepl("num", format_type)] <- "Numeric"
                display_type[grepl("date|time", format_value)] <- "Date"

                projection$xml_metadata <- list(
                    id = as.character(xml_stats$id),
                    display_type = display_type,
                    measurement = as.character(xml_stats$measurement),
                    width = as.numeric(xml_stats$width),
                    decimals = as.numeric(xml_stats$dcml)
                )

                if (!is.null(projection$identity)) {
                    missing_id <- vapply(projection$identity$ID, function(id) {
                        return(is.null(id) || length(id) == 0L ||
                            !nzchar(as.character(id)[1]))
                    }, logical(1))
                    projection$identity$ID[missing_id] <- as.list(xml_stats$id[missing_id])
                }
            }
        }
    }

    return(list(
        abi = 1L,
        layout = "columnar",
        revision = revision,
        columns = positions,
        completed_fields = fields,
        projection = projection
    ))
}
