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

#' @keywords internal
#'
#' @name DDIwR_package
#' @aliases DDIwR-package
#'
#' @title Useful functions for various DDI (Data Documentation Initiative)
#' related outputs.
#'
#' @description This package provides various functions to read DDI based
#' metadata documentation and write dedicated setup files for R, SPSS, Stata and
#' SAS to read an associated .csv file containing the raw data, apply labels for
#' variables and values and also deal with the treatment of missing values.
#'
#' It can also generate a DDI metadata file out of an R information object,
#' which can be used to export directly to the standard statistical packages
#' files (such as SPSS, Stata and SAS, or even Excel). For R, the default
#' object to store data and metadata is a \bold{\code{data.frame}}, and labelled
#' data are automatically coerced to class `declared`.
#'
#' The research leading to the initial functions in this package has received
#' funding from the European Union's Seventh Framework Program (FP7/2007-2013)
#' under grant agreement no. 262608 (DwB - Data without Boundaries)
#'
#' @author Adrian Dusa\cr
#' Department of Sociology\cr
#' University of Bucharest\cr
#' \email{dusa.adrian@unibuc.ro}
#'
#' @details
#' \tabular{ll}{
#'   Package: \tab DDIwR\cr
#'   Type: \tab Package\cr
#'   Version: \tab 0.21\cr
#'   Date: \tab 2026-09-12\cr
#'   License: \tab GPL-v3\cr
#' }
#'
#' @importFrom admisc anyTagged asNumeric getName numdec possibleNumeric stopError trimstr tryCatchWEM wholeNumeric
#' @importFrom base64enc base64encode base64decode
#' @importFrom declared as.declared declared is.declared undeclare
#' @importFrom digest digest
#' @importFrom tools file_ext
#' @importFrom xml2 as_list as_xml_document read_xml xml_attr xml_children xml_find_all xml_find_first xml_name xml_ns xml_text
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @importFrom utils capture.output file_test packageVersion read.csv type.convert write.table
#' @useDynLib DDIwR, .registration = TRUE
#'
NULL
