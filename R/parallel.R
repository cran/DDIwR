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

#' Parallel variable analysis
#'
#' @name DDIwRParallel
#' @description Native worker control for variable summaries and metadata format
#' inference. The serial and parallel paths use the same calculation routines.
#'
#' @details
#' Set `options(DDIwR.variable_threads = 1L)` to run variable analysis serially.
#' A positive whole number up to 256 requests a worker limit, bounded by the
#' available processors and the number of variables. The calling R thread counts
#' toward this limit. `0L`, or an unset option, selects automatic execution:
#' at most four workers for summaries and two for format inference, with serial
#' execution below 10,000 input values. These are conservative defaults; an
#' explicit positive limit bypasses the small-input threshold.
#'
#' This option applies to unweighted variable summaries and format inference.
#' It does not change the separate `DDIwR.readstat_threads` input-reader option.
#' Invalid values produce an error before variable analysis starts.
#'
#' The C metadata collector with `include_formats = FALSE` stays on the calling
#' R thread. It reads variable attributes, without scanning observations. Its
#' workload depends on the number of columns and metadata materialized (for
#' example, factor levels), not the number of rows. Format inference additionally
#' scans values, so its workload depends on both column count and row count.
#' The 10,000-value threshold applies to value scans, not metadata-only sweeps.
#' The R wrapper `collectRMetadata()` also cleans labels and, by default, infers
#' variable types by examining values. Set both `infer_type = FALSE` and
#' `include_formats = FALSE` when requesting only existing metadata from it.
#'
#' Workers use plain C data and do not call the R API. Results are assembled by
#' the calling R thread. If workers cannot be started, unfinished jobs are
#' completed by the calling thread. Temporary-memory allocation failure during
#' summary calculation produces an error instead of an incomplete result.
#'
#' Stock WebR uses the serial native path. Running the shared calculation core
#' in separate browser workers requires application-side orchestration; this
#' R option does not start browser workers.
#'
#' @seealso [collectRMetadata()], [collectMetadata()]
#' @keywords internal
NULL
