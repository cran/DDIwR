# Copyright (c) 2021, Adrian Dusa
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

treatPath <- function(path, type = "*", single = FALSE, check = TRUE) {
    if (length(path) > 1) {
        cat("\n")
        # if (type == "R") {
        #     stop("The <codeBook> argument should contain a single path to the list object.\n\n", call. = FALSE)
        # }
        if (type == "csv") {
            stop("The <csv> argument should contain a single path to the .csv file.\n\n", call. = FALSE)
        }
    }
    
    if (!is.character(path)) {
        cat("\n")
        stop("The path should be specified in a string.\n\n", call. = FALSE)
    }
    currdir <- getwd()
    
    lastpart <- basename(path)
    # normalizePath() deals with the symbolic links, relative paths and absolute paths
    pathname <- suppressWarnings(normalizePath(dirname(path), winslash = "/"))
    
    # check if a path exists, before the lastpart
    pathexists <- pathname != "."
    
    if (pathexists) {
        
        if (!file.exists(pathname)) {
            if (check) {
                cat("\n")
                stop(paste("Cannot find the path up to \"", pathname, "\".\n",
                        "Please check that path, or try changing the working directory.\n\n", sep = ""), call. = FALSE)
            }
            else {
                pathname <- file.path(getwd(), pathname)
            }
        }
        
    }
    
    allfiles <- FALSE
    if (!file.exists(file.path(pathname, lastpart))) {
        # something like /path/to/*.R
        # where lastpart is *.R
        filesplit <- unlist(strsplit(lastpart, split = "\\."))
        
        if (length(filesplit) >= 2) {
            if (filesplit[1] == "*") {
                allfiles <- TRUE
                type <- filesplit[2]
                lastpart <- ""
            }
        }
        
        if (!allfiles & check) {
            cat("\n")
            stop(paste("There is no \"", lastpart, "\" in the directory \"", ifelse(pathname == ".", getwd(), pathname), "/\".\n\n", sep=""), call. = FALSE)
        }
        
        fileobj <- list(
            completePath = pathname,
            files = lastpart,
            filenames = filesplit[1],
            fileext = toupper(filesplit[2])
        )
    }
    else {
    
        ## file_test() determines if a file or a directory
        if (file_test("-d", file.path(pathname, lastpart))) {
            if (single) {
                cat("\n")
                stop(paste("A file name should be provided, not a directory.\n\n", sep = ""), call. = FALSE)
            }
        }
        
        fileobj <- getFiles(path = file.path(pathname, lastpart), type = type)
        
    }
    
    return(fileobj)
}
