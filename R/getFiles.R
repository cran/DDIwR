# Copyright (c) 2019, Adrian Dusa
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

getFiles <- function(dirpath = ".", type = "", currdir) {
    
    # get all files
    files <- list.files(dirpath)
    
    if (length(files) == 0) {
        return(paste("The directory", dirpath, "is empty.\n\n"))
    }
    
    # split all files by "." to separate names from extentions
    filesplit <- strsplit(files, split="\\.")
    
    # get rid of files without extensions, and (sub)directory names
    noext <- unlist(lapply(filesplit, length))
    files <- files[noext > 1]
    filesplit <- filesplit[noext > 1]
    
    if (length(filesplit) == 0) {
        return(paste("The directory \"", dirpath, "\" doesn't contain any known files.\n\n", sep=""))
    }
    
    # get the file extensions
    fileext <- unlist(lapply(filesplit, function(x) {
        # we want the last part(s) of the split
        return(paste(x[-1], collapse="."))
    }))
    
    if (type != "*") {
        # check if there is any file with the right extension
        fileidxs <- which(toupper(fileext) == toupper(type))
        if (toupper(type) == "CSV" & any(toupper(fileext) == "CSV.GZ")) {
            fileidxs <- which(toupper(fileext) %in% c("CSV", "CSV.GZ"))
        }
        
        if (length(fileidxs) == 0) {
            return(paste("There is no .", type, " type file in the directory \"", dirpath, "\"\n\n", sep=""))
        }
        
        # if code survives this far, filter all the "right" files from all files
        files <- files[fileidxs]
    }
    
    # split the files again, just in case some of them were not the right type
    filesplit <- strsplit(files, split="\\.")
    
    # get the file names
    # the code below is necessary just in case the filename contains a "."
    # e.g. test.1.R
    filenames <- unlist(lapply(filesplit, function(x) {
        # we want all parts except the last, to restore the original filename
        return(paste(x[1], collapse="."))
    }))
    
    # get the file extensions again
    fileext <- unlist(lapply(filesplit, function(x) {
        paste(x[-1], collapse=".")
    }))
    
    return(list(files=files, filenames=filenames, fileext=fileext))
    
}
