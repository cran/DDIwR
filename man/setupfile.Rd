\name{setupfile}

\alias{setupfile}

\title{Create setup files for SPSS, Stata, SAS and R}

\description{
This function creates a setup file, based on a list of variable and value labels.
}

\usage{
setupfile(lbls = "", type = "all", csv = "", miss, trymiss = FALSE, uniqueid = "",
          SD = "", delimiter = ",", OS = "windows", outfile = "", ...)
}

\arguments{
  \item{lbls}{The list object containing the variable and value labels as separate
              components, or a path to the folder where these objects are located,
              for batch processing.}
  \item{type}{The type of setup file, can be one of: "SPSS", "Stata", "SAS", "R",
              or "all" (default).}
  \item{csv}{The original dataset, on the basis of which the SPSS setup file
             commands are created, or a path to the folder where the .csv files
             are located, for batch processing.}
  \item{miss}{A vector of missing values, or missing labels.}
  \item{trymiss}{Boolean, if TRUE it will try hard to find common missing values
                 (e.g. "DK", or "NA" etc.)}
  \item{uniqueid}{Character, the name of the unique identifier variable}
  \item{SD}{The row delimiter for the Stata commands, can be for example "" or ";"}
  \item{delimiter}{The column delimiter to be used for reading the .csv file,
                   default is ","}
  \item{OS}{The target operating system, for the eol - end of line separator.}
  \item{outfile}{Character, the name of the setup file being created.}
  \item{...}{Other arguments (not used in this function).}
}

\details{
If \code{type} = "all", it will produce once setup file for each supported type.
All created setup files will be saved in a folder called "Setup Files" which
(if not already found) will be created in the user's current working directory.

The argument \code{miss} expects either:
- a vector of missing values (e.g. -1, -2, -3), or
- a vector of missing labels

If this is not provided, but \code{trymiss} is set to TRUE, then it searches all
value labels for these common missing categories:
"DK/NA", "DK/NO", "DK", "NA", "N/A", "Not answered",
"Don't know", "(Don't know)", "No answer", "No opinion",
"Not applicable", "Not relevant", "Refused", "(Refused)",
"Refused / no answer", "(Refused / no answer)",
"Can't say", "Don't know / Can't say".

If batch processing multiple files, the function will inspect all files in the
provided folder, and retain only those with the extension  \code{.R} or \code{.r}
or DDI versions with the extension \code{.xml} or \code{.XML} (it will
subsequently generate an error if the .R files do not contain an object list,
or if the \code{.xml} files do not contain a DDI structured metadata file).

If the metadata folder contains a subfolder called \code{"data"} or
\code{"Data"}, it will match the name of the metadata file with the name of the
\code{.csv} file (their names have to be *exactly* the same, irrespective of
their extension).

The \code{csv} argument can provide a data.frame object produced by reading the
\code{.csv} file, or a path to the folder where the \code{.csv} files are located.
If the user doesn't provide something for this argument, the function will check
the existence of a subfolder called \code{data} in the folder where the metadata
files are located.

The \code{uniqueid} argument is only relevant if \code{type = "R"}. It is necessary
to identify missing observations in different variables, based on the unique case
identifiers found in the variable provided via this argument. It will generate an
"attr"ibute called "missing types", which is essentially a list whose components
are variable names, and each component is a list itself containing a vector of
values for each missing category (type) plus the identifiers of the cases where
missing values are found (and replaced with NA).
It will also generate an attribute called "unique id", which points to the same
name of the variable containing the unique case identifiers.

The argument \code{SD} only makes sense when \code{type = "Stata"} or \code{type = "all"},
(when Stata setup files will also be generated).

In batch mode, the code starts with the argument \code{delimiter = ","}, but if the
\code{.csv} file is delimited differently it will also try hard to find other delimiters
which will match the variable names in the metadata file. At the initial version 0.1-0,
the automatically detected delimiters include \code{";"} and \code{"\t"}.

The argument \code{OS} can be either:
\code{"windows"} (default), or \code{"Windows"}, \code{"Win"}, \code{"win"},
\code{"MacOS"}, \code{"Darwin"}, \code{"Apple"}, \code{"Mac"}, \code{"mac"},
\code{"Linux"}, \code{"linux"}.

The end of line separator changes only when the target OS is different from the
running OS.

The argument \code{outfile} expects the name of the final setup file being saved
on the disk. If nothing is provided, the name of the object provided for the
\code{lbls} argument will be used as a filename.

There is also an undocumented, boolean argument called \code{saveFile}, which if
set to TRUE it will save an R version if the metadata was read from a DDI \code{.xml}
file, in the same folder. This function uses \code{\link{getMetadata}}, where that
argument is a formal one.
}

\value{
A setup file to complement the imported raw dataset.
}


\examples{
fancyname <- list()

fancyname$varlab <- list(
"ID" = "Questionnaire ID",
"V1" = "First variable's label",
"V2" = "Second variable's label",
"V3" = "Third variable's label"
)


fancyname$vallab$V1 <- c(
"No"             =  0, 
"Yes"            =  1,
"Not answered"   = -1
)


fancyname$vallab$V2 <- c(
"Verry little"   =  1, 
"Little"         =  2,
"So, so"         =  3,
"Much"           =  4,
"Very much"      =  5,
"Not applicable" = -7,
"Don\'t know"     = -8,
"Not answered"   = -9
)


fancyname$vallab$V3 <- c(
"No"             =  0, 
"Yes"            =  1,
"Not answered"   = -1
)


# 
###   IMPORTANT:
##### make sure to set the working directory to a directory with read/write privileges
###
# setwd("/path/to/read/write/directory")


##### then run these commands
###
# setupfile(fancyname, trymiss = TRUE, uniqueid = "ID")


# setupfile(fancyname, type="Stata", SD=";")


##### other types of possible utilizations, using paths to specific files
###
# setupfile("/path/to/the/metadata/file.xml", csv="/path/to/the/csv/file.csv")


##### if the metadata is saved to an .R file containing a list
###
# setupfile("/path/to/the/metadata/file.R", csv="/path/to/the/csv/file.csv")


##### or in batch mode, specifying entire directories
###
# setupfile("/path/to/the/metadata/directory", csv="/path/to/the/csv/directory")

}

\keyword{functions}
