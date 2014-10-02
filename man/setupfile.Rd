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
              components, or a path to the directory where these objects are located,
              for batch processing.}
  \item{type}{The type of setup file, can be: "SPSS", "Stata", "SAS", "R",
              or "all" (default).}
  \item{csv}{The original dataset, on the basis of which the SPSS setup file
             commands are created, or a path to the directory where the .csv files
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
If \verb{type} = "all", it will produce once setup file for each supported type.
All created setup files will be saved in a directory called "Setup Files" which
(if not already found) will be created in the user's current working directory.

The argument \verb{miss} expects either:\cr
- a vector of missing values (e.g. -1, -2, -3), or\cr
- a vector of missing labels

If this is not provided, but \verb{trymiss} is set to TRUE, then it searches all
value labels for these common missing categories:\cr
\verb{"DK/NA"}, \verb{"DK/NO"}, \verb{"DK"}, \verb{"NA"}, \verb{"N/A"}, \verb{"Not answered"},\cr
\verb{"Don't know"}, \verb{"(Don't know)"}, \verb{"No answer"}, \verb{"No opinion"},\cr
\verb{"Not applicable"}, \verb{"Not relevant"}, \verb{"Refused"}, \verb{"(Refused)"},\cr
\verb{"Refused / no answer"}, \verb{"(Refused / no answer)"},\cr
\verb{"Can't say"}, \verb{"Don't know / Can't say"}.

If batch processing multiple files, the function will inspect all files in the
provided directory, and retain only those with the extension  \verb{.R} or \verb{.r}
or DDI versions with the extension \verb{.xml} or \verb{.XML} (it will
subsequently generate an error if the .R files do not contain an object list,
or if the \verb{.xml} files do not contain a DDI structured metadata file).

If the metadata directory contains a subdirectory called \verb{"data"} or
\verb{"Data"}, it will match the name of the metadata file with the name of the
\verb{.csv} file (their names have to be *exactly* the same, irrespective of
their extension).

The \verb{csv} argument can provide a data.frame object produced by reading the
\verb{.csv} file, or a path to the directory where the \verb{.csv} files are located.
If the user doesn't provide something for this argument, the function will check
the existence of a subdirectory called \verb{data} in the directory where the metadata
files are located.

The \verb{uniqueid} argument is only relevant if \verb{type = "R"}. It is necessary
to identify missing observations in different variables, based on the unique case
identifiers found in the variable provided via this argument. It will generate an
"attr"ibute called \verb{"missing types"}, which is essentially a list whose components
are variable names, and each component is a list itself containing a vector of
values for each missing category (type) plus the identifiers of the cases where
missing values are found (and replaced with NA).
It will also generate an attribute called \verb{"unique id"}, which points to the same
name of the variable containing the unique case identifiers.

The argument \verb{SD} only makes sense when \verb{type = "Stata"} or \verb{type = "all"},
(when Stata setup files will also be generated).

In batch mode, the code starts with the argument \verb{delimiter = ","}, but if the
\verb{.csv} file is delimited differently it will also try hard to find other delimiters
which will match the variable names in the metadata file. At the initial version 0.1-0,
the automatically detected delimiters include \verb{";"} and \verb{"\t"}.

The argument \verb{OS} can be either:\cr
\verb{"windows"} (default), or \verb{"Windows"}, \verb{"Win"}, \verb{"win"},\cr
\verb{"MacOS"}, \verb{"Darwin"}, \verb{"Apple"}, \verb{"Mac"}, \verb{"mac"},\cr
\verb{"Linux"}, \verb{"linux"}.

The end of line separator changes only when the target OS is different from the
running OS.

The argument \verb{outfile} expects the name of the final setup file being saved
on the disk. If nothing is provided, the name of the object provided for the
\verb{lbls} argument will be used as a filename.

There is also an undocumented, boolean argument called \verb{saveFile}, which if
set to TRUE it will save an R version if the metadata was read from a DDI \verb{.xml}
file, in the same directory. This function uses \verb{\link{getMetadata}}, where that
argument is a formal one.
}

\value{
A setup file to complement the imported raw dataset.
}


\examples{
test <- list()

test$varlab <- list(
"ID" = "Questionnaire ID",
"V1" = "Label for the first variable",
"V2" = "Label for the second variable",
"V3" = "Label for the third variable"
)


test$vallab$V1 <- c(
"No"             =  0, 
"Yes"            =  1,
"Not answered"   = -1
)


test$vallab$V2 <- c(
"Verry little"   =  1, 
"Little"         =  2,
"So, so"         =  3,
"Much"           =  4,
"Very much"      =  5,
"Not applicable" = -7,
"Don't know"     = -8,
"Not answered"   = -9
)


test$vallab$V3 <- c(
"No"             =  0, 
"Yes"            =  1,
"Not answered"   = -1
)


# 
###   IMPORTANT:
##### make sure to set the working directory to a directory with read/write permissions
###
# setwd("/path/to/read/write/directory")


##### then run these commands
###
# path.to.csv <- file.path(system.file(package = "DDIwR"), "data", "test.csv.gz")
# setupfile(test, trymiss = TRUE, csv = path.to.csv, uniqueid = "ID")


# setupfile(test, csv = path.to.csv, type="Stata", SD=";")


##### other types of possible utilizations, using paths to specific files
###
# setupfile("/path/to/the/metadata/file.xml", csv="/path/to/csv/file.csv")


##### if the metadata is saved to an .R file containing a list
###
# setupfile("/path/to/the/metadata/file.R", csv="/path/to/csv/file.csv")


##### or in batch mode, specifying entire directories
###
# setupfile("/path/to/the/metadata/directory", csv="/path/to/csv/directory")

}

\keyword{functions}
