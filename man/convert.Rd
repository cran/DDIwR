\name{convert}

\alias{convert}

\title{Convert a dataset from one statistical software to another}

\description{
This function converts (or transfers) between R, Stata, SPSS, SAS, and DDI XML files.
Unlike the regular import / export functions from packages \bold{\pkg{haven}} or
\bold{\pkg{rio}}, this function uses the DDI standard as an exchange platform and
also attempts a conversion of the missing values.
}

\usage{
convert(from, to, embed = FALSE, binpath = "", ...)
}

\arguments{
  \item{from}{A path to a file, or a tibble object}
  \item{to}{Character, the name of a software package or a path to a specific file}
  \item{embed}{Boolean, embed the data when generating a DDI XML file}
  \item{binpath}{Path to the binary executable file, to run a recoding script}
  \item{...}{Additional parameters passed to exporting functions, see the Details section}
}

\details{
When the argument \bold{\code{to}} specifies a certain statistical package (\code{"R"},
\code{"Stata"}, \code{"SPSS"} or \code{"SAS"}), the name of the destination file will be the
same as the name of the input file from the argument \bold{\code{to}}, with an automatically
added software specific extension.

Alternatively, the argument \bold{\code{to}} can be specified as a path to a specific file, in
which case the software package is determined from its file extension. The following extentions
are currently recognized: \code{.xml} for DDI, \code{.rds} for R, \code{.dta} for Stata,
\code{.sav} for SPSS and \code{.sas7bdat} for SAS.

The argument \bold{\code{binpath}} is used only for Stata (if installed on the local machine),
to coerce regular missing values to their specific missing values using letters from \code{a} to
\code{z}, given that package \bold{\pkg{haven}} does not convert Stata missing values by default.
Specifying the path to the binary executable file is also a Boolean signal to attempt converting
the missing values via an automatic script that recodes all unique missing values to the same
letters, the lowest numerical value being assigned to the letter \code{a}.

Additional parameters can be specified via the three dots argument \bold{\code{...}}, that are
passed to the respective functions from package \bold{\pkg{haven}}. For instance the function
\bold{\code{\link[haven]{write_dta}()}} has an additional argument called \bold{\code{version}}
(from 8 to a maximum and default value of 14) when writing a Stata file.

Note that this function creates a target file in the same directory as the source file, which is
different from importing the source file into R. To import a file, users should refer to the
specific functions from package \bold{\pkg{haven}}, such as \bold{\code{\link[haven]{read_sav}()}}
or \bold{\code{\link[haven]{read_dta}()}} etc., and be aware the result object is a
\bold{\code{tibble}}.

The current version reads and creates DDI Codebook version 2.5, with future versions to extend
the functionality for DDI Lifecycle versions 3.x and link to the future package \bold{DDI4R}
for the UML model based version 4. It extends the standard DDI Codebook by offering the possibility
to embed a CSV version of the raw data into the XML file containing the Codebook, into a
\code{notes} child of the \code{fileDscr} component. This type of Codebook is unique to this
package and automatically detected when converting to another statistical software.

Future versions will attempt to extend converting the missing values to SAS types, but otherwise
users can also use a setup file produced by function \bold{\code{\link{setupfile}()}} and run the
commands manually.

When importing a file, the R object of choice is a tibble because is the only type of object in R
that allows specifying multiple (coded) missing values. It also plays nicely with the SPSS types of
variables, which are the most commonly used in the social sciences.
}


\author{
Adrian Dusa
}

\references{
DDI - Data Documentation Initiative, see \href{https://www.ddialliance.org/}{https://www.ddialliance.org/}
}


\seealso{\code{\link{setupfile}}, \code{\link{getMetadata}}, \code{\link[tibble]{tibble}}, \code{\link[haven]{labelled}}, \code{\link[haven]{labelled_spss}}} 


\examples{
\dontrun{
# Assuming an SPSS file called test.sav is located in the working directory
# the following command will extract the metadata in a DDI Codebook and
# produce a test.xml file in the same directory
convert("test.sav", to = "DDI")

# It is possible to include the data in the XML file, using:
convert("test.sav", to = "DDI", embed = TRUE)

# To produce a Stata file:
convert("test.sav", to = "Stata")

# Since Stata has different types of missing values than SPSS, it is
# possible to transform these missing values via an automatically run script
# using the argument "binpath", assuming that Stata is installed

# The paths to the binaries differ in various operating systems. A possible
# path for Windows, for Stata version 12 could be:
binpath <- "C:/Progra~1/Stata12/Stata.exe"

# For MacOS, the path could be:
binpath <- "/Applications/Stata/Stata.app/Contents/MacOS/Stata"

# The final command, which also converts to Stata types of missing values
convert("test.sav", to = "Stata", binpath = binpath)
}
}

\keyword{functions}