\name{getMetadata}

\alias{getMetadata}

\title{Get metadata information from an XML file}

\description{
This function reads an XML file containing a DDI codebook version 2.5 and
returns the variable labels and value labels.
}

\usage{
getMetadata(xmlpath, OS = "windows", saveFile = FALSE, ...)
}

\arguments{
  \item{xmlpath}{A path to an XML file with a DDI codebook}
  \item{OS}{The target operating system, for the eol - end of line separator}
  \item{saveFile}{Boolean, save an .R file in the same folder}
  \item{...}{Additional arguments for this function (internal uses only)}
}

\details{
The argument \code{OS} can be either:
"windows" (default), or "Windows", "Win", "win"
"MacOS", "Darwin", "Apple", "Mac", "mac"
"Linux", "linux"

The end of line separator changes only when the target OS is different from the
running OS.

For the moment, only DDI version 2.5 (Codebook) is supported, but the next version
will include the latest DDI version 3.2 (Lifecycle).
}


\value{
A list containing two components
"varlab" for all variable labels, and
"vallab" for all value labels
}



\keyword{functions}
