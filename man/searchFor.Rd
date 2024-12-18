% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/searchFor.R
\name{searchFor}
\alias{searchFor}
\title{Search for key words}
\usage{
searchFor(
  x,
  where = c("everywhere", "title", "description", "attributes", "examples"),
  ...
)
}
\arguments{
\item{x}{Character, either word(s) or a regular expression.}

\item{where}{Character, in which section(s) to search for.}

\item{...}{Other arguments to be passed to the grepl() function.}
}
\value{
Character vector of DDI element names.
}
\description{
Search function to return elements that contain a certain
word or regular expression pattern.
}
\author{
Adrian Dusa
}
