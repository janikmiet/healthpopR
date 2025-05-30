% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{.capitalize}
\alias{.capitalize}
\title{Capitalize the First Letter of a String}
\usage{
.capitalize(s)
}
\arguments{
\item{s}{A character string to be capitalized.}
}
\value{
A character string with the first letter capitalized and the rest in lowercase.
}
\description{
This function takes a character string and returns the same string with the
first letter converted to uppercase and the remaining letters converted to lowercase.
}
\examples{
.capitalize("hello")   # "Hello"
.capitalize("WORLD")   # "World"
.capitalize("rStuDio") # "Rstudio"

}
\keyword{internal}
