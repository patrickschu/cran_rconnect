% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sentimenttools.R
\name{YearMatcher}
\alias{YearMatcher}
\title{YearMatcher}
\usage{
YearMatcher(year_string)
}
\arguments{
\item{year_string, }{a string indicating a year e.g. "2016"}
}
\value{
an integer
}
\description{
YearMatcher is just a convenience function to parse Shiny string input to a format appropriate for comparison
}
\details{
compare this to `as.POSIXlt[["year"]]` numbers. The result is (year_string - 1900) cause that's how POSIXlt rolls. 
see also here \url{https://stat.ethz.ch/R-manual/R-devel/library/base/html/DateTimeClasses.html}
}
