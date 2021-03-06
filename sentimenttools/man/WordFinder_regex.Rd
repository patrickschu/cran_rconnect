% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sentimenttools.R
\name{WordFinder_regex}
\alias{WordFinder_regex}
\title{WordFinder_regex}
\usage{
WordFinder_regex(input_text, search_term, regex_search = FALSE,
  lower_case = TRUE)
}
\arguments{
\item{input_text:}{string}

\item{search_term:}{string that will be checked for existence in input_text}

\item{lower_case:}{if `TRUE`, all words in input_text are lower cased before the search}

\item{regex_search:}{if `TRUE`, uses a limited version regular expression pattern matching: the `*` wildcard is used to match 0 to infinite characters; `?` matches a single character.}
}
\value{
TRUE / FALSE
}
\description{
WordFinder_regex is built on top of Wordfinder. It takes a string as input_text and returns TRUE if search_term is contained.
}
\details{
see GlobToRegex for details on regex_search
NB THIS WILL MESS WITH EMOTICONS, NO?
NB this get real messed up if user inputs whitespace; we could strip it but do we want to?
}
\examples{
wordfinder("This is a sentence", "sentence")
}
