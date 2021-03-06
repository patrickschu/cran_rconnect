% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sentimenttools.R
\name{DictMaker}
\alias{DictMaker}
\title{DictMaker}
\usage{
DictMaker(text_vector, threshold = 10, to_lower = FALSE,
  remove_punctuation = FALSE, remove_numbers = FALSE,
  remove_stopwords = FALSE, stemming = FALSE)
}
\arguments{
\item{text_vector}{a flat vector of string, not tokenized; if directly out of \code{data.frame}, \code{paste} and \code{collapse} first.}

\item{threshold}{items with a token count lower than threshold will not be returned}

\item{to_lower}{input to termFreq's \code{tolower} parameter; TRUE or FALSE for lower casing}

\item{remove_punctuation}{TRUE or FALSE for removing punctuation}

\item{remove_numbers}{TRUE or FALSE for removing numbers}

\item{stemming}{TRUE or FALSE; can supply external stemming function}

\item{remove_stopword}{FALSE for none; TRUE for standard tm set; input vector for custom stopword set}
}
\value{
a \code{matrix} of word,count
}
\description{
The DictMaker calls on tm's \code{termFreq()} to create a matrix of word counts in text_vector.
}
\details{
this uses the tm library's \url{https://cran.r-project.org/web/packages/tm/index.html} \code{termFreq} \url{https://www.rdocumentation.org/packages/tm/versions/0.7-1/topics/termFreq} function. 
Note that stemming increases processing time like nobody's business.
}
