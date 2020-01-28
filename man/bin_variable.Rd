% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bin_variable.R
\name{bin_variable}
\alias{bin_variable}
\title{Bin a numeric variable}
\usage{
bin_variable(x, min = NULL, max = NULL, int = NULL, quant = NULL)
}
\arguments{
\item{x}{numeric vector, variable to binned}

\item{min}{numeric, where to start binning}

\item{max}{numeric, where to end binnig}

\item{int}{numeric, interval length of bins}

\item{quant, }{interger, number of quantiles}
}
\value{
vector of bin labels
}
\description{
bin_variable() is a function that creates bins of either equal interval 
 length of equal size (quartiles)
}