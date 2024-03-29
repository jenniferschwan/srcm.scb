% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SCB_get_quantile_subset.R
\name{SCB_get_quantile_subset}
\alias{SCB_get_quantile_subset}
\title{Slices the SCB according to the given quantiles}
\usage{
SCB_get_quantile_subset(SCB, lower_quantile = 0, upper_quantile = 1)
}
\arguments{
\item{SCB}{the SCB which should be made a subset of itself, must include column 'time'}

\item{lower_quantile}{the lower quantile (between 0 and 1)}

\item{upper_quantile}{the upper quantile (between 0 and 1)}
}
\value{
A subset of the input SCB
}
\description{
Slices the SCB according to the given quantiles
}
