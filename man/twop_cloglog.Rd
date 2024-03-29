% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twop_cloglog.R
\name{twop_cloglog}
\alias{twop_cloglog}
\title{2-parameter complementary log-log (twop_cloglog) model function}
\usage{
twop_cloglog(t, params)
}
\arguments{
\item{t}{the value of t at which the function should be evaluated}

\item{params}{parameters params = c(p1,p2)}
}
\value{
value of the model with parameters p1 and p2
at the given value of t:  \cr \cr
1 - exp(-exp(p1 + p2 * t))
}
\description{
2-parameter complementary log-log (twop_cloglog) model function
}
\examples{
twop_cloglog(0.2, params = c(10, 2))
}
