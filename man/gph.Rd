% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gph.R
\name{gph}
\alias{gph}
\title{Generalized proportional hazard (GPH) model function}
\usage{
gph(t, params)
}
\arguments{
\item{t}{the value of t at which the function should be evaluated}

\item{params}{parameters params = c(p1,p2) for gph}
}
\value{
value of the GPH model function with parameters p1 and p2
at the given value of t:  \cr \cr
p1 / (p1 + t^p2)
}
\description{
Generalized proportional hazard (GPH) model function
}
\examples{
gph(0.2, params = c(10, 2))
}
