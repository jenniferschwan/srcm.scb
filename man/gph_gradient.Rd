% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gph_gradient.R
\name{gph_gradient}
\alias{gph_gradient}
\title{Returns the gradient values of the gph model}
\usage{
gph_gradient(t, params)
}
\arguments{
\item{t}{the input times}

\item{params}{the 2 parameters for gph}
}
\value{
value of the gph gradient at times t for params
}
\description{
Returns the gradient values of the gph model
}
\examples{
gph_gradient(0.5, c(1, 1))
}
