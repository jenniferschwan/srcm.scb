#' Generalized proportional hazard (GPH) model function
#'
#' @param t the value of t at which the function should be evaluated
#' @param params parameters params = c(p1,p2) for gph
#'
#' @return value of the GPH model function with parameters p1 and p2
#'  at the given value of t:  \cr \cr
#'  p1 / (p1 + t^p2)
#' @export
#'
#' @examples
#' gph(0.2, params = c(10, 2))
gph <- function(t, params) {
  params[1] / (params[1] + t^params[2])
}
