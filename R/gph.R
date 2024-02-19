#' Generalized proportional hazard (GPH) model function
#'
#' @param t the value of t at which the function should be evaluated
#' @param params c(theta1, theta2) of the parameters theta1 and theta2 for gph
#'
#' @return value of the GPH model function with parameters theta1 and theta2
#'  at the given value of t:  \cr \cr
#'  theta1 / (theta1 + t^theta2)
#' @export
#'
#' @examples
#' gph(0.2, params = c(10, 2))
gph <- function(t, params) {
  params[1] / (params[1] + t^params[2])
}
