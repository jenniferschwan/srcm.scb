#' 2-parameter complementary log-log (twop_cloglog) model function
#'
#' @param t the value of t at which the function should be evaluated
#' @param params parameters params = c(p1,p2)
#'
#' @return value of the model with parameters p1 and p2
#'  at the given value of t:  \cr \cr
#'  1 - exp(-exp(p1 + p2 * t))
#' @export
#'
#' @examples
#' twop_cloglog(0.2, params = c(10, 2))
twop_cloglog <- function(t, params) {
  1 - exp(-exp(params[1] + params[2] * t))
}
