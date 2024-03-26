#' Returns the gradient values of the twop_cloglog model
#'
#' @param t the input times
#' @param params the 2 parameters for cloglog model
#'
#' @return value of the twop_cloglog gradient at times t for params
#' @export
#'
#' @examples
#' twop_cloglog_gradient(0.5, c(1, 1))
twop_cloglog_gradient <- function(t, params) {
  1 - exp(-exp(params[1] + params[2] * t) + params[1] + params[2] * t) * c(1, t)
}
