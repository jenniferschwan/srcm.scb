#' Returns the gradient values of the gph model
#'
#' @param t the input times
#' @param params the 2 parameters for gph
#'
#' @return value of the gph gradient at times t for params
#' @export
#'
#' @examples
#' gph_gradient(0.5, c(1, 1))
gph_gradient <- function(t, params) {
  t^params[2] / (params[1] + t^params[2]) * c(1, params[1] * log(t))
}
