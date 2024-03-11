#' Title
#'
#' @param t
#' @param params
#'
#' @return
#' @export
#'
#' @examples
twop_cloglog_gradient <- function(t, params) {
  1 - exp(-exp(params[1] + params[2] * t) + params[1] + params[2] * t) * c(1, t)
}
