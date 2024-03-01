#' Title
#'
#' @param t
#' @param params
#'
#' @return
#' @export
#'
#' @examples
gph_gradient <- function(t, params) {
  t^params[2] / (params[1] + t^params[2]) * c(1, params[1] * log(t))
}
