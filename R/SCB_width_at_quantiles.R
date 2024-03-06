#' Title
#'
#' @param SCB
#' @param quantiles
#'
#' @return
#' @export
#'
#' @examples
SCB_width_at_quantiles <- function(SCB, quantiles){
  checkmate::assert_names(names(SCB), must.include = c("SCB_low", "SCB_high"))

  n <- nrow(SCB)

  index_of_quantiles <- floor(quantiles * n)

  SCB$SCB_high[index_of_quantiles] - SCB$SCB_low[index_of_quantiles]
}
