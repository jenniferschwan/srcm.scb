#' Calculates the width of the given SCB at the given quantiles
#'
#' @param SCB the given SCB, must contain the columns 'estimated_survival', 'SCB_low' and 'SCB_high'
#' @param quantiles the quantiles (values between 0 and 1) at which the width should be calculated
#'
#' @return the widths of the given SCB at the given quantiles
#' @export
#'
SCB_width_at_quantiles <- function(SCB, quantiles) {
  checkmate::assert_names(names(SCB), must.include = c("SCB_low", "SCB_high"))
  checkmate::assert_double(quantiles, lower = 0, upper = 1)

  n <- nrow(SCB)

  index_of_quantiles <- floor(1 + quantiles * (n - 1))

  SCB$SCB_high[index_of_quantiles] - SCB$SCB_low[index_of_quantiles]
}
