#' Slices the SCB according to the given quantiles
#'
#' @param SCB the SCB which should be made a subset of itself, must include column 'time'
#' @param lower_quantile the lower quantile (between 0 and 1)
#' @param upper_quantile the upper quantile (between 0 and 1)
#'
#' @return A subset of the input SCB
#' @export
#'
SCB_get_quantile_subset <- function(SCB, lower_quantile = 0, upper_quantile = 1) {
  checkmate::assert_names(names(SCB), must.include = "time")
  checkmate::assert_double(lower_quantile, lower = 0, upper = 1)
  checkmate::assert_double(upper_quantile, lower = 0, upper = 1)

  idx <- which(
    SCB$time >= SCB$time[floor(nrow(SCB) * lower_quantile) + 1]
    &
    SCB$time <= SCB$time[floor(nrow(SCB) * upper_quantile)]
    )

  dplyr::slice(SCB, idx)

}
