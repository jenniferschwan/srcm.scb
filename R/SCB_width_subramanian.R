#' Calculates the width of the given SCB along the lines of Subramanian
#' "state the formula here"
#'
#' @param SCB the given SCB, must contain the columns 'estimated_survival', 'SCB_low' and 'SCB_high'
#'
#' @return the width of the given SCB
#' @export
#'
SCB_width_subramanian <- function(SCB) {
  checkmate::assert_names(names(SCB), must.include = c("estimated_survival", "SCB_low", "SCB_high"))

  l <- SCB$SCB_high - SCB$SCB_low
  delta_S <- c(1, SCB$estimated_survival[-nrow(SCB)]) - SCB$estimated_survival

  sum(l * delta_S)
}
