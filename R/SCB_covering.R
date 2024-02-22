#' Checks whether the SCB fully covers the true survival function or not
#'
#' @param SCB the given SCB, must contain the columns 'time', 'SCB_low' and 'SCB_high'
#' @param true_S the true survival function
#'
#' @return TRUE if the SCB fully covers the true survival function, FALSE if not
SCB_covering <- function(SCB, true_S) {
  checkmate::assert_names(names(SCB), must.include = c("time", "SCB_low", "SCB_high"))

  all(SCB$SCB_high >= true_S(SCB$time)) && all(true_S(SCB$time)[-1] >= SCB$SCB_low[-nrow(SCB)])
}
