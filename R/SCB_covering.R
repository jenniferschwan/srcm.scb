#' Checks whether the SCB fully covers the true survival function or not
#'
#' @param SCB the given SCB, must contain the columns 'time', 'SCB_low' and 'SCB_high'
#' @param S_true the true survival function
#'
#' @return TRUE if the SCB fully covers the true survival function, FALSE if not
SCB_covering <- function(SCB, S_true) {
  checkmate::assert_names(names(SCB), must.include = c("time", "SCB_low", "SCB_high"))

  all(SCB$SCB_high >= S_true(SCB$time)) && all(S_true(SCB$time)[-1] >= SCB$SCB_low[-nrow(SCB)])
}
