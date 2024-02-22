#' The statistic which provides the supremum of W*
#' @noRd
#'
#' @param surv_data the original data, must contain columns 'time', 'event' and 'estimated_survival'
#' @param boot_data the bootstrapped data, must contain columns 'time', 'event' and 'estimated_survival'
#'
#' @return sup (W* = sqrt(n) * abs(S(t) - S*(t)))
sup_Wstar_stat <- function(surv_data, boot_data) {
  checkmate::assert_names(names(surv_data), must.include = c("time", "event", "estimated_survival"))
  checkmate::assert_names(names(boot_data), must.include = c("time", "event", "estimated_survival"))

  n <- nrow(surv_data)
  S1 <- surv_data$estimated_survival
  # S2 are the bootstrap estimated values at original data times
  S2 <- sapply(
    surv_data$time,
    function(t) {
      min(1, boot_data$estimated_survival[boot_data$time <= t])
    }
  )

  sqrt(n) * max(abs(S1 - S2), abs(S1 - c(1, S2[-n])), abs(c(1, S1[-n]) - S2))
}
