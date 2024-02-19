#' Title
#'
#' @param surv_data
#' @param boot_data
#' @param estimatorfunction
#' @param modelfunction
#' @param mlefunction
#'
#' @return
#' @export
#'
#' @examples
sup_W_stat <- function(surv_data, boot_data,
                       estimatorfunction, modelfunction, mlefunction) {
  boot_mle <- mlefunction(boot_data, modelfunction)
  boot_data <- estimatorfunction(boot_data, modelfunction, boot_mle)

  n <- length(surv_data$time)
  S1 <- surv_data$estimated_survival
  # S2 are the bootstrap datas estimated values at original data times
  S2 <- sapply(
    surv_data$time,
    function(t) {
      min(1, boot_data$estimated_survival[boot_data$time <= t])
    }
  )

  # calculate supremum of W*= sqrt(n)*(S(t)-S*(t))
  sqrt(n) * max(abs(S1 - S2), abs(S1 - c(1, S2[-n])), abs(c(1, S1[-n]) - S2))
}
