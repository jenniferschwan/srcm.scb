#' Bootstraps under SRCM with the 2-stage bootstrap method
#' @noRd
#'
#' @param surv_data the input data, must contain columns 'time', 'event' and 'estimated_survival'
#' @param mle the mle of surv_data for the modelfunction
#' @param estimatorfunction the function used to estimate the survival for the bootstrap sample,
#'                          should be the same which has been called on surv_data before
#' @param modelfunction the modelfunction used for resampling under srcm
#' @param statfunction the statistics function which is called with surv_data and boot_data
#'
#' @return the value of statfunction(surv_data, boot_data)
bootstrap_srcm <- function(surv_data, mle, estimatorfunction, modelfunction, statfunction) {
  checkmate::assert_names(names(surv_data), must.include = c("time", "event", "estimated_survival"))

  boot_data <- sample_2stage(surv_data, mle, modelfunction)
  boot_mle <- calculate_mle_srcm(boot_data, modelfunction)
  boot_data <- estimatorfunction(boot_data, boot_mle, modelfunction)

  statfunction(surv_data, boot_data)
}
