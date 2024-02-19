#' Calculates the MLE for the given data and modelfunction under SRCM
#'
#' @param surv_data the given survival_data. Needs to have the properties time and event
#' @param modelfunction the modelfunction used for the srcm
#'
#' @return the mle of surv_data for the given modelfunction
#' @export
#'
#' @examples
#' surv_data <- dplyr::tibble(time = c(1:5), event = c(1, 0, 1, 0, 1))
#' calculate_mle_srcm(surv_data, gph)
calculate_mle_srcm <- function(surv_data, modelfunction) {
  stats::optim(
    par = c(1, 1),
    fn = negLL_srcm,
    surv_data = surv_data,
    modelfunction = modelfunction,
    method = "Nelder-Mead"
  )$par
}

#' the negative log-likelihood-function under SRCM
#'
#' @param par the parameter for modelfunction
#' @param surv_data the given survival_data. Needs to have the properties time and event
#' @param modelfunction the modelfunction used for the srcm
#'
#' @return the value of the negative log-likelihood-function
negLL_srcm <- function(par, surv_data, modelfunction) {
  m <- modelfunction(surv_data$time, par)
  if (any(m <= 0) || any(1 - m <= 0)) { # na in logs entfernen
    return(100000)
  }
  -mean(surv_data$event * log(m) + (1 - surv_data$event) * log(1 - m))
}
