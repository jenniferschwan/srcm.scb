#' Calculates the SCB of type proposed 2 according to Subramanian
#'
#' @param surv_data the input data, must contain columns 'time' and 'event'
#' @param estimatorfunction the plug-in estimator which is used to calculate the SCB
#' @param modelfunction the modelfunction used for the srcm
#' @param modelfunction_gradient the gradient of the modelfunction used for the srcm
#' @param alpha confidence level
#' @param n_boot number of bootstrap repetitions
#'
#' @return a tibble containing the columns 'time', 'event',
#'          rank', 'estimated_survival', 'SCB_low' and 'SCB_high'
#' @export
#'
#' @examples
#' surv_data <- dplyr::tibble(time = c(1:5), event = c(1, 0, 1, 0, 1))
#' SCB_subramanian_type2(surv_data, estimator_dikta98, gph, gph_gradient, 0.05, 100)
SCB_subramanian_type2 <- function(surv_data,
                                  estimatorfunction, modelfunction, modelfunction_gradient,
                                  alpha, n_boot) {
  checkmate::assert_names(names(surv_data), must.include = c("time", "event"))

  # convert functionnames to function references
  if (is.character(estimatorfunction)) {
    estimatorfunction <- get(estimatorfunction)
  }
  if (is.character(modelfunction)) {
    modelfunction <- get(modelfunction)
  }
  if (is.character(modelfunction_gradient)) {
    modelfunction_gradient <- get(modelfunction_gradient)
  }

  mle <- calculate_mle_srcm(surv_data, modelfunction)
  surv_data <- estimatorfunction(surv_data, mle, modelfunction)
  n <- nrow(surv_data)

  surv_data <- dplyr::mutate(surv_data, estimated_var = estimate_variance_srcm(surv_data, mle, modelfunction, modelfunction_gradient))

  boot_W2s <- sort(
    replicate(
      n_boot,
      bootstrap_srcm(
        sup_W2star_stat,
        surv_data, mle,
        estimatorfunction, modelfunction
      )
    )
  )
  q_alpha <- boot_W2s[floor((1 - alpha) * n_boot)]

  surv_data <- dplyr::mutate(surv_data,
    SCB_low = pmax(0, estimated_survival - 1 / sqrt(n) * estimated_survival * sqrt(estimated_var) * q_alpha),
    SCB_high = pmin(1, estimated_survival + 1 / sqrt(n) * estimated_survival * sqrt(estimated_var) * q_alpha)
  )
  surv_data$SCB_high[n] <- pmin(1, surv_data$estimated_survival[n] + 1 / sqrt(n) * surv_data$estimated_survival[n - 1] * sqrt(surv_data$estimated_var[n - 1]) * q_alpha)
  dplyr::select(surv_data, !estimated_var) # remove estimated variance from surv_data
}
