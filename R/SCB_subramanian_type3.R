#' Calculates the SCB of type proposed 3 according to Subramanian
#'
#' @param surv_data the input data, must contain columns 'time' and 'event'
#' @param estimatorfunction the plug-in estimator which is used to calculate the SCB
#' @param modelfunction the modelfunction used for the srcm
#' @param alpha confidence level
#' @param n_boot number of bootstrap repetitions
#'
#' @return a tibble containing the columns 'time', 'event',
#'          rank', 'estimated_survival', 'SCB_low' and 'SCB_high'
#' @export
#'
#' @examples
#' surv_data <- dplyr::tibble(time = c(1:5), event = c(1, 0, 1, 0, 1))
#' SCB_subramanian_type3(surv_data, estimator_dikta98, gph, 0.05, 100)
SCB_subramanian_type3 <- function(surv_data,
                                  estimatorfunction, modelfunction,
                                  alpha, n_boot) {
  checkmate::assert_names(names(surv_data), must.include = c("time", "event"))

  # convert functionnames to function references
  if (is.character(estimatorfunction)) {
    estimatorfunction <- get(estimatorfunction)
  }
  if (is.character(modelfunction)) {
    modelfunction <- get(modelfunction)
  }

  mle <- calculate_mle_srcm(surv_data, modelfunction)
  surv_data <- estimatorfunction(surv_data, mle, modelfunction)
  n <- nrow(surv_data)

  boot_Ws <- sort(
    replicate(
      n_boot,
      bootstrap_srcm(
        sup_Wstar_stat,
        surv_data, mle,
        estimatorfunction, modelfunction
      )
    )
  )
  q_alpha <- boot_Ws[floor((1 - alpha) * n_boot)]

  inner_exponent <- 1 / sqrt(n) * q_alpha / (surv_data$estimated_survival * log(surv_data$estimated_survival))
  inner_exponent[surv_data$estimated_survival == 0] <- -Inf # weil sonst NaN hier Grenzwert einsetzen

  surv_data %>%
    dplyr::mutate(
      SCB_low = pmax(0, estimated_survival^exp(-inner_exponent)),
      SCB_high = pmin(1, estimated_survival^exp(inner_exponent))
    )
}
