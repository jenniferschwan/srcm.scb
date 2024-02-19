#' Calculates the SCB of type proposed 1 according to Subramanian
#'
#' @param surv_data the input data, must contain columns 'time' and 'event'
#' @param estimatorfunction the plug-in estimator which is used to calculate the SCB
#' @param modelfunction the modelfunction used for the srcm
#' @param alpha confidence level
#' @param n_boot number of bootstrap repititions
#'
#' @return a tibble containing the columns 'time', 'event',
#'          rank', 'estimated_survival', 'SCB_low' and 'SCB_high'
#' @export
#'
#' @examples
#' surv_data <- dplyr::tibble(time = c(1:5), event = c(1, 0, 1, 0, 1))
#' SCB_subramanian_type1(surv_data, estimator_dikta98, gph, 0.05, 100)
SCB_subramanian_type1 <- function(surv_data,
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
  n <- length(surv_data$time)

  boot_Ws <- sort(
    replicate(
      n_boot,
      bootstrap(
        surv_data, mle,
        estimatorfunction, modelfunction, sup_Wstar_stat
        )
    )
  )
  q_alpha <- boot_Ws[floor((1 - alpha) * n_boot)]

  surv_data %>%
    dplyr::mutate(
      SCB_low = pmax(0, estimated_survival - 1 / sqrt(n) * q_alpha),
      SCB_high = pmin(1, estimated_survival + 1 / sqrt(n) * q_alpha)
    )
}
