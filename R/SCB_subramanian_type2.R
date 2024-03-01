#' Title
#'
#' @param surv_data
#' @param estimatorfunction
#' @param modelfunction
#' @param modelfunction_gradient
#' @param alpha
#' @param n_boot
#'
#' @return
#' @export
#'
#' @examples
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

  surv_data <- dplyr::mutate(surv_data, estimated_var = estimate_variance(surv_data, mle, modelfunction, modelfunction_gradient))

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
  plot(1:500,boot_W2s)
  q_alpha <- boot_W2s[floor((1 - alpha) * n_boot)]

  surv_data %>%
    dplyr::mutate(
      SCB_low = pmax(0, estimated_survival - 1 / sqrt(n) * estimated_survival * sqrt(estimated_var) * q_alpha),
      SCB_high = pmin(1, estimated_survival + 1 / sqrt(n) * estimated_survival * sqrt(estimated_var) * q_alpha)
    ) %>%
    dplyr::select(!estimated_var) #remove estimated variance from surv_data
}
