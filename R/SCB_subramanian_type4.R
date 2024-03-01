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
SCB_subramanian_type4 <- function(surv_data,
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
  q_alpha <- boot_W2s[floor((1 - alpha) * n_boot)]

  inner_exponent <- 1 / sqrt(n) * q_alpha * sqrt(surv_data$estimated_var) / (log(surv_data$estimated_survival))
  #inner_exponent[surv_data$estimated_survival == 0] = -Inf # weil sonst NaN hier Grenzwert einsetzen

  surv_data <- dplyr::mutate(surv_data,
      SCB_low = pmax(0, estimated_survival^exp(-inner_exponent)),
      SCB_high = pmin(1, estimated_survival^exp(inner_exponent))
    )
  surv_data$SCB_high[n] <- surv_data$SCB_high[n-1]
  dplyr::select(surv_data, !estimated_var) #remove estimated variance from surv_data
}
