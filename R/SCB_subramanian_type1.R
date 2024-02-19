#' Calculates the SCB of type proposed 1 according to Subramanian
#'
#' @param surv_data
#' @param estimatorfunction
#' @param modelfunction
#' @param mlefunction
#' @param alpha
#' @param n_boot
#'
#' @return
#' @export
#'
#' @examples
SCB_subramanian_type1 <- function(surv_data,
                                  estimatorfunction, modelfunction, mlefunction,
                                  alpha, n_boot) {
  # convert functionnames to function references because of how simTool works
  if (is.character(estimatorfunction)) {
    estimatorfunction <- get(estimatorfunction)
  }
  if (is.character(modelfunction)) {
    modelfunction <- get(modelfunction)
  }
  if (is.character(mlefunction)) {
    mlefunction <- get(mlefunction)
  }

  mle <- mlefunction(surv_data, modelfunction)
  surv_data <- estimatorfunction(surv_data, modelfunction, mle)
  n <- length(surv_data$time)

  boot_Ws <- sort(
    replicate(
      n_boot,
      sup_W_stat(
        surv_data,
        sample_2stage(surv_data, modelfunction, mle),
        estimatorfunction,
        modelfunction,
        mlefunction
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
