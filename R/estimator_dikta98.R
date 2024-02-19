#' Calculates the semiparametric estimator for the survival function
#' given by Dikta 98
#'
#' @param surv_data input data, needs to have the properties time and event
#' @param modelfunction the modelfunction used for the srcm
#' @param mle the mle of surv_data for the modelfunction
#'
#' @return surv_data with additional columns rank and estimated_survival
#' @export
#'
#' @examples
#' surv_data <- dplyr::tibble(time = c(1:5), event = c(1, 0, 1, 0, 1))
#' mle <- calculate_mle_srcm(surv_data, gph)
#' estimator_dikta98(surv_data, gph, mle)
estimator_dikta98 <- function(surv_data, modelfunction, mle) {
  if ('estimated_survival' %in% names(surv_data)) {
    stop("surv_data already contains column estimated survival")
  }
  surv_data %>%
    dplyr::mutate(rank = rank(time, ties.method = "average")) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      estimated_survival = cumprod(1 - modelfunction(time, mle) /
        (length(time) - rank + 1))
    )
}
