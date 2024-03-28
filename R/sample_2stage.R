#' Generates a bootstrap sample with the two-stage bootstrap method
#'
#' @param surv_data the input sample which is resampled
#' @param mle the mle of surv_data for the modelfunction
#' @param modelfunction the modelfunction used for resampling under srcm
#'
#' @return a bootstrap sample
#' @export
#'
#' @examples
#' surv_data <- dplyr::tibble(time = c(1:5), event = c(1, 0, 1, 0, 1))
#' mle <- calculate_mle_srcm(surv_data, gph)
#' sample_2stage(surv_data, mle, gph)
sample_2stage <- function(surv_data, mle, modelfunction) {
  checkmate::assert_names(names(surv_data), must.include = c("time"))

  dplyr::tibble(
    time = sample(surv_data$time, replace = TRUE),
    event = sapply(modelfunction(time, mle), function(m) {
      stats::rbinom(n = 1, size = 1, prob = m)
    })
  )
}
