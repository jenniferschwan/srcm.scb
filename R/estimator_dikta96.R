#' Calculates the semiparametric estimator for the survival function
#' given by Dikta 96
#'
#' @param surv_data input data, needs to have the properties 'time' and 'event'
#' @param mle the mle of surv_data for the modelfunction
#' @param modelfunction the modelfunction used for the srcm
#'
#' @return surv_data with additional columns 'rank' and 'estimated_survival'
#' @export
#'
#' @examples
#' surv_data <- dplyr::tibble(time = c(1:5), event = c(1, 0, 1, 0, 1))
#' mle <- calculate_mle_srcm(surv_data, gph)
#' estimator_dikta96(surv_data, mle, gph)
estimator_dikta96 <- function(surv_data, mle, modelfunction) {
  checkmate::assert_names(names(surv_data), must.include = c("time", "event"))
  checkmate::assert_names(names(surv_data), disjunct.from = "estimated_survival")

  n <- nrow(surv_data)
  surv_data %>%
    dplyr::mutate(rank = rank(time, ties.method = "average")) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      estimated_survival = cumprod(
        ((n - rank) / (n - rank + 1))^modelfunction(time, mle)
      )
    )
}
