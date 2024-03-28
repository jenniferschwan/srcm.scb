#' Estimates the variance under srcm at all given times of the input sample
#'
#' @param surv_data the input sample, must contain columns "time"
#' @param mle the mle of the given sample and the given model under srcm
#' @param modelfunction the modelfunction to be used
#' @param modelfunction_gradient the gradient of the modelfunction to be used
#'
#' @return the variance of the given sample at all surv_data$time
#' @export
#'
#' @examples
#' surv_data <- dplyr::tibble(time = c(1:5), event = c(1, 0, 1, 0, 1))
#' mle <- calculate_mle_srcm(surv_data, gph)
#' estimate_variance_srcm(surv_data, mle, gph, gph_gradient)
estimate_variance_srcm <- function(surv_data, mle, modelfunction, modelfunction_gradient) {
  checkmate::assert_names(names(surv_data), must.include = c("time"))

  n <- nrow(surv_data)

  gradient_values_for_times <- sapply(surv_data$time, modelfunction_gradient, params = mle)
  modelvalues_for_times <- sapply(surv_data$time, modelfunction, params = mle)

  fisher_information <- array(dim = c(length(mle), length(mle)))
  for (r in 1:length(mle)) {
    for (s in 1:r) {
      fisher_information[r, s] <- sum(gradient_values_for_times[r, ] * gradient_values_for_times[s, ] /
        (modelvalues_for_times * (1 - modelvalues_for_times)))
      fisher_information[s, r] <- fisher_information[r, s]
    }
  }
  fisher_information <- 1 / n * fisher_information
  fisher_information_inverse <- solve(fisher_information) # calculate inverse

  estimated_variance <- array(dim = n)

  for (k in 1:n) {
    sum1 <- 0
    sum2 <- 0
    for (i in 1:k) {
      sum1 <- sum1 + modelvalues_for_times[i]^2 / (1 + 1 / n - i / n)^2
      for (j in 1:k) {
        alpha_i_j <- gradient_values_for_times[, i] %*% (fisher_information_inverse %*% gradient_values_for_times[, j])
        sum2 <- sum2 + (alpha_i_j) / ((1 + 1 / n - i / n) * (1 + 1 / n - j / n))
      }
    }
    estimated_variance[k] <- 1 / n * sum1 + 1 / n^2 * sum2
  }

  estimated_variance
}
