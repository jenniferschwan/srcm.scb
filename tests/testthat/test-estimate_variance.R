test_that("estimate variance works", {
  surv_data <- dplyr::tibble(time = c(1, 2, 3, 4, 5), event = c(1, 0, 1, 0, 1))
  var <- estimate_variance(
    surv_data,
    calculate_mle_srcm(surv_data, gph),
    gph,
    gph_gradient
  )
  expect_equal(
    estimate_variance(surv_data, calculate_mle_srcm(surv_data, gph), gph, gph_gradient),
    array(c(0.12605268, 0.31231156, 0.59171585, 1.19140438, 3.86675972))
  )
})
