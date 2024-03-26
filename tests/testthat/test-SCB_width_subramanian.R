test_that("SCB_width_subramanian() gives error for missing SCB_high in input", {
  SCB <- dplyr::tibble(
    estimated_survival = c(1:10),
    SCB_low = c(1:10)
  )

  expect_error(SCB_width_subramanian(SCB = SCB))
})

test_that("SCB_width_subramanian() gives error for missing SCB_low in input", {
  SCB <- dplyr::tibble(
    estimated_survival = c(1:10),
    SCB_high = c(1:10)
  )

  expect_error(SCB_width_subramanian(SCB = SCB))
})

test_that("SCB_width_subramanian() gives error for missing estimated_survival in input", {
  SCB <- dplyr::tibble(
    SCB_low  = c(1:10),
    SCB_high = c(1:10)
  )

  expect_error(SCB_width_subramanian(SCB = SCB))
})

test_that("SCB_width_subramanian() gives expected output for a simple SCB", {
  SCB <- dplyr::tibble(
    estimated_survival = c(1:10),
    SCB_low = rep(0, n = 10),
    SCB_high = c(1:10)
  )

  expect_equal(SCB_width_subramanian(SCB), 5)
})
