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

test_that("SCB_width_subramanian() gives expected output for a simple SCB with fixed width", {
  SCB <- dplyr::tibble(
    estimated_survival = c(1, 0.5, 0),
    SCB_low = c(0, 0, 0),
    SCB_high = c(1, 1, 1)
  )

  expect_equal(SCB_width_subramanian(SCB), 1)
})

test_that("SCB_width_subramanian() gives expected output for a more complex SCB with fixed width", {
  SCB <- dplyr::tibble(
    estimated_survival = c(0.9, 0.7, 0.4, 0.2, 0.1),
    SCB_low = rep(0, n = 5),
    SCB_high = rep(1, n = 5)
  )

  expect_equal(SCB_width_subramanian(SCB), 0.9)
})

test_that("SCB_width_subramanian() gives expected output for a more complex SCB with fixed SCB_high", {
  SCB <- dplyr::tibble(
    estimated_survival = c(0.9, 0.7, 0.4, 0.2, 0.1),
    SCB_low = c(0.8, 0.6, 0.35, 0.1, 0),
    SCB_high = rep(1, n = 5)
  )

  expect_equal(SCB_width_subramanian(SCB), 0.575)
})

test_that("SCB_width_subramanian() gives expected output for a more complex SCB with variable width", {
  SCB <- dplyr::tibble(
    estimated_survival = c(0.9, 0.7, 0.4, 0.2, 0.1),
    SCB_low = c(0.8, 0.6, 0.35, 0.1, 0),
    SCB_high = c(1, 0.7, 0.45, 0.25, 0.2)
  )

  expect_equal(SCB_width_subramanian(SCB), 0.12)
})
