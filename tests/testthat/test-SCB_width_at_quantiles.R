test_that("SCB_width_at_quantiles() gives error for missing SCB_high in input", {
  SCB <- dplyr::tibble(
    SCB_low = c(0, 0, 0, 0, 0)
  )

  expect_error(SCB_width_at_quantiles(SCB = SCB, quantiles = 0))
})

test_that("SCB_width_at_quantiles() gives error for missing SCB_low in input", {
  SCB <- dplyr::tibble(
    SCB_high = c(0, 0, 0, 0, 0)
  )

  expect_error(SCB_width_at_quantiles(SCB = SCB, quantiles = 0))
})

test_that("SCB_width_at_quantiles() gives error for quantiles not between 0 and 1", {
  SCB <- dplyr::tibble(
    SCB_low  = c(0, 0, 0, 0, 0),
    SCB_high = c(1, 1, 1, 1, 1)
  )

  expect_error(SCB_width_at_quantiles(SCB = SCB, quantiles = -1))
  expect_error(SCB_width_at_quantiles(SCB = SCB, quantiles = 1.1))
  expect_error(SCB_width_at_quantiles(SCB = SCB, quantiles = c(0, -3, 1)))
})

test_that("SCB_width_at_quantiles() gives no error for quantiles 0 and 1", {
  SCB <- dplyr::tibble(
    SCB_low  = c(0, 0, 0, 0, 0),
    SCB_high = c(1, 1, 1, 1, 1)
  )

  expect_no_error(SCB_width_at_quantiles(SCB = SCB, quantiles = c(0, 1)))
})

test_that("SCB_width_at_quantiles() gives expected output for one input quantile and n even", {
  SCB <- dplyr::tibble(
    SCB_low  = rep(0, n = 11),
    SCB_high = c(1:10)
  )
  quantiles <- c(0.5)

  expect_equal(SCB_width_at_quantiles(SCB, quantiles), 5)
})

test_that("SCB_width_at_quantiles() gives expected output for one input quantile and n uneven", {
  SCB <- dplyr::tibble(
    SCB_low  = rep(0, n = 11),
    SCB_high = c(1:11)
  )
  quantiles <- c(0.5)

  expect_equal(SCB_width_at_quantiles(SCB, quantiles), 6)
})

test_that("SCB_width_at_quantiles() at 0 and 1 quantile", {
  SCB <- dplyr::tibble(
    SCB_low  = rep(0, n = 10),
    SCB_high = c(1:10)
  )
  quantiles <- c(0, 1)

  expect_equal(SCB_width_at_quantiles(SCB, quantiles), c(1, 10))
})

test_that("SCB_width_at_quantiles() at multiple quantiles for n = 100", {
  SCB <- dplyr::tibble(
    SCB_low  = rep(0, n = 100),
    SCB_high = c(1:100)
  )
  quantiles <- c(0:10) * 0.1

  expect_equal(SCB_width_at_quantiles(SCB, quantiles), c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
})

test_that("SCB_width_at_quantiles() at 100 quantiles for n = 100", {
  SCB <- dplyr::tibble(
    SCB_low  = rep(0, n = 100),
    SCB_high = c(1:100)
  )
  quantiles <- c(0:100) * 0.01

  expect_equal(SCB_width_at_quantiles(SCB, quantiles), c(1, 1:100))
})
