test_that("SCB_enclosed_area_subramanian() gives error for missing SCB_high in input", {
  SCB <- dplyr::tibble(
    time = c(1:10),
    SCB_low = c(1:10)
  )

  expect_error(SCB_enclosed_area_subramanian(SCB = SCB))
})

test_that("SCB_enclosed_area_subramanian() gives error for missing SCB_low in input", {
  SCB <- dplyr::tibble(
    time = c(1:10),
    SCB_high = c(1:10)
  )

  expect_error(SCB_enclosed_area_subramanian(SCB = SCB))
})

test_that("SCB_enclosed_area_subramanian() gives error for missing time in input", {
  SCB <- dplyr::tibble(
    SCB_low  = c(1:10),
    SCB_high = c(1:10)
  )

  expect_error(SCB_enclosed_area_subramanian(SCB = SCB))
})

test_that("SCB_enclosed_area_subramanian() gives expected output for a simple SCB with fixed width", {
  SCB <- dplyr::tibble(
    time = c(0, 1, 2),
    SCB_low = c(0, 0, 0),
    SCB_high = c(1, 1, 1)
  )

  expect_equal(SCB_enclosed_area_subramanian(SCB), 2)
})

test_that("SCB_enclosed_area_subramanian() gives expected output for a more complex SCB with fixed width", {
  SCB <- dplyr::tibble(
    time = c(0.1, 0.2, 0.4, 0.6, 0.9),
    SCB_low = rep(0, n = 5),
    SCB_high = rep(1, n = 5)
  )

  expect_equal(SCB_enclosed_area_subramanian(SCB), 0.8)
})

test_that("SCB_enclosed_area_subramanian() gives expected output for a more complex SCB with fixed SCB_high", {
  SCB <- dplyr::tibble(
    time = c(0.1, 0.2, 0.4, 0.6, 0.9),
    SCB_low = c(0.8, 0.6, 0.35, 0.1, 0),
    SCB_high = rep(1, n = 5)
  )

  expect_equal(SCB_enclosed_area_subramanian(SCB), 0.5)
})

test_that("SCB_enclosed_area_subramanian() gives expected output for a more complex SCB with variable width", {
  SCB <- dplyr::tibble(
    time = c(0.1, 0.2, 0.4, 0.7, 0.9),
    SCB_low = c(0.8, 0.6, 0.35, 0.1, 0),
    SCB_high = c(1, 0.7, 0.45, 0.25, 0.2)
  )

  expect_equal(SCB_enclosed_area_subramanian(SCB), 0.1)
})
