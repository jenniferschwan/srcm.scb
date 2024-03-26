test_that("SCB_covering() gives error for incorrect SCB input", {
  SCB <- dplyr::tibble(
    time     = c(1, 2, 3, 4, 5),
    SCB_low  = c(0, 0, 0, 0, 0)
  )
  S_true <- function(t) {
    0
  }

  expect_error(SCB_covering(SCB = SCB, S_true = S_true))
})

test_that("SCB_covering() gives error for S_true not beeing a function", {
  SCB <- dplyr::tibble(
    time     = c(1, 2, 3, 4, 5),
    SCB_low  = c(0, 0, 0, 0, 0),
    SCB_high = c(1, 1, 1, 1, 1)
  )
  S_true <- 0

  expect_error(SCB_covering(SCB = SCB, S_true = S_true))
})

test_that("SCB_covering() gives true for trivial case", {
  SCB <- dplyr::tibble(
    time     = c(1, 2, 3, 4, 5),
    SCB_low  = c(0, 0, 0, 0, 0),
    SCB_high = c(1, 1, 1, 1, 1)
  )
  S_true <- function(t) {
    0.5
  }

  expect_equal(SCB_covering(SCB = SCB, S_true = S_true), TRUE)
})

test_that("SCB_covering() gives false for trivial case", {
  SCB <- dplyr::tibble(
    time     = c(1, 2, 3, 4, 5),
    SCB_low  = c(0, 0, 0, 0, 0),
    SCB_high = c(1, 1, 1, 1, 1)
  )
  S_true <- function(t) {
    2
  }

  expect_equal(SCB_covering(SCB = SCB, S_true = S_true), FALSE)
})

test_that("SCB_covering() finds breakout of function at SCB_high", {
  SCB <- dplyr::tibble(
    time     = c(0.1, 0.3, 0.35, 0.7, 0.9),
    SCB_low  = c(0.8, 0.6, 0.4, 0.2, 0),
    SCB_high = c(1, 0.8, 0.6, 0.4, 0.2)
  )
  S_true <- function(t) {
    1 - t
  }

  expect_equal(SCB_covering(SCB = SCB, S_true = S_true), FALSE)
})

test_that("SCB_covering() finds breakout of function at SCB_low", {
  SCB <- dplyr::tibble(
    time     = c(0.1, 0.3, 0.65, 0.7, 0.9),
    SCB_low  = c(0.8, 0.6, 0.4, 0.2, 0),
    SCB_high = c(1, 0.8, 0.6, 0.4, 0.2)
  )
  S_true <- function(t) {
    1 - t
  }

  expect_equal(SCB_covering(SCB = SCB, S_true = S_true), FALSE)
})

test_that("SCB_covering() allows value to be exactly at the limits at SCB_high (test <=)", {
  SCB <- dplyr::tibble(
    time     = c(0.1, 0.3, 0.6, 0.7, 0.9),
    SCB_low  = c(0, 0.2, 0.4, 0.6, 0.8),
    SCB_high = c(0.2, 0.4, 0.6, 0.8, 1)
  )
  S_true <- function(t) {
    t
  }

  expect_equal(SCB_covering(SCB = SCB, S_true = S_true), TRUE)
})

test_that("SCB_covering() allows value to be exactly at the limits at SCB_low (test >=)", {
  SCB <- dplyr::tibble(
    time     = c(0.1, 0.3, 0.4, 0.7, 0.9),
    SCB_low  = c(0, 0.2, 0.4, 0.6, 0.8),
    SCB_high = c(0.2, 0.4, 0.6, 0.8, 1)
  )
  S_true <- function(t) {
    t
  }

  expect_equal(SCB_covering(SCB = SCB, S_true = S_true), TRUE)
})
