test_that("sample_2stage() works for seed 0 with simple modelfunction which is always 1", {
  set.seed(0)

  surv_data <- dplyr::tibble(
    time = c(1:10)
  )

  modelfunction <- function(t, parameter){
    1
  }
  mle <- 0

  val <- sample_2stage(surv_data, modelfunction, mle)
  expect_equal(nrow(val), 10)
  expect_equal(val$time, c(9, 4, 7, 1, 2, 7, 2, 3, 1, 5))
  expect_equal(val$event, c(rep(1, times = 10)))
})

test_that("sample_2stage() works for seed 0 with simple modelfunction which is always 0", {
  set.seed(0)

  surv_data <- dplyr::tibble(
    time = c(1:10)
  )

  modelfunction <- function(t, parameter){
    0
  }
  mle <- 0

  val <- sample_2stage(surv_data, modelfunction, mle)
  expect_equal(nrow(val), 10)
  expect_equal(val$time, c(9, 4, 7, 1, 2, 7, 2, 3, 1, 5))
  expect_equal(val$event, c(rep(0, times = 10)))
})

test_that("sample_2stage() works for seed 0 with simple modelfunction which is only 1 for t = 1 ", {
  set.seed(0)

  surv_data <- dplyr::tibble(
    time = c(1:10)
  )

  modelfunction <- function(t, param){
    ret <- rep(0, times = length(t))
    ret[t==1] <- param
    ret
  }
  mle <- 1

  val <- sample_2stage(surv_data, modelfunction, mle)
  expect_equal(nrow(val), 10)
  expect_equal(val$time, c(9, 4, 7, 1, 2, 7, 2, 3, 1, 5))
  expect_equal(val$event, c(0, 0, 0, 1, 0 , 0, 0, 0, 1, 0))
})

test_that("sample_2stage() works for seed 45653 with simple modelfunction which is always 1", {
  set.seed(45653)

  surv_data <- dplyr::tibble(
    time = c(1:10)
  )

  modelfunction <- function(t, parameter){
    1
  }
  mle <- 0

  val <- sample_2stage(surv_data, modelfunction, mle)
  expect_equal(nrow(val), 10)
  expect_equal(val$time, c(8, 6, 8, 3, 5, 8, 4, 2, 4, 4))
  expect_equal(val$event, c(rep(1, times = 10)))
})

test_that("sample_2stage() works for seed 45653 with simple modelfunction which is always 0", {
  set.seed(45653)

  surv_data <- dplyr::tibble(
    time = c(1:10)
  )

  modelfunction <- function(t, parameter){
    0
  }
  mle <- 0

  val <- sample_2stage(surv_data, modelfunction, mle)
  expect_equal(nrow(val), 10)
  expect_equal(val$time, c(8, 6, 8, 3, 5, 8, 4, 2, 4, 4))
  expect_equal(val$event, c(rep(0, times = 10)))
})

test_that("sample_2stage() works for seed 45653 with simple modelfunction which is only 1 for t = 4 ", {
  set.seed(45653)

  surv_data <- dplyr::tibble(
    time = c(1:10)
  )

  modelfunction <- function(t, param){
    ret <- rep(0, times = length(t))
    ret[t==4] <- param
    ret
  }
  mle <- 1

  val <- sample_2stage(surv_data, modelfunction, mle)
  expect_equal(nrow(val), 10)
  expect_equal(val$time, c(8, 6, 8, 3, 5, 8, 4, 2, 4, 4))
  expect_equal(val$event, c(0, 0, 0, 0, 0 , 0, 1, 0, 1, 1))
})

test_that("sample_2stage() throws error if time is missing in input", {
  surv_data <- dplyr::tibble()
  expect_error(sample_2stage(surv_data, 0, NULL))
})
