test_that("twop_cloglog_gradient() gives expected function values", {
  expect_equal(twop_cloglog_gradient(1, c(1, 1)), exp(-exp(2) + 2) * c(1, 1))
  expect_equal(twop_cloglog_gradient(1, c(-1, 1)), exp(-1) * c(1, 1))
  expect_equal(twop_cloglog_gradient(-1, c(-1, 1)), exp(-exp(-2) - 2) * c(1, -1))
  expect_equal(twop_cloglog_gradient(0, c(0, 0)), exp(-1) * c(1, 0))
})

test_that("twop_cloglog_gradient() gives expected output for Inf input", {
  expect_equal(twop_cloglog_gradient(Inf, c(1, 1)), c(NaN, NaN))
  expect_equal(twop_cloglog_gradient(1, c(Inf, 1)), c(NaN, NaN))
  expect_equal(twop_cloglog_gradient(1, c(1, Inf)), c(NaN, NaN))
})

test_that("twop_cloglog_gradient() gives expected output for -Inf input", {
  expect_equal(twop_cloglog_gradient(-Inf, c(1, 1)), c(0, NaN))
  expect_equal(twop_cloglog_gradient(1, c(-Inf, 1)), c(0, 0))
  expect_equal(twop_cloglog_gradient(1, c(1, -Inf)), c(0, 0))
})

test_that("twop_cloglog_gradient() gives NaN", {
  expect_equal(twop_cloglog_gradient(NaN, c(1, 1)), c(NaN, NaN))
  expect_equal(twop_cloglog_gradient(1, c(NaN, 1)), c(NaN, NaN))
  expect_equal(twop_cloglog_gradient(1, c(1, NaN)), c(NaN, NaN))
})
