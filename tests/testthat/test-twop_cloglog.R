test_that("twop_cloglog() gives expected function values", {
  expect_equal(twop_cloglog(1, c(1, 1)), 1 - exp(-exp(2)))
  expect_equal(twop_cloglog(1, c(-1, 1)), 1 - exp(-1))
  expect_equal(twop_cloglog(-1, c(-1, 1)), 1 - exp(-exp(-2)))
  expect_equal(twop_cloglog(0, c(0, 0)), 1 - exp(-1))
})

test_that("twop_cloglog() gives expected output for Inf input", {
  expect_equal(twop_cloglog(Inf, c(1, 1)), 1)
  expect_equal(twop_cloglog(1, c(Inf, 1)), 1)
  expect_equal(twop_cloglog(1, c(1, Inf)), 1)
})

test_that("twop_cloglog() gives expected output for -Inf input", {
  expect_equal(twop_cloglog(-Inf, c(1, 1)), 0)
  expect_equal(twop_cloglog(1, c(-Inf, 1)), 0)
  expect_equal(twop_cloglog(1, c(1, -Inf)), 0)
})

test_that("twop_cloglog() gives NaN", {
  expect_equal(twop_cloglog(NaN, c(1, 1)), NaN)
  expect_equal(twop_cloglog(1, c(NaN, 1)), NaN)
  expect_equal(twop_cloglog(1, c(1, NaN)), NaN)
})
