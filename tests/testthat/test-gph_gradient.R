test_that("gph_gradient() returns expected positive values", {
  expect_equal(gph_gradient(1, par = c(1, 1)), c(0.25, 0))
  expect_equal(gph_gradient(3, par = c(5, 3)), c(27 / 1024, 135 / 1024 * log(3)))
  expect_equal(gph_gradient(2, par = c(-200, 10)), c(16 / 10609, -3200 / 10609 * log(2)))
})

test_that("gph_gradient() returns zero", {
  expect_equal(gph_gradient(1, par = c(0, 1)), c(1, 0))
})

test_that("gph_gradient() returns NaN in case of zero division", {
  expect_equal(gph_gradient(0, par = c(0, 1)), c(NaN, NaN))
})

test_that("gph_gradient() returns -Inf & NaN", {
  expect_warning(val <- gph_gradient(-1, par = c(1, 1)))
  expect_equal(val, c(-Inf, NaN))
})

test_that("gph_gradient() returns expected NaN for negative t", {
  expect_warning(val <- gph_gradient(-1, par = c(0.5, 1)))
  expect_equal(val, c(-4, NaN))
})
