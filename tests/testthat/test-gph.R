test_that("gph() returns expected positive value", {
  expect_equal(gph(1, par = c(1, 1)), 0.5)
})

test_that("gph() returns zero", {
  expect_equal(gph(0, par = c(0, 0)), 0)
})

test_that("gph() returns NaN in case of zero division", {
  expect_equal(gph(0, par = c(0, 1)), NaN)
})

test_that("gph() returns Inf", {
  expect_equal(gph(-1, par = c(1, 1)), Inf)
})

test_that("gph() returns expected negative value", {
  expect_equal(gph(-1, par = c(0.5, 1)), -1)
})
