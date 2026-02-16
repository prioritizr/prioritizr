test_that("default parameters", {
  # create object
  x <- objective_weights_matrix(3, 5)
  # run tests
  ## format
  expect_equal(ncol(x), 3)
  expect_equal(nrow(x), 199)
  ## values
  stop("TODO")
})

test_that("include_zeros = FALSE", {
  stop("TODO")
})

test_that("include_extremes = FALSE", {
  stop("TODO")
})

test_that("invalid inputs", {
  stop("TODO")
})
