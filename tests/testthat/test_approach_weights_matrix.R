test_that("default parameters", {
  # create object
  x <- approach_weights_matrix(3, 5)
  # run tests
  ## format
  expect_equal(ncol(x), 3)
  expect_equal(nrow(x), 112)
  ## values
  expect_true(all(x >= 0 & x <= 1))
  ### extremes included (diagonal)
  expect_true(any(apply(x, 1, function(r) all(r == c(1,0,0)))))
  expect_true(any(apply(x, 1, function(r) all(r == c(0,1,0)))))
  expect_true(any(apply(x, 1, function(r) all(r == c(0,0,1)))))
  ## equal weighting row present
  expect_true(any(apply(x, 1, function(r) all(abs(r - 1) < 1e-8))))
})

test_that("include_zeros = FALSE", {
  # create object
  x <- approach_weights_matrix(
    3, 5, include_zeros = FALSE, include_extremes = FALSE
  )
  # run tests
  ## format
  expect_equal(ncol(x), 3)
  ## no zeros except in extremes
  expect_false(any(x == 0))
  ## all values within 0-1
  expect_true(all(x > 0 & x <= 1))
})

test_that("include_extremes = FALSE", {
  # create object
  x <- approach_weights_matrix(3, 5, include_extremes = FALSE)
  # run tests
  ## format
  expect_equal(ncol(x), 3)
  expect_equal(nrow(x), 112 - 3)
  ## extreme single-objective rows should not be present
  ## in other words, we expect no sets of weights to contain two zeros
  expect_false(any(rowSums(abs(x) < 1e-15) == 2L))
  ## equal weighting row should still be present
  expect_true(any(rowSums(abs(x - 1) < 1e-15) == 3L))
  ## all values between 0 and 1
  expect_true(all(x >= 0 & x <= 1))
})

test_that("invalid inputs", {
  # run tests
  expect_error(approach_weights_matrix(0, 5))
  expect_error(approach_weights_matrix(3, 0))
  expect_error(approach_weights_matrix(3, 5, include_zeros = "yes"))
  expect_error(approach_weights_matrix(3, 5, include_extremes = NA))
})
