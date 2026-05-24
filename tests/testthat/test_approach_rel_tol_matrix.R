test_that("default parameters", {
  # create object
  x <- approach_rel_tol_matrix(3, 5, max = 1)
  # run tests
  ## format
  expect_equal(ncol(x), 2)
  expect_equal(nrow(x), 15)
  ## values
  expect_true(all(x >= 0 & x <= 1))
  expect_true(all(x[, 1] >= x[, 2]))
})

test_that("include_zeros = FALSE", {
  # create object
  x <- approach_rel_tol_matrix(3, 5, max = 1, include_zeros = FALSE)
  # run tests
  ## format
  expect_equal(ncol(x), 2)
  expect_equal(nrow(x), 15)
  ## values
  expect_true(all(x > 0 & x <= 1))
  expect_true(all(x[, 1] >= x[, 2]))
})

test_that("order = FALSE", {
  # create object
  x <- approach_rel_tol_matrix(3, 5, max = 1, order = FALSE)
  # run tests
  ## format
  expect_equal(ncol(x), 2)
  expect_equal(nrow(x), 25)
  ## values
  expect_true(all(x >= 0 & x <= 1))
  expect_true(any(x[, 1] > x[, 2]))
  expect_true(any(x[, 1] < x[, 2]))
  expect_true(any(x[, 1] == x[, 2]))
})

test_that("invalid inputs", {
  expect_error(approach_rel_tol_matrix(0, 5))
  expect_error(approach_rel_tol_matrix(3, 0))
  expect_error(approach_rel_tol_matrix(3, 0, max = NA_real_))
  expect_error(approach_rel_tol_matrix(3, 0, max = "a"))
  expect_error(approach_rel_tol_matrix(3, 5, include_zeros = "yes"))
  expect_error(approach_rel_tol_matrix(3, 5, include_extremes = NA))
  expect_error(approach_rel_tol_matrix(3, 0, order = "yes"))
  expect_error(approach_rel_tol_matrix(3, 0, order = NA))
})
