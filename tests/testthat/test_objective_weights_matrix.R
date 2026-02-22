test_that("default parameters", {
  # create object
  x <- objective_weights_matrix(3, 5)
  # run tests
  ## format
  expect_equal(ncol(x), 3)
  expect_equal(nrow(x), 199)
  ## values
  # all weights between 0 and 1
  expect_true(all(x >= 0 & x <= 1))
  # extremes included (diagonal)
  expect_true(any(apply(x, 1, function(r) all(r == c(1,0,0)))))
  expect_true(any(apply(x, 1, function(r) all(r == c(0,1,0)))))
  expect_true(any(apply(x, 1, function(r) all(r == c(0,0,1)))))
  # equal weighting row present
  expect_true(any(apply(x, 1, function(r) all(abs(r - 1) < 1e-8))))
})

test_that("include_zeros = FALSE", {
  x <- objective_weights_matrix(3, 5, include_zeros = FALSE)
  
  ## format
  expect_equal(ncol(x), 3)
  
  ## values
  # no zeros except in extremes
  expect_false(any(x[-c(2,3,4), ] == 0))
  # all values within 0-1
  expect_true(all(x >= 0 & x <= 1))
})

test_that("include_extremes = FALSE", {
  x <- objective_weights_matrix(3, 5, include_extremes = FALSE)
  
  ## format
  expect_equal(ncol(x), 3)
  expect_equal(nrow(x), 199-3)
  
  # extreme single-objective rows should not be present
  extreme_rows <- rbind(c(1,0,0), c(0,1,0), c(0,0,1))
  for(i in 1:nrow(extreme_rows)) {
    expect_false(any(apply(x, 1, function(r) all(abs(r - extreme_rows[i,]) < 1e-8))))
  }
  
  # equal weighting row should still be present
  expect_true(any(apply(x, 1, function(r) all(abs(r - 1) < 1e-8))))
  
  # all values between 0 and 1
  expect_true(all(x >= 0 & x <= 1))
  
  
})

test_that("invalid inputs", {
  expect_error(objective_weights_matrix(0, 5))
  expect_error(objective_weights_matrix(3, 0))
  expect_error(objective_weights_matrix(3, 5, include_zeros = "yes"))
  expect_error(objective_weights_matrix(3, 5, include_extremes = NA))
})
