context("category_vector")

test_that("matrix", {
  # create data
  x <- matrix(c(1, 0, 0, NA, 0, 1, 0, NA, 0, 0, 0, NA), ncol = 3)
  # create category vector
  y <- category_vector(x)
  # run tests
  expect_equal(y, c(1, 2, 0, NA))
})

test_that("data.frame", {
  # create data
  x <- as.data.frame(matrix(c(1, 0, 0, NA, 0, 1, 0, NA, 0, 0, 0, NA), ncol = 3))
  # create category vector
  y <- category_vector(x)
  # run tests
  expect_equal(y, c(1, 2, 0, NA))
})

test_that("invalid inputs", {
  expect_error(category_vector(data.frame(integer(0), integer(0))))
  expect_error(category_vector(data.frame(a = 1, b = 2)))
  expect_error(category_vector(data.frame(a = 1, b = -1)))
  expect_error(category_vector(data.frame(a = 1, b = "a")))
  expect_error(category_vector(matrix(c(1, 2), ncol = 2)))
  expect_error(category_vector(matrix(c(1, -1), ncol = 2)))
  expect_error(category_vector(matrix(c("a", "b"), ncol = 2)))
})
