test_that("rescale_matrix (matrix)", {
  # create data
  d <- matrix(seq_len(12) * 1000, ncol = 3, nrow = 4)
  # run calculations
  x <- rescale_matrix(d, max = 5)
  # tests
  expect_inherits(x, "matrix")
  expect_equal(nrow(x), nrow(d))
  expect_equal(ncol(x), ncol(d))
  expect_equal(max(x), 5, tolerance = 1e-5)
  expect_equal(c(x), c(d) / (max(d) / 5))
})

test_that("rescale_matrix (array)", {
  # create data
  d <- array(seq_len(12 * 2) * 1000, dim = c(3, 4, 2))
  # run calculations
  x <- rescale_matrix(d, max = 5)
  # tests
  expect_inherits(x, "array")
  expect_equal(dim(d), dim(x))
  expect_equal(max(x), 5, tolerance = 1e-5)
  expect_equal(c(x), c(d) / (max(d) / 5))
})

test_that("rescale_matrix (Matrix)", {
  # create data
  d <- Matrix::sparseMatrix(
    i = c(1, 3, 3),
    j = c(2, 3, 4),
    x = c(1000, 3000, 6000),
    dims = c(3, 4)
  )
  # run calculations
  x <- rescale_matrix(d, max = 5)
  # tests
  expect_inherits(x, "dgCMatrix")
  expect_equal(nrow(d), nrow(x))
  expect_equal(ncol(d), ncol(x))
  expect_equal(max(x), 5, tolerance = 1e-5)
  expect_equal(as.matrix(x), as.matrix(d) / (max(d) / 5))
})
