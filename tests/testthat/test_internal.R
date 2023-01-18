context("internal functions")

test_that("matrix_to_triplet_dataframe", {
  skip_if_not(utils::packageVersion("Matrix") >= 1.3)
  expect_equal(
    matrix_to_triplet_dataframe(
      Matrix::sparseMatrix(i = 1:3, j = c(1, 1, 2), x = 4:6, repr = "T")
    ),
    data.frame(i = 1:3, j = c(1, 1, 2), x = 4:6)
  )
  expect_equal(
    matrix_to_triplet_dataframe(
      Matrix::sparseMatrix(i = 1:3, j = c(1, 1, 2), x = 4:6, repr = "C")
    ),
    data.frame(i = 1:3, j = c(1, 1, 2), x = 4:6)
  )
  expect_equal(
    matrix_to_triplet_dataframe(
      Matrix::sparseMatrix(
        i = 1:3, j = c(1, 1, 2), x = 4:6, repr = "C", symmetric = TRUE
      )
    ),
    data.frame(i = 1:3, j = c(1, 1, 2), x = 4:6)
  )
})
