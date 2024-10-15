test_that("matrix_to_triplet_dataframe", {
  skip_if_not(utils::packageVersion("Matrix") >= as.package_version("1.3"))
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

test_that("terra_can_process_in_memory", {
  skip_on_cran()
  # import data
  sim_features <- get_sim_features()
  # tests
  expect_true(
    terra_can_process_in_memory(sim_features, n = 1)
  )
  expect_false(
    terra_can_process_in_memory(sim_features, n = 1e100)
  )
})
