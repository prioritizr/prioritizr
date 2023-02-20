context("assertions (class)")

test_that("is_matrix_ish", {
  expect_true(is_matrix_ish(matrix(c(1, 2, 3))))
  expect_true(is_matrix_ish(Matrix::Matrix(c(1, 2, 3))))
  expect_false(is_matrix_ish(new_waiver()))
  expect_error(assert(is_matrix_ish(new_waiver())), "matrix")
})

test_that("is_integer", {
  expect_true(is_integer(c(1, 2, 3, NA)))
  expect_false(is_integer(c(0.5, 2, 3, NA)))
  expect_false(is_integer("a"))
  expect_error(assert(is_integer("a")), "integer")
})

test_that("is_conservation_problem", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # tests
  expect_true(is_conservation_problem(problem(sim_pu_raster, sim_features)))
  expect_false(is_conservation_problem(new_waiver()))
  expect_false(
    is_conservation_problem(
      structure(1, class = c("ConservationProblem", "pproto"))
    )
  )
  expect_error(
    assert(is_conservation_problem(new_waiver())),
    "problem"
  )
  expect_error(
    assert(
      is_conservation_problem(
        structure(1, class = c("ConservationProblem", "pproto"))
      )
    ),
    "version"
  )
})
