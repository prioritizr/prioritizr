test_that("is_matrix_ish", {
  expect_true(is_matrix_ish(matrix(c(1, 2, 3))))
  expect_true(is_matrix_ish(Matrix::Matrix(c(1, 2, 3))))
  expect_false(is_matrix_ish(new_waiver()))
  expect_error(assert(is_matrix_ish(new_waiver())), "matrix")
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

test_that("is_method", {
  expect_true(is_method(spec_jung_targets()))
  expect_false(is_method("a"))
  expect_error(assert(is_method("a")), "method object")
})

test_that("is_inherits", {
  expect_true(is_inherits("a", "character"))
  expect_true(is_inherits("a", c("character", "integer")))
  expect_false(is_inherits("a", "integer"))
  expect_error(assert(is_inherits("a", "integer")))
})

test_that("is_spatially_explicit", {
  expect_true(is_spatially_explicit(get_sim_pu_raster()))
  expect_false(is_spatially_explicit("a"))
  expect_error(assert(is_spatially_explicit("a")), "spatially explicit")
})

test_that("is_spatially_explicit", {
  expect_true(is_spatially_explicit(get_sim_pu_raster()))
  expect_false(is_spatially_explicit("a"))
  expect_error(assert(is_spatially_explicit("a")), "spatially explicit")
})

test_that("all_elements_inherit", {
  expect_true(all_elements_inherit(list("a"), "character"))
  expect_true(all_elements_inherit(list("a", "b"), "character"))
  expect_false(all_elements_inherit(list("a"), "integer"))
  expect_false(all_elements_inherit(list("a", "b"), "integer"))
  expect_error(
    assert(all_elements_inherit(list("a", 2), "character")),
    "elements"
  )

test_that("is_multi_conservation_problem", {

})

test_that("is_generic_conservation_problem", {

})
