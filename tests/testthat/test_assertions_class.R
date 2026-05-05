test_that("is_matrix_ish", {
  # run tests
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
})

test_that("is_multi_conservation_problem", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  # prepare data
  b1 <- 0.8 * terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]]
  f1 <- sim_features[[1]]
  f2 <- sim_features[[2]]
  # build problem
  p <- multi_problem(
    obj1 = problem(sim_zones_pu_raster[[1]], f1) %>%
      add_max_wtd_sum_objective(budget = b1) %>%
      add_binary_decisions(),
    obj2 = problem(sim_zones_pu_raster[[1]], f2) %>%
      add_max_wtd_sum_objective(budget = b1) %>%
      add_binary_decisions()
  )
  # run tests
  expect_true(is_multi_conservation_problem(p))
  expect_false(is_multi_conservation_problem(new_waiver()))
  expect_false(
    is_multi_conservation_problem(
      structure(1, class = c("ConservationProblem", "pproto"))
    )
  )
  expect_false(
    is_multi_conservation_problem(
      problem(sim_zones_pu_raster[[1]], f1)
    )
  )
  expect_error(
    assert(is_multi_conservation_problem(new_waiver())),
    "problem"
  )
  expect_error(
    assert(
      is_multi_conservation_problem(
        problem(sim_zones_pu_raster[[1]], f1)
      )
    ),
    "problem"
  )
})

test_that("is_generic_conservation_problem", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  # prepare data
  b1 <- 0.8 * terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]]
  f1 <- sim_features[[1]]
  f2 <- sim_features[[2]]
  # build problem
  p1 <-
    problem(sim_zones_pu_raster[[1]], f1) %>%
    add_max_wtd_sum_objective(budget = b1) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_zones_pu_raster[[1]], f2) %>%
    add_max_wtd_sum_objective(budget = b1) %>%
    add_binary_decisions()
  mp <- multi_problem(obj1 = p1, obj2 = p2)
  # run tests
  expect_true(is_generic_conservation_problem(p1))
  expect_true(is_generic_conservation_problem(mp))
  expect_false(is_generic_conservation_problem(new_waiver()))
  expect_error(
    assert(is_generic_conservation_problem(new_waiver())),
    "problem"
  )
})
