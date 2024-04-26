test_that("binary decisions", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create and solve problem
  s <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_rsymphony_solver(first_feasible = TRUE, verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_true(inherits(s, "SpatRaster"))
  expect_equal(terra::nlyr(s), 1)
  expect_true(all_binary(s))
  expect_true(is_comparable_raster(sim_pu_raster, s))
})

test_that("proportion decisions", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create and solve problem
  s <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions() %>%
    add_rsymphony_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_true(inherits(s, "SpatRaster"))
  expect_equal(terra::nlyr(s), 1)
  expect_gte(min(terra::values(s), na.rm = TRUE), 0)
  expect_lte(max(terra::values(s), na.rm = TRUE), 1)
  expect_gt(
    max(terra::values(s) - round(terra::values(s)), na.rm = TRUE),
    0.01
  )
  expect_true(is_comparable_raster(sim_pu_raster, s))
})

test_that("proportion decisions (floating point)", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  skip_if_not_installed("prioritizrdata", minimum_version = "0.3.0")
  # import data
  tas_pu <- prioritizrdata::get_tas_pu()[seq_len(15), ]
  tas_features <- prioritizrdata::get_tas_features()
  # create and solve problem
  s <-
    problem(tas_pu, tas_features, cost = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(1) %>%
    add_proportion_decisions() %>%
    add_rsymphony_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_true(inherits(s, "sf"))
  expect_true("solution_1" %in% names(s))
  expect_equal(nrow(s), nrow(tas_pu))
  expect_inherits(s$solution_1, "numeric")
  expect_gte(min(s$solution_1), 0)
  expect_lte(max(s$solution_1), 1)
})

test_that("variable bounds methods", {
  skip_if_not_installed("Rsymphony")
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()#
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_rsymphony_solver(first_feasible = TRUE, verbose = FALSE)
  # modify problem
  p$solver$calculate(compile.ConservationProblem(p))
  p$solver$set_variable_ub(1, 0)
  p$solver$set_variable_lb(2, 1)
  # tests
  expect_equal(
    p$solver$internal$model$bounds$upper$val,
    replace(rep(1, p$number_of_planning_units()), 1, 0)
  )
  expect_equal(
    p$solver$internal$model$bounds$lower$val,
    replace(rep(0, p$number_of_planning_units()), 2, 1)
  )
})

test_that("mix of binary and continuous variables", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate budget
  b <- terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.2
  # create and solve problem
  s <-
    problem(sim_pu_raster, sim_features) %>%
    add_max_utility_objective(b) %>%
    add_binary_decisions() %>%
    add_rsymphony_solver(verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_true(inherits(s, "SpatRaster"))
  expect_equal(terra::nlyr(s), 1)
  expect_true(all_binary(s))
  expect_true(is_comparable_raster(sim_pu_raster, s))
})

test_that("first_feasible", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create and solve problem
  s <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_rsymphony_solver(first_feasible = TRUE, verbose = FALSE) %>%
    solve_fixed_seed()
  # check that solution has correct properties
  expect_inherits(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 1)
  expect_true(all_binary(s))
  expect_true(is_comparable_raster(sim_pu_raster, s))
})

test_that("correct solution (simple)", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # import data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_rsymphony_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve_fixed_seed(p)
  s2 <- solve_fixed_seed(p)
  # test for correct solution
  expect_equal(c(terra::values(s1)), c(0, 1, 1, NA))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("correct solution (complex)", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # import data
  cost <- terra::rast(matrix(c(1000, 100, 200, 300, NA), nrow = 1))
  features <- c(
    terra::rast(matrix(c(5,  5,   0,  0,  NA), nrow = 1)),
    terra::rast(matrix(c(2,  0,   8,  10, NA), nrow = 1)),
    terra::rast(matrix(c(10, 100, 10, 10, NA), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_manual_targets(
      tibble::tibble(
        feature = names(features),
        type = "absolute",
        sense = c("=", ">=", "<="),
        target = c(5, 10, 20)
      )
    ) %>%
    add_rsymphony_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve_fixed_seed(p)
  s2 <- solve_fixed_seed(p)
  # test for correct solution
  expect_equal(c(terra::values(s1)), c(1, 0, 1, 0, NA))
  expect_equal(terra::values(s1), terra::values(s2))
})
