test_that("binary decisions", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_cbc_solver(time_limit = 5, verbose = FALSE)
  p2 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_cbc_solver(time_limit = 5, verbose = FALSE)
  s1 <- solve(p1)
  s2 <- solve(p2)
  # check that solution has correct properties
  expect_inherits(s1, "SpatRaster")
  expect_equal(terra::nlyr(s1), 1)
  expect_equal(sort(unique(c(terra::values(s1)))), c(0, 1))
  expect_equal(terra::values(s1), terra::values(s2))
  expect_true(is_comparable_raster(sim_pu_raster, s1))
})

test_that("proportion decisions", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions() %>%
    add_cbc_solver(gap = 0, verbose = FALSE, presolve = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_inherits(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 1)
  expect_gte(min(c(terra::values(s)), na.rm = TRUE), 0)
  expect_lte(max(c(terra::values(s)), na.rm = TRUE), 1)
  expect_gt(
    max(
      c(terra::values(s)) - round(c(terra::values(s))),
      na.rm = TRUE
    ), 0.01
  )
  expect_true(is_comparable_raster(sim_pu_raster, s))
})

test_that("proportion decisions (floating point)", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  skip_if_not_installed("prioritizrdata", minimum_version = "0.3.0")
  # load data
  tas_pu <- prioritizrdata::get_tas_pu()
  tas_features <- prioritizrdata::get_tas_features()
  tas_pu <- tas_pu[seq_len(15), ]
  # create problem
  p <-
    problem(tas_pu, tas_features, cost = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(1) %>%
    add_proportion_decisions() %>%
    add_cbc_solver(gap = 0, verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "sf"))
  expect_true("solution_1" %in% names(s))
  expect_equal(nrow(s), nrow(tas_pu))
  expect_inherits(s$solution_1, "numeric")
  expect_gte(min(s$solution_1), 0)
  expect_lte(max(s$solution_1), 1)
})

test_that("variable bounds methods", {
  skip_if_not_installed("rcbc")
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_cbc_solver(verbose = TRUE)
  # modify problem
  p$solver$calculate(compile.ConservationProblem(p))
  p$solver$set_variable_ub(1, 0)
  p$solver$set_variable_lb(2, 1)
  # check that  solution has correct properties
  expect_equal(
    p$solver$internal$model$col_ub,
    replace(rep(1, p$number_of_planning_units()), 1, 0)
  )
  expect_equal(
    p$solver$internal$model$col_lb,
    replace(rep(0, p$number_of_planning_units()), 2, 1)
  )
})

test_that("mix of binary and continuous variables", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  b <- terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.2
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_max_utility_objective(b) %>%
    add_binary_decisions() %>%
    add_cbc_solver(verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "SpatRaster"))
  expect_equal(terra::nlyr(s), 1)
  expect_equal(sort(unique(c(terra::values(s)))), c(0, 1))
  expect_true(is_comparable_raster(sim_pu_raster, s))
})

test_that("first_feasible", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create and solve problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_cbc_solver(first_feasible = TRUE, verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_inherits(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 1)
  expect_equal(sort(unique(c(terra::values(s)))), c(0, 1))
  expect_true(is_comparable_raster(sim_pu_raster, s))
})

test_that("correct solution (simple)", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  terra::set.names(features, make.unique(names(features)))
  # create problems
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_cbc_solver(gap = 0, verbose = FALSE)
  # solve problems
  s1 <- solve(p)
  s2 <- solve(p)
  # test for correct solution
  expect_equal(c(terra::values(s1)), c(0, 1, 1, NA))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("correct solution (complex)", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # create data
  cost <- terra::rast(matrix(c(1000, 100, 200, 300, NA), nrow = 1))
  features <- c(
    terra::rast(matrix(c(5,  5,   0,  0,  NA), nrow = 1)),
    terra::rast(matrix(c(2,  0,   8,  10, NA), nrow = 1)),
    terra::rast(matrix(c(10, 100, 10, 10, NA), nrow = 1))
  )
  terra::set.names(features, make.unique(names(features)))
  # create problems
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_manual_targets(
      tibble::tibble(
        feature = names(features),
        type = "absolute",
        sense = c("=", ">=", "<="),
        target = c(5, 10, 20))
      ) %>%
   add_cbc_solver(gap = 0, verbose = FALSE)
  # solve problems
  s1 <- solve(p)
  s2 <- solve(p)
  # test for correct solution
  expect_equal(c(terra::values(s1)), c(1, 0, 1, 0, NA))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("start_solution", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  skip_if_not(
    any(grepl(
      "initial_solution", deparse1(args(rcbc::cbc_solve)), fixed = TRUE)),
    message = "newer version of rcbc R package required"
  )
  # create data
  cost <- terra::rast(matrix(c(1000, 100, 200, 300, NA), nrow = 1))
  features <- c(
    terra::rast(matrix(c(5,  5,   0,  0,  NA), nrow = 1)),
    terra::rast(matrix(c(2,  0,   8,  10, NA), nrow = 1)),
    terra::rast(matrix(c(10, 100, 10, 10, NA), nrow = 1))
  )
  terra::set.names(features, make.unique(names(features)))
  start_valid <- terra::rast(matrix(c(1, 0, 1, 0, NA), nrow = 1))
  start_invalid <- terra::rast(matrix(c(0, 0, 0, 0, NA), nrow = 1))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_manual_targets(
    tibble::tibble(
      feature = names(features),
      type = "absolute",
      sense = c("=", ">=", "<="),
      target = c(5, 10, 20))
    ) %>%
    add_binary_decisions()
  # create solution
  s1 <-
    p %>%
    add_cbc_solver(gap = 0, verbose = FALSE) %>%
    solve()
  s2 <-
    p %>%
    add_cbc_solver(gap = 0, verbose = FALSE, start_solution = start_valid) %>%
    solve()
  s3 <-
    p %>%
    add_cbc_solver(gap = 0, verbose = FALSE, start_solution = start_invalid) %>%
    solve()
  # test for correct solution
  expect_equal(c(terra::values(s1)), c(1, 0, 1, 0, NA))
  expect_equal(terra::values(s1), terra::values(s2))
  expect_equal(terra::values(s1), terra::values(s3))
})

test_that("correct solution (last rij value = 0)", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # create data
  cost <- terra::rast(matrix(c(1000, 100, 200, 300, 1), nrow = 1))
  features <- c(
    terra::rast(matrix(c(5,  5,   0,  0,  0), nrow = 1)),
    terra::rast(matrix(c(2,  0,   8,  10, 0), nrow = 1)),
    terra::rast(matrix(c(10, 100, 10, 10, 0), nrow = 1))
  )
  terra::set.names(features, make.unique(names(features)))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_manual_targets(
    tibble::tibble(
      feature = names(features),
      type = "absolute",
      sense = c("=", ">=", "<="),
      target = c(5, 10, 20))
    ) %>%
    add_cbc_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # test for correct solution
  expect_equal(c(terra::values(s1)), c(1, 0, 1, 0, 0))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("solver information (single solution)", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_cbc_solver(time_limit = 5, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_true(is.numeric(attr(s, "objective")))
  expect_length(attr(s, "objective"), 1)
  expect_true(is.numeric(attr(s, "runtime")))
  expect_length(attr(s, "runtime"), 1)
  expect_true(is.character(attr(s, "status")))
  expect_length(attr(s, "status"), 1)
  expect_true(is.numeric(attr(s, "gap")))
  expect_length(attr(s, "gap"), 1)
})

test_that("solver information (multiple solutions)", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_shuffle_portfolio(3, remove_duplicates = FALSE) %>%
    add_cbc_solver(time_limit = 5, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_true(is.numeric(attr(s, "objective")))
  expect_length(attr(s, "objective"), 3)
  expect_true(is.numeric(attr(s, "runtime")))
  expect_length(attr(s, "runtime"), 3)
  expect_true(is.character(attr(s, "status")))
  expect_length(attr(s, "status"), 3)
  expect_true(is.numeric(attr(s, "gap")))
  expect_length(attr(s, "gap"), 3)
})

test_that("set_start_solution", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  skip_if_not(
    any(grepl(
      "initial_solution", deparse1(args(rcbc::cbc_solve)), fixed = TRUE)),
    message = "newer version of rcbc R package required"
  )
  # create data
  cost <- terra::rast(matrix(c(1000, 100, 200, 300, NA), nrow = 1))
  features <- c(
    terra::rast(matrix(c(5,  5,   0,  0,  NA), nrow = 1)),
    terra::rast(matrix(c(2,  0,   8,  10, NA), nrow = 1)),
    terra::rast(matrix(c(10, 100, 10, 10, NA), nrow = 1))
  )
  terra::set.names(features, make.unique(names(features)))
  start_valid <- terra::rast(matrix(c(1, 0, 1, 0, NA), nrow = 1))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_manual_targets(
    tibble::tibble(
      feature = names(features),
      type = "absolute",
      sense = c("=", ">=", "<="),
      target = c(5, 10, 20))
    ) %>%
    add_binary_decisions() %>%
    add_cbc_solver(verbose = FALSE)
  # force calculations
  p$solver$calculate(compile(p))
  # overwrite start solution
  p$solver$set_start_solution(c(1, 2, 3))
  # tests
  expect_equal(
    p$solver$internal$model$initial_solution,
    c(1, 2, 3, NA)
    )
})

test_that("set_constraint_rhs", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # create data
  cost <- terra::rast(matrix(c(1000, 100, 200, 300, NA), nrow = 1))
  features <- c(
    terra::rast(matrix(c(5,  5,   0,  0,  NA), nrow = 1)),
    terra::rast(matrix(c(2,  0,   8,  10, NA), nrow = 1)),
    terra::rast(matrix(c(10, 100, 10, 10, NA), nrow = 1)),
    terra::rast(matrix(c(10, 100, 10, 10, NA), nrow = 1))
  )
  terra::set.names(features, make.unique(names(features)))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_manual_targets(
      tibble::tibble(
        feature = names(features),
        type = "absolute",
        sense = c("=", ">=", "<=", ">="),
        target = c(5, 10, 20, 18)
      )
    ) %>%
    add_binary_decisions() %>%
    add_cbc_solver(verbose = FALSE)
  # force calculations
  p$solver$calculate(compile(p))
  # overwrite problem
  p$solver$set_constraint_rhs(c(1, 3, 4), c(100, 200, 800))
  # tests
  expect_equal(
    p$solver$internal$model$row_lb,
    c(100, 10, -Inf, 800)
  )
  expect_equal(
    p$solver$internal$model$row_ub,
    c(100, Inf, 200, Inf)
  )
})

test_that("set_variable_lb", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # create data
  cost <- terra::rast(matrix(c(1000, 100, 200, 300, NA), nrow = 1))
  features <- c(
    terra::rast(matrix(c(5,  5,   0,  0,  NA), nrow = 1)),
    terra::rast(matrix(c(2,  0,   8,  10, NA), nrow = 1)),
    terra::rast(matrix(c(10, 100, 10, 10, NA), nrow = 1))
  )
  terra::set.names(features, make.unique(names(features)))
  locked_in <- terra::rast(matrix(c(1, 0, 0, 0, NA), nrow = 1))
  locked_out <- terra::rast(matrix(c(0, 1, 0, 0, NA), nrow = 1))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_manual_targets(
    tibble::tibble(
      feature = names(features),
      type = "absolute",
      sense = c("=", ">=", "<="),
      target = c(5, 10, 20))
    ) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_cbc_solver(verbose = FALSE)
  # force calculations
  p$solver$calculate(compile(p))
  # overwrite problem
  p$solver$set_variable_lb(c(3, 4), c(1, 0))
  # tests
  expect_equal(
    p$solver$internal$model$col_lb,
    c(1, 0, 1, 0)
  )
  expect_equal(
    p$solver$internal$model$col_ub,
    c(1, 0, 1, 1)
  )
})

test_that("set_variable_ub", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # create data
  cost <- terra::rast(matrix(c(1000, 100, 200, 300, NA), nrow = 1))
  features <- c(
    terra::rast(matrix(c(5,  5,   0,  0,  NA), nrow = 1)),
    terra::rast(matrix(c(2,  0,   8,  10, NA), nrow = 1)),
    terra::rast(matrix(c(10, 100, 10, 10, NA), nrow = 1))
  )
  terra::set.names(features, make.unique(names(features)))
  locked_in <- terra::rast(matrix(c(1, 0, 0, 0, NA), nrow = 1))
  locked_out <- terra::rast(matrix(c(0, 1, 0, 0, NA), nrow = 1))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_manual_targets(
    tibble::tibble(
      feature = names(features),
      type = "absolute",
      sense = c("=", ">=", "<="),
      target = c(5, 10, 20))
    ) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_cbc_solver(verbose = FALSE)
  # force calculations
  p$solver$calculate(compile(p))
  # overwrite problem
  p$solver$set_variable_ub(c(3, 4), c(1, 0))
  # tests
  expect_equal(
    p$solver$internal$model$col_lb,
    c(1, 0, 0, 0)
  )
  expect_equal(
    p$solver$internal$model$col_ub,
    c(1, 0, 1, 0)
  )
})

test_that("control and backwards-compatibility presolve", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_cbc_solver(
      time_limit = 5, verbose = TRUE,
      presolve = TRUE,
      control = list(combineSolutions = "on")
  )
  # solve problems
  s1 <- solve(p1)
  # tests
  expect_inherits(s1, "SpatRaster")
  expect_equal(terra::nlyr(s1), 1)
  expect_true(all_binary(s1))
  expect_true(is_comparable_raster(sim_pu_raster, s1))
})
