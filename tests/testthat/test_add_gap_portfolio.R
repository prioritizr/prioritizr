test_that("compile", {
  skip_if_not_installed("gurobi")
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
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
    add_gap_portfolio(number_solutions = 5, pool_gap = 1) %>%
    add_gurobi_solver(gap = 0.2, verbose = FALSE)
  # compile problem
  o <- compile(p)
  # tests
  expect_inherits(o, "OptimizationProblem")
})

test_that("solve (single zone)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  locked_in <- 2
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_gap_portfolio(number_solutions = 5, pool_gap = 0.5) %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  # solve problem
  expect_warning(
    s <- solve(p),
    "Portfolio could only"
  )
  # tests
  expect_inherits(s, "list")
  expect_equal(length(s), 2) # only two solutions meet this gap
  expect_true(all_elements_inherit(s, "SpatRaster"))
  expect_named(s, paste0("solution_", seq_along(s)))
  for (i in seq_along(s))
    expect_true(
      all(
        terra::global(s[[i]] * features, "sum", na.rm = TRUE) >= c(2, 10)
      )
    )
})

test_that("solve (multiple zones)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(
        2, nrow = number_of_features(sim_zones_features),
        ncol = number_of_zones(sim_zones_features)
      )
    ) %>%
    add_gap_portfolio(number_solutions = 5, pool_gap = 1) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_inherits(s, "list")
  expect_length(s, 5)
  expect_true(all_elements_inherit(s, "SpatRaster"))
  expect_named(s, paste0("solution_", seq_along(s)))
  for (i in seq_along(s))
    for (z in seq_len(number_of_zones(sim_zones_features)))
      expect_true(
        all(
          terra::global(
            s[[i]][[z]] * sim_zones_features[[z]], "sum", na.rm = TRUE
          )
          >= 2
        )
      )
})

test_that("solver information (single solution)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_gap_portfolio(1, pool_gap = 5) %>%
    add_gurobi_solver(time_limit = 5, verbose = FALSE)
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
  skip_if_not_installed("gurobi")
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_gap_portfolio(3, pool_gap = 5) %>%
    add_gurobi_solver(time_limit = 5, verbose = FALSE)
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
