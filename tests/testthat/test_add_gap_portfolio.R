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
    s <- solve_fixed_seed(p),
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
  s <- solve_fixed_seed(p)
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
