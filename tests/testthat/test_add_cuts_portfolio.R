context("add_cuts_portfolio")

test_that("compile", {
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
    add_cuts_portfolio(2) %>%
    add_default_solver(gap = 0.2, verbose = FALSE)
  # compile problem
  o <- compile(p)
  # tests
  expect_is(o, "OptimizationProblem")
})

test_that("solve (number_solutions within limit, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- c(
    terra::rast(matrix(c(2, 2, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_cuts_portfolio(2) %>%
    add_default_solver(gap = 0.2, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_is(s, "list")
  expect_length(s, 2)
  expect_named(s, c("solution_1", "solution_2"))
  for (i in seq_len(length(s)))
    expect_true(
      all(
        terra::global(s[[i]] * features, "sum", na.rm = TRUE)[[1]] >= c(2, 10)
      )
    )
})

test_that("solve (number_solutions within limit, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create targets
  targets <- matrix(
    0, nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 0
  targets[, 2] <- 1
  targets[, 3] <- 0
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_cuts_portfolio(2) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0.2, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_is(s, "list")
  expect_length(s, 2)
  expect_true(all_elements_inherit(s, "SpatRaster"))
  expect_named(s, paste0("solution_", seq_len(2)))
  for (i in seq_along(s))
    expect_true(
      all(
        terra::global(
          s[[i]][[2]] * sim_zones_features[[i]], "sum",
          na.rm = TRUE)[[1]]
        >= 1
      )
    )
  expect_true(
    lapply(s, category_layer) %>%
      lapply(terra::values) %>%
      lapply(c) %>%
      lapply(paste, collapse = ",") %>%
      no_duplicates()
  )
})

test_that("solve (number_solutions outside limit)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  locked_in <- 2
  locked_out <- 1
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_cuts_portfolio(100) %>%
    add_default_solver(gap = 0.2, verbose = FALSE)
  # solve problem
  expect_warning(
    s <- solve(p),
    "Only found"
  )
  # output checks
  expect_is(s, "SpatRaster")
  expect_true(
    all(terra::global(s * features, "sum", na.rm = TRUE)[[1]] >= c(2, 10))
  )
})
