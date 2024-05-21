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
    add_default_portfolio() %>%
    add_default_solver(gap = 0.2, verbose = FALSE)
  # compile problem
  o <- compile(p)
  # tests
  expect_inherits(o, "OptimizationProblem")
})

test_that("solve (single zone)", {
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
    add_default_portfolio() %>%
    add_default_solver(gap = 0.2, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_inherits(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 1)
  expect_true(
    all(
      terra::global(s * features, "sum", na.rm = TRUE)[[1]] >= c(2, 10)
    )
  )
})

test_that("solve (multiple zones)", {
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
    add_default_portfolio() %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0.2, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_inherits(s, "SpatRaster")
  expect_equal(terra::nlyr(s), number_of_zones(sim_zones_features))
  expect_true(
    all(
      terra::global(
        s[[2]] * sim_zones_features[[2]],
        "sum", na.rm = TRUE
      )[[1]] >= 1
    )
  )
})
