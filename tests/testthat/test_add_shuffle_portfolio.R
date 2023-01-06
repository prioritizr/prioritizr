context("add_shuffle_portfolio")

test_that("compile", {
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_shuffle_portfolio(2) %>%
       add_default_solver(gap = 0.2, verbose = FALSE)
  # compile problem
  cmp <- compile(p)
  # tests
  expect_is(cmp, "OptimizationProblem")
})

test_that("solve (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  locked_in <- 2
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_locked_in_constraints(locked_in) %>%
       add_shuffle_portfolio(3, remove_duplicates = FALSE) %>%
       add_default_solver(gap = 0.2, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "list")
  expect_equal(length(s), 3)
  expect_true(all(sapply(s, inherits, "RasterLayer")))
  expect_equal(names(s), paste0("solution_", seq_len(3)))
  for (i in seq_along(s))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
})

test_that("solve (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <- problem(sim_zones_pu_raster, sim_zones_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(2,
                            nrow = number_of_features(sim_zones_features),
                            ncol = number_of_zones(sim_zones_features))) %>%
       add_shuffle_portfolio(3, remove_duplicates = FALSE) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0.2, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "list")
  expect_equal(length(s), 3)
  expect_true(all(sapply(s, inherits, "RasterStack")))
  expect_equal(names(s), paste0("solution_", seq_len(3)))
  for (i in seq_along(s))
    for (z in seq_len(number_of_zones(sim_zones_features)))
      expect_true(all(raster::cellStats(s[[i]][[z]] * sim_zones_features[[z]],
                                        "sum") >= 2))
})

test_that("solve (no duplicates)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  set.seed(500)
  cost <- raster::raster(matrix(c(1, 1, 0.5, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 2, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_shuffle_portfolio(5, remove_duplicates = TRUE) %>%
       add_default_solver(gap = 0.8, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "list")
  expect_equal(length(s), 2)
  expect_equal(names(s), paste0("solution_", seq_len(2)))
  expect_true(all(sapply(s, inherits, "RasterLayer")))
  expect_equal(anyDuplicated(apply(raster::as.data.frame(raster::stack(s)), 2,
                                   paste, collapse = " ")), 0)
  expect_equal(names(s), paste0("solution_", seq_len(2)))
  for (i in seq_len(2))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
})

test_that("solve (parallel processing)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  set.seed(500)
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_shuffle_portfolio(3, threads = 2, remove_duplicates = FALSE) %>%
       add_default_solver(gap = 0.2, verbose = FALSE)
  # solve problem
  s <- suppressWarnings(solve(p))
  # output checks
  expect_is(s, "list")
  expect_equal(length(s), 3)
  expect_equal(names(s), paste0("solution_", seq_len(3)))
  expect_true(all(sapply(s, inherits, "RasterLayer")))
  expect_equal(names(s), paste0("solution_", seq_len(3)))
  for (i in seq_along(s))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
})
