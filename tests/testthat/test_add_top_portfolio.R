context("add_top_portfolio")

test_that("compile", {
  skip_if_not_installed("gurobi")
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_top_portfolio(5) %>%
       add_gurobi_solver(gap = 0.2, verbose = FALSE)
  # compile problem
  cmp <- compile(p)
  # tests
  expect_is(cmp, "OptimizationProblem")
})

test_that("solve (single zone)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
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
       add_top_portfolio(number_solutions = 5) %>%
       add_gurobi_solver(gap = 1, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "list")
  expect_equal(length(s), 3) # only three feasible solutions exist
  expect_true(all(sapply(s, inherits, "RasterLayer")))
  expect_equal(names(s), paste0("solution_", seq_along(s)))
  for (i in seq_along(s))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
})

test_that("solve (multiple zones)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # create data
  data(sim_pu_zones_stack, sim_features_zones)
  # create problem
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(2,
                            nrow = number_of_features(sim_features_zones),
                            ncol = number_of_zones(sim_features_zones))) %>%
       add_top_portfolio(number_solutions = 5) %>%
       add_binary_decisions() %>%
       add_gurobi_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "list")
  expect_equal(length(s), 5)
  expect_true(all(sapply(s, inherits, "RasterStack")))
  expect_equal(names(s), paste0("solution_", seq_along(s)))
  for (i in seq_along(s))
    for (z in seq_len(number_of_zones(sim_features_zones)))
      expect_true(all(raster::cellStats(s[[i]][[z]] * sim_features_zones[[z]],
                                        "sum") >= 2))
})
