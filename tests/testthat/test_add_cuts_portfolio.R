context("add_cuts_portfolio")

test_that("compile", {
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
       add_cuts_portfolio(2) %>%
       add_default_solver(gap = 0.2)
  # compile problem
  cmp <- compile(p)
  # tests
  expect_is(cmp, "OptimizationProblem")
})

test_that("solve (number_solutions within feasible limit, single zone)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 2, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_cuts_portfolio(2) %>%
       add_default_solver(gap = 0.2)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "list")
  expect_length(s, 2)
  expect_equal(names(s), c("solution_1", "solution_2"))
  for (i in seq_len(length(s)))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
})

test_that("solve (number_solutions within feasible limit, multiple zones)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_zones_stack, sim_features_zones)
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(2, nrow = n_feature(sim_features_zones),
                                   ncol = n_zone(sim_features_zones))) %>%
       add_cuts_portfolio(2) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0.2)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "list")
  expect_equal(length(s), 2)
  expect_true(all(sapply(s, inherits, "RasterStack")))
  expect_equal(names(s), paste0("solution_", seq_len(2)))
  for (i in seq_along(s))
    for (z in seq_len(n_zone(sim_features_zones)))
      expect_true(all(raster::cellStats(s[[i]][[z]] * sim_features_zones[[z]],
                                        "sum") >= 2))
})

test_that("solve (number_solutions outside limit)", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  locked_in <- 2
  locked_out <- 1
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_locked_in_constraints(locked_in) %>%
       add_locked_out_constraints(locked_out) %>%
       add_cuts_portfolio(100) %>%
       add_default_solver(gap = 0.5)
  # solve problem
  s <- suppressWarnings(tryCatch(solve(p),
                warning = function(w) return(list(solve(p), w$message))))
  warn <- s[[2]]
  s <- s[[1]]
  # output checks
  expect_is(s, "list")
  expect_length(s, 1)
  expect_equal(names(s), "solution_1")
  for (i in seq_len(length(s)))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
  expect_equal(warn,
               "there are only 1 feasible solutions within the optimality gap")
})
