test_that("compile (single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_cost_constraints(5, ">=")
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), c(p2$planning_unit_costs())))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 5))
  expect_equal(o2$sense(), c(o1$sense(), ">="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("solve (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- terra::rast(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7))
  spp <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7))
  )
  names(spp) <- paste("F", seq_len(terra::nlyr(spp)))
  # create problem
  p <-
    problem(costs, spp) %>%
    add_max_wtd_sum_objective(budget = 1000) %>%
    add_cost_constraints(1.5, "<=") %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, run_checks = FALSE)
  # tests
  expect_inherits(s, "SpatRaster")
  expect_equal(c(terra::values(s)), c(1, 0, NA, 0, 0, 0, NA))
})

test_that("compile (single budget, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create targets data
  targ <- matrix(
    runif(
      number_of_features(sim_zones_features) *
      number_of_zones(sim_zones_features)
    ) * 10,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  # create problems
  p1 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_cost_constraints(5, ">=")
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), c(p1$planning_unit_costs())))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 5))
  expect_equal(o2$sense(), c(o1$sense(), ">="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("solve (single budget, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- c(
    terra::rast(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7)),
    terra::rast(matrix(c(10, 10, 10, 10,  4,   1, NA), ncol = 7))
  )
  spp <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7))
  )
  # create problem
  p <-
    problem(costs, zones(spp[[1:2]], spp[[3:4]])) %>%
    add_max_wtd_sum_objective(budget = 1000) %>%
    add_cost_constraints(1, "<=") %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, run_checks = FALSE)
  # tests
  expect_inherits(s, "SpatRaster")
  expect_equal(c(terra::values(s[[1]])), c(0, 0, NA, 0, 0, 0, NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 0, 0,  0, 0, 1, NA))
})

test_that("compile (multiple budgets, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create targets data
  targ <- matrix(
    runif(
      number_of_features(sim_zones_features) *
      number_of_zones(sim_zones_features)
    ) * 10,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  # create problems
  p1 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_cost_constraints(c(5, 10, 12), c(">=", "<=", "="))
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  extra_A <- matrix(0, nrow = 3, ncol = length(o1$obj()))
  costs <- p1$planning_unit_costs()
  extra_A[1, ] <- c(costs[, 1], costs[, 2] * 0, costs[, 3] * 0)
  extra_A[2, ] <- c(costs[, 1] * 0, costs[, 2], costs[, 3] * 0)
  extra_A[3, ] <- c(costs[, 1] * 0, costs[, 2] * 0, costs[, 3])
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), extra_A))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), c(5, 10, 12)))
  expect_equal(o2$sense(), c(o1$sense(), c(">=", "<=", "=")))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("invalid inputs", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # tests
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_cost_constraints(NA_real_, "<=")
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_cost_constraints("a", "<=")
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_cost_constraints(c(3, 4), "<=")
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_cost_constraints(3, "a")
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_cost_constraints(3, 5)
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_cost_constraints(3, c("<=", "<="))
  })
})
