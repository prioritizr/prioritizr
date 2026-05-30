test_that("two objectives", {
  # define skips
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  pu <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1)))
  ft1 <- terra::rast(matrix(c(5, 0.5, 0, 0, 0, 0)))
  ft2 <- terra::rast(matrix(c(0, 0, 0, 0, 3, 2)))
  # create multi-object problem
  mp <-
    prioritizr::multi_problem(
      obj1 =
        prioritizr::problem(pu, ft1) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions(),
      obj2 =
        prioritizr::problem(pu, ft2) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions()
    ) %>%
    add_ref_point_approach(verbose = FALSE) %>%
    prioritizr::add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(mp, run_checks = FALSE)
  # run tests
  expect_s4_class(s, "SpatRaster")
  expect_equal(
    c(terra::values(s)),
    c(1, 0, 0, 0, 1, 0)
  )
  expect_equal(
    attr(s, "objective")[1, ],
    c(obj1 = 5, obj2 = 3)
  )
})

test_that("three objectives", {
  # define skips
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  pu <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1)))
  ft1 <- terra::rast(matrix(c(5, 0.5, 0, 0, 0, 0)))
  ft2 <- terra::rast(matrix(c(0, 0, 0, 0, 3, 2)))
  ft3 <- terra::rast(matrix(c(0, 0, 10, 1, 0.5, 0)))
  # create multi-object problem
  mp <-
    prioritizr::multi_problem(
      obj1 =
        prioritizr::problem(pu, ft1) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 3) %>%
        prioritizr::add_binary_decisions(),
      obj2 =
        prioritizr::problem(pu, ft2) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 3) %>%
        prioritizr::add_binary_decisions(),
      obj3 =
        prioritizr::problem(pu, ft3) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 3) %>%
        prioritizr::add_binary_decisions()
    ) %>%
    add_ref_point_approach(verbose = FALSE) %>%
    prioritizr::add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(mp, run_checks = FALSE)
  # run tests
  expect_s4_class(s, "SpatRaster")
  expect_equal(
    c(terra::values(s)),
    c(1, 0, 1, 0, 1, 0)
  )
  expect_equal(
    attr(s, "objective")[1, ],
    c(obj1 = 5, obj2 = 3, obj3 = 10.5)
  )
})

test_that("mixed objectives", {
  # define skips
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  pu <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1)))
  ft1 <- terra::rast(matrix(c(5, 0.5, 0, 0, 0, 0)))
  ft2 <- terra::rast(matrix(c(0, 0, 0, 0, 3, 2)))
  # create multi-object problem
  mp <-
    prioritizr::multi_problem(
      obj1 =
        prioritizr::problem(pu, ft1) %>%
        prioritizr::add_min_shortfall_objective(budget = 2) %>%
        prioritizr::add_absolute_targets(5.25) %>%
        prioritizr::add_binary_decisions(),
      obj2 =
        prioritizr::problem(pu, ft2) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions()
    ) %>%
    add_ref_point_approach(verbose = FALSE) %>%
    prioritizr::add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(mp, run_checks = FALSE)
  # run tests
  expect_s4_class(s, "SpatRaster")
  expect_equal(
    c(terra::values(s)),
    c(1, 0, 0, 0, 1, 0)
  )
  expect_equal(
    attr(s, "objective")[1, ],
    c(obj1 = (5.25 - 5) / 5.25, obj2 = 3)
  )
})

test_that("manually specified parameters", {
  # define skips
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  pu <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1)))
  ft1 <- terra::rast(matrix(c(5, 0.5, 0, 0, 0, 0)))
  ft2 <- terra::rast(matrix(c(0, 0, 0, 0, 3, 2)))
  # define parameters
  best <- c(0, 5)
  worst <- c(1, 0)
  wts <- matrix(0, nrow = 3, ncol = 2)
  wts[1, ] <- c(1, 0)
  wts[2, ] <- c(0, 1)
  wts[3, ] <- 1 / c(abs(best - worst))
  # create multi-object problem
  mp <-
    prioritizr::multi_problem(
      obj1 =
        prioritizr::problem(pu, ft1) %>%
        prioritizr::add_min_shortfall_objective(budget = 2) %>%
        prioritizr::add_absolute_targets(5.25) %>%
        prioritizr::add_binary_decisions(),
      obj2 =
        prioritizr::problem(pu, ft2) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions()
    ) %>%
    add_ref_point_approach(
      weights = wts, ref_points = best,
      best = best, worst = worst,
      rescale = FALSE,
      verbose = FALSE
    ) %>%
    prioritizr::add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(mp, run_checks = FALSE)
  # run tests
  expect_type(s, "list")
  expect_length(s, 3)
  expect_s4_class(s[[1]], "SpatRaster")
  expect_s4_class(s[[2]], "SpatRaster")
  expect_s4_class(s[[3]], "SpatRaster")
  expect_equal(
    c(terra::values(s$solution_1)),
    c(1, 1, 0, 0, 0, 0)
  )
  expect_equal(
    c(terra::values(s$solution_2)),
    c(0, 0, 0, 0, 1, 1)
  )
  expect_equal(
    c(terra::values(s$solution_3)),
    c(1, 0, 0, 0, 1, 0)
  )
  expect_equal(
    attr(s, "objective")[1, ],
    c(obj1 = 0, obj2 = 0)
  )
  expect_equal(
    attr(s, "objective")[2, ],
    c(obj1 = 1, obj2 = 5)
  )
  expect_equal(
    attr(s, "objective")[3, ],
    c(obj1 = (5.25 - 5) / 5.25, obj2 = 3)
  )
})

test_that("infeasibility (highs)", {
  skip_on_cran()
  skip_if_not_installed("highs")
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create multi-objective problem with infeasible constraints
  p <-
    multi_problem(
      obj1 =
        problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.99) %>%
        add_linear_constraints(0, "<=", sim_pu_raster) %>%
        add_binary_decisions(),
      obj2 =
        problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.99) %>%
        add_linear_constraints(0, "<=", sim_pu_raster) %>%
        add_binary_decisions()
    ) %>%
    add_highs_solver(verbose = FALSE)
  # test
  ## generating best
  expect_tidy_error(
    p %>%
    add_ref_point_approach(
      weights = c(1, 1), worst = c(1000, 1000), verbose = FALSE
    ) %>%
    solve(),
    "solution"
  )
  ## generating worst
  expect_tidy_error(
    p %>%
    add_ref_point_approach(
      weights = c(1, 1), best = c(0, 0), verbose = FALSE
    ) %>%
    solve(),
    "solution"
  )
  ## generating solution
  expect_tidy_error(
    p %>%
    add_ref_point_approach(
      weights = c(1, 1), best = c(0, 0), worst = c(1000, 1000), verbose = FALSE
    ) %>%
    solve(),
    "solution"
  )
})

test_that("infeasibility (gurobi)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create multi-objective problem with infeasible constraints
  p <-
    multi_problem(
      obj1 =
        problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.99) %>%
        add_linear_constraints(0, "<=", sim_pu_raster) %>%
        add_binary_decisions(),
      obj2 =
        problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.99) %>%
        add_linear_constraints(0, "<=", sim_pu_raster) %>%
        add_binary_decisions()
    ) %>%
    add_gurobi_solver(verbose = FALSE)
  # test
  ## generating best
  expect_tidy_error(
    p %>%
    add_ref_point_approach(
      weights = c(1, 1), worst = c(1000, 1000), verbose = FALSE
    ) %>%
    solve(),
    "solution"
  )
  ## generating worst
  expect_tidy_error(
    p %>%
    add_ref_point_approach(
      weights = c(1, 1), best = c(0, 0), verbose = FALSE
    ) %>%
    solve(),
    "solution"
  )
  ## generating runs
  expect_tidy_error(
    p %>%
    add_ref_point_approach(
      weights = c(1, 1), best = c(0, 0), worst = c(1000, 1000), verbose = FALSE
    ) %>%
    solve(),
    "solution"
  )
})

test_that("invalid inputs", {
  # import data
  pu <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1)))
  ft1 <- terra::rast(matrix(c(5, 0.5, 0, 0, 0, 0)))
  ft2 <- terra::rast(matrix(c(0, 0, 0, 0, 3, 2)))
  # create multi-object problem
  mp <-
    prioritizr::multi_problem(
      obj1 =
        prioritizr::problem(pu, ft1) %>%
        prioritizr::add_min_shortfall_objective(budget = 2) %>%
        prioritizr::add_absolute_targets(5.25) %>%
        prioritizr::add_binary_decisions(),
      obj2 =
        prioritizr::problem(pu, ft2) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions()
    )
  # define parameters
  wts <- matrix(seq_len(6), ncol = 2)
  rp <- matrix(seq_len(6) / 10, ncol = 2)
  b <- c(4, 20)
  w <- c(12, 0)
  # run tests
  ## works
  expect_inherits(
    add_ref_point_approach(mp, wts, rp, b, w, FALSE, TRUE),
    "MultiObjConservationProblem"
  )
  ## weights
  expect_error(
    add_ref_point_approach(mp, wts[-1, ], rp, b, w, FALSE, TRUE),
    "nrow"
  )
  expect_error(
    add_ref_point_approach(mp, wts[, -1], rp, b, w, FALSE, TRUE),
    "length"
  )
  expect_error(
    add_ref_point_approach(mp, wts[, -1, drop = FALSE], rp, b, w, FALSE, TRUE),
    "ncol"
  )
  expect_error(
    add_ref_point_approach(mp, replace(wts, 2, NA), rp, b, w, FALSE, TRUE),
    "missing"
  )
  ## ref_points
  expect_error(
    add_ref_point_approach(mp, wts, rp[-1, ], b, w, FALSE, TRUE),
    "nrow"
  )
  expect_error(
    add_ref_point_approach(mp, wts, rp[, -1], b, w, FALSE, TRUE),
    "length"
  )
  expect_error(
    add_ref_point_approach(mp, wts, rp[, -1, drop = FALSE], b, w, FALSE, TRUE),
    "ncol"
  )
  expect_error(
    add_ref_point_approach(mp, wts, replace(rp, 2, NA), b, w, FALSE, TRUE),
    "missing"
  )
  ## best
  expect_error(
    add_ref_point_approach(mp, wts, rp, b[-1], w, FALSE, TRUE),
    "length"
  )
  expect_error(
    add_ref_point_approach(mp, wts, rp, replace(b, 2, NA), w, FALSE, TRUE),
    "missing"
  )
  ## worst
  expect_error(
    add_ref_point_approach(mp, wts, rp, b, w[-1], FALSE, TRUE),
    "length"
  )
  expect_error(
    add_ref_point_approach(mp, wts, rp, b, replace(w, 2, NA), FALSE, TRUE),
    "missing"
  )
  ## rescale
  expect_error(
    add_ref_point_approach(mp, wts, rp, b, w, "a", TRUE),
    "flag"
  )
  ## verbose
  expect_error(
    add_ref_point_approach(mp, wts, rp, b, w, FALSE, "a"),
    "flag"
  )
})
