test_that("min set objective (approx = FALSE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  gap <- 0.02
  # create minimal problem
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = gap, verbose = FALSE)
  # create problem with boundary penalties
  p2 <-
    p1 %>%
    add_boundary_penalties(penalty = 1)
  # calculate result
  x <- calibrate_cohon_method(p2, verbose = FALSE, approx = FALSE)
  # calculate correct result
  s1 <- solve(p1)
  s2 <- suppressWarnings(
    p2 %>%
    add_min_penalties_objective(1e6) %>%
    add_default_solver(gap = gap, verbose = FALSE, start_solution = s1) %>%
    solve(run_checks = FALSE)
  )
  s1_metrics <- data.frame(
    total_cost = eval_cost_summary(p2, s1)$cost,
    total_boundary_length = eval_boundary_summary(p2, s1)$boundary
  )
  s2_metrics <- data.frame(
    total_cost = eval_cost_summary(p2, s2)$cost,
    total_boundary_length = eval_boundary_summary(p2, s2)$boundary
  )
  y <-
    abs(s1_metrics$total_cost - s2_metrics$total_cost) /
    abs(s1_metrics$total_boundary_length - s2_metrics$total_boundary_length)
  # tests
  expect_true(assertthat::is.number(x))
  expect_true(assertthat::noNA(x))
  expect_true(assertthat::is.number(attr(x, "solution_1_objective")))
  expect_true(assertthat::noNA(attr(x, "solution_1_objective")))
  expect_true(assertthat::is.number(attr(x, "solution_2_objective")))
  expect_true(assertthat::noNA(attr(x, "solution_2_objective")))
  expect_true(assertthat::is.number(attr(x, "solution_1_penalty")))
  expect_true(assertthat::noNA(attr(x, "solution_1_penalty")))
  expect_true(assertthat::is.number(attr(x, "solution_2_penalty")))
  expect_true(assertthat::noNA(attr(x, "solution_2_penalty")))
  expect_lte(
    round(attr(x, "solution_1_objective"), 5),
    round(s1_metrics$total_cost, 5)
  )
  expect_lte(
    round(attr(x, "solution_1_penalty"), 5),
    round(s1_metrics$total_boundary_length, 5)
  )
  expect_lte(
    round(attr(x, "solution_2_objective"), 5),
    round(s2_metrics$total_cost, 5)
  )
  expect_lte(
    round(attr(x, "solution_2_penalty"), 5),
    round(s2_metrics$total_boundary_length, 5)
  )
  expect_equal(x[[1]], y, tolerance = 300)
})

test_that("min set objective (approx = TRUE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  gap <- 0.02
  # create minimal problem
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_boundary_penalties(penalty = 1) %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = gap, verbose = FALSE)
  # calculate results
  x <- calibrate_cohon_method(p1, verbose = FALSE, approx = FALSE)
  y <- suppressMessages(
    calibrate_cohon_method(p1, verbose = TRUE, approx = TRUE)
  )
  # tests
  expect_true(assertthat::is.number(x))
  expect_true(assertthat::noNA(x))
  expect_true(assertthat::is.number(attr(x, "solution_1_objective")))
  expect_true(assertthat::noNA(attr(x, "solution_1_objective")))
  expect_true(assertthat::is.number(attr(x, "solution_2_objective")))
  expect_true(assertthat::noNA(attr(x, "solution_2_objective")))
  expect_true(assertthat::is.number(attr(x, "solution_1_penalty")))
  expect_true(assertthat::noNA(attr(x, "solution_1_penalty")))
  expect_true(assertthat::is.number(attr(x, "solution_2_penalty")))
  expect_true(assertthat::noNA(attr(x, "solution_2_penalty")))
  expect_true(assertthat::is.number(attr(y, "solution_1_objective")))
  expect_true(assertthat::noNA(attr(y, "solution_1_objective")))
  expect_true(assertthat::is.number(attr(y, "solution_2_objective")))
  expect_true(assertthat::noNA(attr(y, "solution_2_objective")))
  expect_true(assertthat::is.number(attr(y, "solution_1_penalty")))
  expect_true(assertthat::noNA(attr(y, "solution_1_penalty")))
  expect_true(assertthat::is.number(attr(y, "solution_2_penalty")))
  expect_true(assertthat::noNA(attr(y, "solution_2_penalty")))
  expect_lte(
    abs(attr(x, "solution_1_objective") -  attr(y, "solution_1_objective")),
    400
  )
  expect_lte(
    abs(attr(x, "solution_1_penalty") - attr(y, "solution_1_penalty")),
    2.0
  )
  expect_lte(
    abs(attr(x, "solution_2_objective") - attr(y, "solution_2_objective")),
    400
  )
  expect_lte(
    abs(attr(x, "solution_2_penalty") - attr(y, "solution_2_penalty")),
    0.5
  )
  expect_lte(
    abs(x[[1]] - y[[1]]),
    500
  )
})

test_that("min shortfall objective (approx = FALSE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  gap <- 0.0
  budget <- terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.8
  wts <- runif(terra::nlyr(sim_features)) * 100
  # create minimal problem
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(budget) %>%
    add_feature_weights(wts) %>%
    add_relative_targets(0.8) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = gap, verbose = FALSE)
  # create problem with boundary penalties
  p2 <-
    p1 %>%
    add_boundary_penalties(penalty = 1)
  # calculate result
  x <- calibrate_cohon_method(p2, verbose = FALSE, approx = FALSE)
  # calculate correct result
  s1 <- solve(p1)
  s2 <- s1 * 0
  s1_metrics <- data.frame(
    total_shortfall =
      sum(eval_target_coverage_summary(p2, s1)$relative_shortfall * wts),
    total_boundary_length = eval_boundary_summary(p2, s1)$boundary
  )
  s2_metrics <- data.frame(
    total_shortfall =
      sum(eval_target_coverage_summary(p2, s2)$relative_shortfall * wts),
    total_boundary_length = eval_boundary_summary(p2, s2)$boundary
  )
  y <-
    abs(s1_metrics$total_shortfall - s2_metrics$total_shortfall) /
    abs(s1_metrics$total_boundary_length - s2_metrics$total_boundary_length)
  # tests
  expect_true(assertthat::is.number(x))
  expect_true(assertthat::noNA(x))
  expect_true(assertthat::is.number(attr(x, "solution_1_objective")))
  expect_true(assertthat::noNA(attr(x, "solution_1_objective")))
  expect_true(assertthat::is.number(attr(x, "solution_2_objective")))
  expect_true(assertthat::noNA(attr(x, "solution_2_objective")))
  expect_true(assertthat::is.number(attr(x, "solution_1_penalty")))
  expect_true(assertthat::noNA(attr(x, "solution_1_penalty")))
  expect_true(assertthat::is.number(attr(x, "solution_2_penalty")))
  expect_true(assertthat::noNA(attr(x, "solution_2_penalty")))
  expect_equal(
    attr(x, "solution_1_objective"),
    s1_metrics$total_shortfall,
    tolerance = 1e-5
  )
  expect_equal(
    attr(x, "solution_1_penalty"),
    s1_metrics$total_boundary_length,
    tolerance = 1e-5
  )
  expect_equal(
    attr(x, "solution_2_objective"),
    s2_metrics$total_shortfall,
    tolerance = 1e-5
  )
  expect_equal(
    attr(x, "solution_2_penalty"),
    s2_metrics$total_boundary_length,
    tolerance = 1e-5
  )
  expect_equal(x[[1]], y[[1]], tolerance = 1e-5)
})

test_that("invalid inputs", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # no penalty
  expect_error(
    problem(sim_pu_raster, sim_features) %>%
    calibrate_cohon_method(),
    "single penalty"
  )
  # multiple penalties
  expect_error(
    problem(sim_pu_raster, sim_features) %>%
    add_boundary_penalties(penalty = 1) %>%
    add_boundary_penalties(penalty = 2) %>%
    calibrate_cohon_method(verbose = FALSE),
    "multiple"
  )
  # no trade-offs between objectives
  expect_error(
    problem(sim_pu_raster, sim_features) %>%
    add_max_utility_objective(budget = 0) %>%
    add_boundary_penalties(penalty = 1) %>%
    add_binary_decisions() %>%
    calibrate_cohon_method(verbose = FALSE),
    "trade-offs"
  )
  # failed to compile
  expect_error(
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_boundary_penalties(penalty = 1) %>%
    add_binary_decisions() %>%
    calibrate_cohon_method(verbose = FALSE),
    "compile"
  )
})
