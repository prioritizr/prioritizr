test_that("assert_pass_presolve_check (OptimizationProblem, TRUE)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # compile problem
  o <- compile(p)
  # run tests
  expect_silent(expect_true(assert_pass_presolve_check(o)))
})

test_that("assert_pass_presolve_check (OptimizationProblem, error)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update data
  sim_features[[1]][1] <- 1e+15
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(1) %>%
    add_binary_decisions()
  # compile problem
  o <- compile(p)
  # run tests
  expect_message(
    expect_error(assert_pass_presolve_check(o)),
    "rij"
  )
})

test_that("verify_pass_presolve_check (OptimizationProblemm, TRUE)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # compile problem
  o <- compile(p)
  # run tests
  expect_silent(expect_true(verify_pass_presolve_check(o)))
})

test_that("verify_pass_presolve_check (OptimizationProblem, error)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update data
  sim_features[[1]][1] <- 1e+15
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(1) %>%
    add_binary_decisions()
  # compile problem
  o <- compile(p)
  # run tests
  expect_message(
    expect_warning(verify_pass_presolve_check(o)),
    "rij"
  )
})

test_that("assert_pass_presolve_check (list, TRUE)", {
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
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions()
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  x <- list(o1, o2)
  # run tests
  expect_silent(expect_true(assert_pass_presolve_check(x)))
})

test_that("assert_pass_presolve_check (list, error)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # introduce infeasibility in second problem
  sim_features_bad <- sim_features
  sim_features_bad[[1]][1] <- 1e+15
  # create problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_pu_raster, sim_features_bad) %>%
    add_min_set_objective() %>%
    add_absolute_targets(1) %>%
    add_binary_decisions()
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  x <- list(o1, o2)
  # run tests
  expect_message(
    expect_error(assert_pass_presolve_check(x)),
    "rij"
  )
})

test_that("verify_pass_presolve_check (list, TRUE)", {
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
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions()
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  x <- list(o1, o2)
  # run tests
  expect_silent(expect_true(verify_pass_presolve_check(x)))
})

test_that("verify_pass_presolve_check (list, warning)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # introduce infeasibility
  sim_features_bad <- sim_features
  sim_features_bad[[1]][1] <- 1e+15
  # create problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_pu_raster, sim_features_bad) %>%
    add_min_set_objective() %>%
    add_absolute_targets(1) %>%
    add_binary_decisions()
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  x <- list(o1, o2)
  # run tests
  expect_message(
    expect_warning(
      verify_pass_presolve_check(x)
    ),
    "rij"
  )
})
