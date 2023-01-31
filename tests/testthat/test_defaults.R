context("default components")

test_that("add_default_targets", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # tests
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_default_targets(),
    "targets"
  )
})

test_that("add_default_objective", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # tests
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_default_objective(),
    "missing an objective"
  )
})

test_that("add_default_portfolio", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_default_portfolio()
  # tests
  expect_is(p, "ConservationProblem")
})

test_that("add_default_decisions", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_default_solver()
  # tests
  expect_is(p, "ConservationProblem")
})
