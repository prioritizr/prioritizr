context("add_default_solver")

test_that("implicit default solver", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create and solve problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions()
  s <- solve(p)
  # tests
  expect_true(inherits(s, "SpatRaster"))
  expect_equal(terra::nlyr(s), 1)
  expect_true(is_comparable_raster(sim_pu_raster, s))
})
