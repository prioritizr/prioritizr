test_that("TODO", {
  stop("No tests")
})

test_that("minimum set objective (obj fun, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problems
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_surrounded_penalties(10000, 1) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  s <- solve(p)
  expect_inherits(s, "SpatRaster")
})
