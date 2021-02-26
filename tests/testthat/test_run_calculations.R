context("run_calculations")

test_that("works", {
  ## make data
  data(sim_pu_raster, sim_features)
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(3, 0.5) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  p2 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(3, 0.5) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  ## run calculations
  run_calculations(p1)
  ## generate solutions
  s1 <- solve(p1)
  s2 <- solve(p2)
  ## compare solutions
  expect_is(s1, "RasterLayer")
  expect_is(s2, "RasterLayer")
  expect_equal(raster::values(s1), raster::values(s2))
})
