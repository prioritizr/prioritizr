context("run_calculations")

test_that("works", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(3, 0.5)
  p2 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(3, 0.5)
  # run calculations for first problem
  run_calculations(p1)
  # compile both problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # tests
  expect_equal(as.list(o1), as.list(o2))
})
