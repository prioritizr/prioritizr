context("default components")

test_that("add_default_targets", {
  expect_error({
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_default_targets()
  })
})

test_that("add_default_objective", {
  expect_error({
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_default_objective()
  })
})

test_that("add_default_portfolio", {
  p <-
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_default_portfolio()
  expect_is(p, "ConservationProblem")
})

test_that("add_default_decisions", {
  p <-
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_default_solver()
  expect_is(p, "ConservationProblem")
})
