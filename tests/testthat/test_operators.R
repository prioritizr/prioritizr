context('operators') 

test_that('ConservationProblem', {
  expect_is(problem(sim_pu_raster, sim_features) + maximum_coverage_objective(5), 'ConservationProblem')
  expect_is(problem(sim_pu_raster, sim_features) + relative_targets(0.2), 'ConservationProblem')
  expect_is(problem(sim_pu_raster, sim_features) + connected_constraints(), 'ConservationProblem')
  expect_is(problem(sim_pu_raster, sim_features) + binary_decision(), 'ConservationProblem')
  expect_error(problem(sim_pu_raster, sim_features) + problem(sim_pu_raster, sim_features))
})

test_that('Objective', {
  expect_error(minimum_set_objective() + maximum_coverage_objective(5))
  expect_error(minimum_set_objective() + relative_targets(0.2))
  expect_error(minimum_set_objective() + fragmentation_constraints(5, 0.5))
  expect_error(minimum_set_objective() + binary_decision())
  expect_error(minimum_set_objective() + problem(sim_pu_raster, sim_features))
})


test_that('Target', {
  expect_error(relative_targets(0.2) + absolute_targets(5))
  expect_error(relative_targets(0.2) + minimum_set_objective())
  expect_error(relative_targets(0.2) + fragmentation_constraints(5, 0.5))
  expect_error(relative_targets(0.2) + binary_decision())
  expect_error(relative_targets(0.2) + problem(sim_pu_raster, sim_features))
})

test_that('Decision', {
  expect_error(binary_decision() + proportion_decision())
  expect_error(binary_decision() + absolute_targets(5))
  expect_error(binary_decision() + minimum_set_objective())
  expect_error(binary_decision() + fragmentation_constraints(5, 0.5))
  expect_error(binary_decision() + problem(sim_pu_raster, sim_features))
})

test_that('Constraint', {
  expect_error(connected_contraint() + fragmentation_constraints(5, 0.5))
  expect_error(connected_contraint() + absolute_targets(5))
  expect_error(connected_contraint() + minimum_set_objective())
  expect_error(connected_contraint() + binary_decision())
})
