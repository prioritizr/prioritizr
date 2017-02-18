context('decisions') 

test_that('add_binary_decision', {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision()  
  o <- compile(p)
  # check that decision variables are correctly applied
  n_pu <- length(raster::Which(!is.na(sim_pu_raster), cells=TRUE))
  expect_equal(o$lb(), rep(0, n_pu))
  expect_equal(o$ub(), rep(1, n_pu))
  expect_equal(o$vtype(), rep('B', n_pu))
  # check that solutions have correct decisions
  s <- solve(p)
  expect_true(all(values(s) %in% c(0L, 1L, NA_integer_)))
})

test_that('add_proportion_decision', {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decision()  
  o <- compile(p)
  # check that decision variables are correctly applied
  n_pu <- length(raster::Which(!is.na(sim_pu_raster), cells=TRUE))
  expect_equal(o$lb(), rep(0, n_pu))
  expect_equal(o$ub(), rep(1, n_pu))
  expect_equal(o$vtype(), rep('S', n_pu))
  # check that solutions have correct decisions
  s <- solve(p)
  expect_true(isTRUE(all(na.omit(values(s)) <= 1)))
  expect_true(isTRUE(all(na.omit(values(s)) >= 0)))
})

test_that('add_semicontinuous_decision', {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_semicontinuous_decision(0.3)  
  o <- compile(p)
  # check that decision variables are correctly applied
  n_pu <- length(raster::Which(!is.na(sim_pu_raster), cells=TRUE))
  expect_equal(o$lb(), rep(0, n_pu))
  expect_equal(o$ub(), rep(0.3, n_pu))
  expect_equal(o$vtype(), rep('S', n_pu))
  # check that invalid inputs result in an error
  expect_error(p %>% add_semicontinuous_decision(NA))
  expect_error(p %>% add_semicontinuous_decision(Inf))
  expect_error(p %>% add_semicontinuous_decision(c()))
  expect_error(p %>% add_semicontinuous_decision(c(1, 3)))
  # check that solutions have correct decisions
  s <- solve(p)
  expect_true(isTRUE(all(na.omit(values(s)) <= 0.3)))
  expect_true(isTRUE(all(na.omit(values(s)) >= 0)))  
})
