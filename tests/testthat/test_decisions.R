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
  expect_equal(o$lb(), rep(0, length(raster::Which(!is.na(sim_pu_raster)))))
  expect_equal(o$ub(), rep(1, length(raster::Which(!is.na(sim_pu_raster)))))
  expect_equal(o$vtype(), 
    rep('B', length(raster::Which(!is.na(sim_pu_raster)))))
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
  expect_equal(o$lb(), rep(0, length(raster::Which(!is.na(sim_pu_raster)))))
  expect_equal(o$ub(), rep(1, length(raster::Which(!is.na(sim_pu_raster)))))
  expect_equal(o$vtype(), 
    rep('S', length(raster::Which(!is.na(sim_pu_raster)))))
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
  expect_equal(o$lb(), rep(0, length(raster::Which(!is.na(sim_pu_raster)))))
  expect_equal(o$ub(), rep(0.3, length(raster::Which(!is.na(sim_pu_raster)))))
  expect_equal(o$vtype(), 
    rep('S', length(raster::Which(!is.na(sim_pu_raster)))))
  # check that invalid inputs result in an error
  expect_error(p %>% add_semicontinuous_decision(NA))
  expect_error(p %>% add_semicontinuous_decision(Inf))
  expect_error(p %>% add_semicontinuous_decision(c()))
  expect_error(p %>% add_semicontinuous_decision(c(1, 3)))
})
