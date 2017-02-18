context('solvers') 

test_that('add_default_solver (raster cost data)', {
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision()
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, 'Raster'))
  expect_equal(raster::nlayers(s), 1)
  expect_true(raster::compareRaster(sim_pu_raster, s, res=TRUE, crs=TRUE,
    tolerance=1e-5, stopiffalse=FALSE))
})

test_that('add_default_solver (spatial cost data)', {
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision()
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, 'SpatialPolygonsDataFrame'))
  expect_equal(length(s), length(sim_pu_polygons))
  expect_equal(s@polygons, sim_pu_polygons@polygons)
  expect_true(raster::compareCRS(s, sim_pu_polygons))
})

test_that('add_rsymphony_solver', {
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision() %>% 
    add_rsymphony_solver(gap=0.95)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, 'Raster'))
  expect_equal(raster::nlayers(s), 1)
  expect_true(raster::compareRaster(sim_pu_raster, s, res=TRUE, crs=TRUE,
    tolerance=1e-5, stopiffalse=FALSE))
})

test_that('add_lpsymphony_solver', {
  skip_if_not_installed('lpsymphony')
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision() %>% 
    add_lpsymphony_solver(gap=0.95)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, 'Raster'))
  expect_equal(raster::nlayers(s), 1)
  expect_true(raster::compareRaster(sim_pu_raster, s, res=TRUE, crs=TRUE,
    tolerance=1e-5, stopiffalse=FALSE))
})


test_that('add_gurobi_solver', {
  skip_if_not_installed('gurobi')
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision() %>% 
    add_gurobi_solver(gap=0.95)
  s <- solve(p)
  # check that solution has correct properties
  expect_is(s, 'Raster')
  expect_equal(raster::nlayers(s), 1)
  expect_true(raster::compareRaster(sim_pu_raster, s, res=TRUE, crs=TRUE,
    tolerance=1e-5, stopiffalse=FALSE))
})
