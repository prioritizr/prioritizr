context('problem') 

test_that('raster planning unit data', {
  x <- problem(sim_pu_raster, sim_features)
  print(x)
  x
})

test_that('SpatialPolyons planning unit data', {
  x <- problem(sim_pu_polygons, sim_features, 'cost')
  print(x)
  x
})

test_that('SpatialLines planning unit data', {
  x <- problem(sim_pu_lines, sim_features, 'cost')
  print(x)
  x
})

test_that('SpatialLines planning unit data', {
  x <- problem(sim_pu_points, sim_features, 'cost')
  print(x)
  x
})

test_that('invalid problem inputs', {
  expect_error(sim_pu_raster, sim_pu_polygons)
  expect_error(sim_pu_raster, sim_pu_lines)
  expect_error(sim_pu_raster, sim_pu_points)
  expect_error(raster::stack(sim_pu_raster, sim_pu_raster),
               sim_features)
})

