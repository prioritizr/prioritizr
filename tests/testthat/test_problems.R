context('problem') 

test_that('raster planning unit data', {
  data(sim_pu_raster, sim_features)
  x <- problem(sim_pu_raster, sim_features)
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(),
    length(raster::Which(!is.na(sim_pu_raster), cells=TRUE)))
  expect_equal(x$costs(), sim_pu_raster[!is.na(sim_pu_raster)])
})

test_that('SpatialPolyonsDataFrame planning unit data', {
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons$cost[1:5] <- NA
  x <- problem(sim_pu_polygons, sim_features, 'cost')
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_polygons$cost)))
  expect_equal(x$costs(), sim_pu_polygons$cost[!is.na(sim_pu_polygons$cost)])
  expect_equal(x$data$rij_matrix, 
    rij_matrix(sim_pu_polygons[!is.na(sim_pu_polygons[[1]]),], sim_features))
  expect_equal(x$feature_abundances_planning_units(),
    Matrix::colSums(x$data$rij_matrix))
})

test_that('SpatialLinesDataFrame planning unit data', {
  data(sim_pu_lines, sim_features)
  sim_pu_lines$cost[1:5] <- NA
  x <- problem(sim_pu_lines, sim_features, 'cost')
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_lines$cost)))
  expect_equal(x$costs(), sim_pu_lines$cost[!is.na(sim_pu_lines$cost)])
  
  
test_that('SpatialLinesDataFrame planning unit data', {
  data(sim_pu_lines, sim_features)
  sim_pu_lines$cost[1:5] <- NA
  x <- problem(sim_pu_lines, sim_features, 'cost')
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_lines$cost)))
  expect_equal(x$costs(), sim_pu_lines$cost[!is.na(sim_pu_lines$cost)])
  expect_equal(x$data$rij_matrix, 
    rij_matrix(sim_pu_lines[!is.na(sim_pu_lines[[1]]),], sim_features))
  expect_equal(x$feature_abundances_planning_units(),
    Matrix::colSums(x$data$rij_matrix))
})
  
test_that('SpatialPointsDataFrame planning unit data', {
  data(sim_pu_points, sim_features)
  sim_pu_points$cost[1:5] <- NA
  x <- problem(sim_pu_points, sim_features, 'cost')
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_points$cost)))
  expect_equal(x$costs(), sim_pu_points$cost[!is.na(sim_pu_points$cost)])
  expect_equal(x$data$rij_matrix, 
    rij_matrix(sim_pu_points[!is.na(sim_pu_points[[1]]),], sim_features))
  expect_equal(x$feature_abundances_planning_units(),
    Matrix::colSums(x$data$rij_matrix))
})

test_that('invalid problem inputs', {
  # check that errors are thrown if cost data is not single-band raster
  data(sim_pu_polygons, sim_pu_lines, sim_pu_points, sim_pu_raster,
    sim_features)
  expect_error(problem(sim_pu_polygons, sim_pu_raster))
  expect_error(problem(sim_pu_lines, sim_pu_raster))
  expect_error(problem(sim_pu_points, sim_pu_raster))
  expect_error(problem(raster::stack(sim_pu_raster, sim_pu_raster),
    sim_features))

  # check that errors are thrown if all planning units have NA cost
  sim_pu_polygons$cost <- NA
  sim_pu_lines$cost <- NA
  sim_pu_points$cost <- NA
  sim_pu_raster <- raster::setValues(sim_pu_raster, NA)
  expect_error(problem(sim_pu_raster, sim_features))
  expect_error(problem(sim_pu_polygons, sim_features))
  expect_error(problem(sim_pu_lines, sim_features))
  expect_error(problem(sim_pu_points, sim_features))
  
  # check that errors are thrown if features only contain NAs
  data(sim_pu_polygons, sim_pu_lines, sim_pu_points, sim_pu_raster)
  sim_features[[1]] <- raster::setValues(sim_features[[1]], NA)
  expect_error(problem(sim_pu_raster, sim_features))
  expect_error(problem(sim_pu_polygons, sim_features))
  expect_error(problem(sim_pu_lines, sim_features))
  expect_error(problem(sim_pu_points, sim_features))

  # check that errors are thrown if features only contain zeros
  data(sim_pu_polygons, sim_pu_lines, sim_pu_points, sim_pu_raster)
  sim_features[[1]] <- raster::setValues(sim_features[[1]], 0)
  expect_error(problem(sim_pu_raster, sim_features))
  expect_error(problem(sim_pu_polygons, sim_features))
  expect_error(problem(sim_pu_lines, sim_features))
  expect_error(problem(sim_pu_points, sim_features))
})

