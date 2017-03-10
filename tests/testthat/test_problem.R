context("problem")

test_that("raster planning unit data", {
  data(sim_pu_raster, sim_features)
  x <- problem(sim_pu_raster, sim_features)
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(),
    length(raster::Which(!is.na(sim_pu_raster), cells = TRUE)))
  expect_equal(x$planning_unit_costs(),
    sim_pu_raster[[1]][!is.na(sim_pu_raster)])
    expect_equal(x$feature_abundances_in_planning_units(),
    raster::cellStats(raster::mask(sim_features, sim_pu_raster), "sum"))
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_raster, sim_pu_raster)$number_of_features(), 1L)
})

test_that("SpatialPolygonsDataFrame planning unit data", {
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons$cost[1:5] <- NA
  x <- problem(sim_pu_polygons, sim_features, "cost")
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_polygons$cost)))
  expect_equal(x$planning_unit_costs(),
    sim_pu_polygons$cost[!is.na(sim_pu_polygons$cost)])
  expect_equal(x$data$rij_matrix,
    rij_matrix(sim_pu_polygons[!is.na(sim_pu_polygons[[1]]), ], sim_features))
  expect_equal(x$feature_abundances_in_planning_units(),
    Matrix::rowSums(x$data$rij_matrix))
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_polygons, sim_pu_raster)$number_of_features(), 1L)
})

test_that("SpatialLinesDataFrame planning unit data", {
  data(sim_pu_lines, sim_features)
  sim_pu_lines$cost[1:5] <- NA
  x <- problem(sim_pu_lines, sim_features, "cost")
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_lines$cost)))
  expect_equal(x$planning_unit_costs(),
    sim_pu_lines$cost[!is.na(sim_pu_lines$cost)])
  expect_equal(x$feature_abundances_in_planning_units(),
    Matrix::rowSums(x$data$rij_matrix))
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_lines, sim_pu_raster)$number_of_features(), 1L)
})

test_that("SpatialPointsDataFrame planning unit data", {
  data(sim_pu_points, sim_features)
  sim_pu_points$cost[1:5] <- NA
  x <- problem(sim_pu_points, sim_features, "cost")
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_points$cost)))
  expect_equal(x$planning_unit_costs(),
    sim_pu_points$cost[!is.na(sim_pu_points$cost)])
  expect_equal(x$data$rij_matrix,
    rij_matrix(sim_pu_points[!is.na(sim_pu_points[[1]]), ], sim_features))
  expect_equal(x$feature_abundances_in_planning_units(),
    Matrix::rowSums(x$data$rij_matrix))
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_points, sim_pu_raster)$number_of_features(), 1L)
})

test_that("invalid problem inputs", {
  # check that errors are thrown if invalid arguments
  data(sim_pu_polygons, sim_pu_lines, sim_pu_points, sim_pu_raster,
    sim_features)
  expect_error(problem(1, sim_features))
  expect_error(problem(sim_pu_lines, sim_pu_points))
  expect_error(problem(raster::stack(sim_pu_raster, sim_pu_raster),
    sim_features))

  # check that errors are thrown if all planning units have NA cost
  sim_pu_polygons$cost <- NA
  sim_pu_lines$cost <- NA
  sim_pu_points$cost <- NA
  suppressWarnings(sim_pu_raster <- raster::setValues(sim_pu_raster, NA))
  expect_error(problem(sim_pu_raster, sim_features))
  expect_error(problem(sim_pu_polygons, sim_features))
  expect_error(problem(sim_pu_lines, sim_features))
  expect_error(problem(sim_pu_points, sim_features))

  # check that errors are thrown if features only contain NAs
  data(sim_pu_polygons, sim_pu_lines, sim_pu_points, sim_pu_raster)
  suppressWarnings(sim_features[[1]] <- raster::setValues(sim_features[[1]],
    NA))
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


test_that("inheritance", {
  data(sim_pu_raster, sim_features)
  p1 <- problem(sim_pu_polygons, sim_features)
  p2 <- p1 %>% add_locked_in_constraints("locked_in")
  expect_equal(p1$constraints$length(), 0)
  expect_equal(p2$constraints$length(), 1)
})
