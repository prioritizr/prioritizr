context("problem")

test_that("x=Raster, y=RasterStack", {
  data(sim_pu_raster, sim_features)
  x <- problem(sim_pu_raster, sim_features)
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(),
    length(raster::Which(!is.na(sim_pu_raster), cells = TRUE)))
  expect_equal(x$planning_unit_costs(),
               matrix(sim_pu_raster[[1]][!is.na(sim_pu_raster)], ncol = 1))
  expect_equivalent(x$feature_abundances_in_planning_units(),
    matrix(raster::cellStats(raster::mask(sim_features, sim_pu_raster),
                             "sum"), ncol = 1))
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_raster, sim_features)))
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_raster, sim_pu_raster)$number_of_features(), 1L)
})

test_that("x=RasterStack, y=ZonesRaster", {
  data(sim_pu_zones_stack, sim_features_zones)
  x <- problem(sim_pu_zones_stack, sim_features_zones)
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features_zones[[1]]))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features_zones[[1]]))
  expect_equal(x$number_of_planning_units(),
               raster::cellStats(max(!is.na(sim_pu_zones_stack)), "sum"))
  expect_equal(x$planning_unit_costs(),
               sim_pu_zones_stack[raster::Which(
                 max(!is.na(sim_pu_zones_stack)) == 1)])
  expect_equivalent(x$feature_abundances_in_planning_units(),
    sapply(seq_len(raster::nlayers(sim_pu_zones_stack)), function(i) {
      raster::cellStats(raster::mask(sim_features_zones[[i]],
                                     sim_pu_zones_stack[[i]]), "sum")
  }))
  expect_equivalent(x$data$rij_matrix,
                    lapply(seq_len(raster::nlayers(sim_pu_zones_stack)),
                          function(i) rij_matrix(sim_pu_zones_stack[[i]],
                                                 sim_features_zones[[i]])))
  expect_error(x$feature_targets())
})

test_that("x=SpatialPolygonsDataFrame, y=RasterStack", {
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons$cost[1:5] <- NA
  x <- problem(sim_pu_polygons, sim_features, "cost")
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_polygons$cost)))
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_polygons$cost[!is.na(sim_pu_polygons$cost)],
                           ncol = 1))
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_polygons[
                      !is.na(sim_pu_polygons[[1]]), ], sim_features)))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    Matrix::rowSums(x$data$rij_matrix[[1]]))
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_polygons, sim_pu_raster,
                       "cost")$number_of_features(),
               1L)
})

test_that("x=SpatialPolygonsDataFrame, y=ZonesRaster", {
  data(sim_pu_zones_polygons, sim_features_zones)
  sim_pu_zones_polygons[5, paste0("cost_", 1:3)] <- NA
  x <- problem(sim_pu_zones_polygons, sim_features_zones, paste0("cost_", 1:3))
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features_zones[[1]]))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features_zones[[1]]))
  expect_equal(x$number_of_planning_units(), nrow(sim_pu_zones_polygons) - 1)
  expect_equal(x$planning_unit_costs(),
              as.matrix(sim_pu_zones_polygons@data[-5, paste0("cost_", 1:3)]))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    sapply(x$data$rij_matrix, Matrix::rowSums))
  expect_equivalent(x$data$rij_matrix,
                    lapply(sim_features_zones, rij_matrix,
                           x = sim_pu_zones_polygons[-5, ]))
  expect_error(x$feature_targets())
})

test_that("x=SpatialLinesDataFrame, y=RasterStack", {
  data(sim_pu_lines, sim_features)
  sim_pu_lines$cost[1:5] <- NA
  x <- problem(sim_pu_lines, sim_features, "cost")
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_lines$cost)))
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_lines$cost[!is.na(sim_pu_lines$cost)],
                           ncol = 1))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(Matrix::rowSums(x$data$rij_matrix[[1]]), ncol = 1))
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_lines[!is.na(sim_pu_lines[[1]]), ],
                                    sim_features)))
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_lines, sim_pu_raster,
                       "cost")$number_of_features(), 1L)
})

test_that("x=SpatialPointsDataFrame, y=RasterStack", {
  data(sim_pu_points, sim_features)
  sim_pu_points$cost[1:5] <- NA
  x <- problem(sim_pu_points, sim_features, "cost")
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_points$cost)))
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_points$cost[!is.na(sim_pu_points$cost)],
                           ncol = 1))
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_points[!is.na(sim_pu_points[[1]]), ],
                                    sim_features)))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(Matrix::rowSums(x$data$rij_matrix[[1]]), ncol = 1))
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_points, sim_pu_raster,
                       "cost")$number_of_features(), 1L)
})

test_that("x=SpatialPolygonsDataFrame, y=character", {
  # simulate data
  data(sim_pu_polygons)
  sim_pu_polygons$spp1 <- runif(length(sim_pu_polygons))
  sim_pu_polygons$spp2 <- c(NA, rpois(length(sim_pu_polygons) -1, 5))
  # create problem
  x <- problem(sim_pu_polygons, c("spp1", "spp2"), "cost")
  print(x)
  x
  # run tests
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_planning_units(), length(sim_pu_polygons))
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_polygons$cost, ncol = 1))
  rij <- Matrix::sparseMatrix(i = c(rep(1, length(sim_pu_polygons)),
                                    rep(2, length(sim_pu_polygons) -1)),
                              j = c(seq_len(length(sim_pu_polygons)),
                                    seq_len(length(sim_pu_polygons))[-1]),
                              x = c(sim_pu_polygons$spp1,
                                    sim_pu_polygons$spp2[-1]),
                              dims = c(2, length(sim_pu_polygons)))
  rij <- list(rij)
  expect_true(all(x$data$rij_matrix[[1]] == rij[[1]]))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(colSums(sim_pu_polygons@data[, c("spp1", "spp2")],
                                   na.rm = TRUE), ncol = 1))
  expect_error(x$feature_targets())
})

test_that("x=SpatialPolygonsDataFrame, y=ZonesCharacter", {
  # simulate data
  data(sim_pu_zones_polygons)
  sim_pu_zones_polygons$spp1_1 <- runif(length(sim_pu_zones_polygons))
  sim_pu_zones_polygons$spp2_1 <- c(NA, rpois(length(sim_pu_zones_polygons) - 1,
                                    5))
  sim_pu_zones_polygons$spp1_2 <- runif(length(sim_pu_zones_polygons))
  sim_pu_zones_polygons$spp2_2 <- runif(length(sim_pu_zones_polygons))
  # create problem
  x <- problem(sim_pu_zones_polygons,
               zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
               c("cost_1", "cost_2"))
  print(x)
  x
  # run tests
  expect_equal(x$feature_names(), c("spp1_1", "spp2_1"))
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_planning_units(), length(sim_pu_polygons))
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_polygons$cost, ncol = 1))
  rij <- Matrix::sparseMatrix(i = c(rep(1, length(sim_pu_polygons)),
                                    rep(2, length(sim_pu_polygons) - 1)),
                              j = c(seq_len(length(sim_pu_polygons)),
                                    seq_len(length(sim_pu_polygons))[-1]),
                              x = c(sim_pu_polygons$spp1,
                                    sim_pu_polygons$spp2[-1]),
                              dims = c(2, length(sim_pu_polygons)))
  rij <- list(rij)
  expect_true(all(x$data$rij_matrix[[1]] == rij[[1]]))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(colSums(sim_pu_polygons@data[, c("spp1", "spp2")],
                                   na.rm = TRUE), ncol = 1))
  expect_error(x$feature_targets())
})

test_that("x=data.frame, y=data.frame", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = runif(10))
  species <- data.frame(id = seq_len(5), name = letters[1:5], targets = 0.5)
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5))
  rij$amount <- runif(nrow(rij))
  # create problem
  x <- problem(pu, species, rij, "cost")
  print(x)
  x
  # run tests
  expect_equal(x$feature_names(), letters[1:5])
  expect_equal(x$number_of_features(), 5)
  expect_equal(x$number_of_planning_units(), 10)
  expect_equivalent(x$planning_unit_costs(), matrix(pu$cost, ncol = 1))
  expect_equivalent(x$data$rij_matrix[[1]],
                    Matrix::sparseMatrix(i = rij[[2]], j = rij[[1]],
                                         x = rij[[3]],
                                         dims = c(5, 10)))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    Matrix::rowSums(Matrix::sparseMatrix(i = rij[[2]],
                                                         j = rij[[1]],
                                                         x = rij[[3]])))
  expect_error(x$feature_targets())
})

test_that("x=data.frame, y=character", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = runif(10),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  x <- problem(pu, c("spp1", "spp2"), "cost")
  print(x)
  x
  # run tests
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_planning_units(), 10)
  expect_equivalent(x$planning_unit_costs(), matrix(pu$cost, ncol = 1))
  expect_true(all(x$data$rij_matrix[[1]] ==
                  Matrix::sparseMatrix(i = c(rep(1, 10), rep(2, 9)),
                                       j = c(1:10, 1:9),
                                       x = c(pu$spp1, pu$spp2[1:9]),
                                       dims = c(2, 10))))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(colSums(pu[, c("spp1", "spp2")], na.rm = TRUE),
                           ncol = 1))
  expect_error(x$feature_targets())
})

test_that("x=numeric, y=data.frame", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = runif(10),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  x <- problem(pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               as.matrix(t(pu[, 3:4])))
  print(x)
  x
  # run tests
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_planning_units(), 10)
  expect_equal(x$planning_unit_costs(), matrix(pu$cost, ncol = 1))
  expect_true(all(x$data$rij_matrix[[1]] ==
                  Matrix::sparseMatrix(i = c(rep(1, 10), rep(2, 9)),
                                       j = c(1:10, 1:9),
                                       x = c(pu$spp1, pu$spp2[1:9]),
                                       dims = c(2, 10))))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(colSums(pu[, c("spp1", "spp2")], na.rm = TRUE),
                           ncol = 1))
  expect_error(x$feature_targets())
})

test_that("invalid problem inputs", {
  # check that errors are thrown if invalid arguments
  data(sim_pu_polygons, sim_pu_lines, sim_pu_points, sim_pu_raster,
    sim_features)
  expect_error(problem(1, sim_features))
  expect_error(problem(sim_pu_lines, sim_pu_points, "cost"))
  expect_error(problem(raster::stack(sim_pu_raster, sim_pu_raster),
    sim_features))

  # check that errors are thrown if all planning units have NA cost
  sim_pu_polygons$cost <- NA
  sim_pu_lines$cost <- NA
  sim_pu_points$cost <- NA
  suppressWarnings(sim_pu_raster <- raster::setValues(sim_pu_raster, NA))
  expect_error(problem(sim_pu_raster, sim_features))
  expect_error(problem(sim_pu_polygons, sim_features, "cost"))
  expect_error(problem(sim_pu_lines, sim_features, "cost"))
  expect_error(problem(sim_pu_points, sim_features, "cost"))

  # check that errors are thrown if features only contain NAs
  data(sim_pu_polygons, sim_pu_lines, sim_pu_points, sim_pu_raster)
  suppressWarnings(sim_features[[1]] <- raster::setValues(sim_features[[1]],
    NA))
  expect_error(problem(sim_pu_raster, sim_features))
  expect_error(problem(sim_pu_polygons, sim_features, "cost"))
  expect_error(problem(sim_pu_lines, sim_features, "cost"))
  expect_error(problem(sim_pu_points, sim_features, "cost"))

  # check that errors are thrown if features only contain zeros
  data(sim_pu_polygons, sim_pu_lines, sim_pu_points, sim_pu_raster)
  sim_features[[1]] <- raster::setValues(sim_features[[1]], 0)
  expect_error(problem(sim_pu_raster, sim_features))
  expect_error(problem(sim_pu_polygons, sim_features, "cost"))
  expect_error(problem(sim_pu_lines, sim_features, "cost"))
  expect_error(problem(sim_pu_points, sim_features, "cost"))
})

test_that("inheritance", {
  data(sim_pu_raster, sim_features)
  p1 <- problem(sim_pu_polygons, sim_features, "cost")
  p2 <- p1 %>% add_locked_in_constraints("locked_in")
  expect_equal(p1$constraints$length(), 0)
  expect_equal(p2$constraints$length(), 1)
})
