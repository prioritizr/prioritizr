context("problem")

test_that("x=Raster, features=RasterStack", {
  data(sim_pu_raster, sim_features)
  x <- problem(sim_pu_raster, sim_features)
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), names(sim_pu_raster))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(),
    length(raster::Which(!is.na(sim_pu_raster), cells = TRUE)))
  expect_equivalent(x$planning_unit_costs(),
               matrix(sim_pu_raster[[1]][!is.na(sim_pu_raster)], ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), names(sim_pu_raster))
  expect_equivalent(x$feature_abundances_in_planning_units(),
    matrix(raster::cellStats(raster::mask(sim_features, sim_pu_raster),
                             "sum"), ncol = 1))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               x$zone_names())
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               x$feature_names())
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_raster, sim_features)))
  expect_equal(names(x$data$rij_matrix), x$zone_names())
  expect_equal(rownames(x$data$rij_matrix[[1]]), x$feature_names())
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_raster, sim_pu_raster)$number_of_features(), 1L)
})

test_that("x=RasterStack, features=ZonesRaster", {
  data(sim_pu_zones_stack, sim_features_zones)
  x <- problem(sim_pu_zones_stack, sim_features_zones)
  print(x)
  x
  expect_equal(x$feature_names(), feature_names(sim_features_zones))
  expect_equal(x$zone_names(), zone_names(sim_features_zones))
  expect_equal(x$number_of_features(), n_feature(sim_features_zones))
  expect_equal(x$number_of_zones(), n_zone(sim_features_zones))
  expect_equal(x$number_of_planning_units(),
               raster::cellStats(max(!is.na(sim_pu_zones_stack)), "sum"))
  expect_equivalent(x$planning_unit_costs(),
                    sim_pu_zones_stack[raster::Which(
                      max(!is.na(sim_pu_zones_stack)) == 1)])
  expect_equal(colnames(x$planning_unit_costs()),
               zone_names(sim_features_zones))
  expect_equivalent(x$feature_abundances_in_planning_units(),
    sapply(seq_len(raster::nlayers(sim_pu_zones_stack)), function(i) {
      raster::cellStats(raster::mask(sim_features_zones[[i]],
                                     sim_pu_zones_stack[[i]]), "sum")
  }))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
              zone_names(sim_features_zones))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
              feature_names(sim_features_zones))
  expect_equivalent(x$data$rij_matrix,
                    lapply(seq_len(raster::nlayers(sim_pu_zones_stack)),
                          function(i) rij_matrix(sim_pu_zones_stack[[i]],
                                                 sim_features_zones[[i]])))
  expect_equal(names(x$data$rij_matrix), zone_names(sim_features_zones))
  expect_equivalent(sapply(x$data$rij_matrix, rownames),
               matrix(feature_names(sim_features_zones),
                      ncol = n_zone(sim_features_zones),
                      nrow = n_feature(sim_features_zones)))
  expect_error(x$feature_targets())
})

test_that("x=SpatialPolygonsDataFrame, features=RasterStack", {
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons$cost[1:5] <- NA
  x <- problem(sim_pu_polygons, sim_features, "cost")
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_polygons$cost)))
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_polygons$cost[!is.na(sim_pu_polygons$cost)],
                           ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    Matrix::rowSums(x$data$rij_matrix[[1]]))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               "cost")
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               names(sim_features))
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_polygons[
                      !is.na(sim_pu_polygons[[1]]), ], sim_features)))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), names(sim_features))
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_polygons, sim_pu_raster,
                       "cost")$number_of_features(), 1L)
})

test_that("x=SpatialPolygonsDataFrame, features=ZonesRaster", {
  data(sim_pu_zones_polygons, sim_features_zones)
  sim_pu_zones_polygons[5, paste0("cost_", 1:3)] <- NA
  x <- problem(sim_pu_zones_polygons, sim_features_zones, paste0("cost_", 1:3))
  print(x)
  x
  expect_equal(x$feature_names(), feature_names(sim_features_zones))
  expect_equal(x$zone_names(), zone_names(sim_features_zones))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features_zones[[1]]))
  expect_equal(x$number_of_planning_units(), nrow(sim_pu_zones_polygons) - 1)
  expect_equivalent(x$planning_unit_costs(), as.matrix(
    sim_pu_zones_polygons@data[-5, paste0("cost_", 1:3)]))
  expect_equal(colnames(x$planning_unit_costs()),
               zone_names(sim_features_zones))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    sapply(x$data$rij_matrix, Matrix::rowSums))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               zone_names(sim_features_zones))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               feature_names(sim_features_zones))
  expect_equivalent(x$data$rij_matrix,
                    lapply(sim_features_zones, rij_matrix,
                           x = sim_pu_zones_polygons[-5, ]))
  expect_equal(names(x$data$rij_matrix), zone_names(sim_features_zones))
  expect_equivalent(sapply(x$data$rij_matrix, rownames),
               matrix(feature_names(sim_features_zones),
                      ncol = n_zone(sim_features_zones),
                      nrow = n_feature(sim_features_zones)))
  expect_error(x$feature_targets())
})

test_that("x=SpatialLinesDataFrame, features=RasterStack", {
  data(sim_pu_lines, sim_features)
  sim_pu_lines$cost[1:5] <- NA
  x <- problem(sim_pu_lines, sim_features, "cost")
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_lines$cost)))
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_lines$cost[!is.na(sim_pu_lines$cost)],
                           ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(Matrix::rowSums(x$data$rij_matrix[[1]]), ncol = 1))
  expect_equal(colnames(x$feature_abundances_in_planning_units()), "cost")
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               names(sim_features))
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_lines[!is.na(sim_pu_lines[[1]]), ],
                                    sim_features)))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), names(sim_features))
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_lines, sim_pu_raster,
                       "cost")$number_of_features(), 1L)
})

test_that("x=SpatialPointsDataFrame, features=RasterStack", {
  data(sim_pu_points, sim_features)
  sim_pu_points$cost[1:5] <- NA
  x <- problem(sim_pu_points, sim_features, "cost")
  print(x)
  x
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_points$cost)))
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_points$cost[!is.na(sim_pu_points$cost)],
                           ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(Matrix::rowSums(x$data$rij_matrix[[1]]), ncol = 1))
  expect_equal(colnames(x$feature_abundances_in_planning_units()), "cost")
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               names(sim_features))
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_points[!is.na(sim_pu_points[[1]]), ],
                                    sim_features)))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), names(sim_features))
  expect_error(x$feature_targets())
  expect_equal(problem(sim_pu_points, sim_pu_raster,
                       "cost")$number_of_features(), 1L)
})

test_that("x=SpatialPolygonsDataFrame, features=character", {
  # simulate data
  data(sim_pu_polygons)
  sim_pu_polygons$spp1 <- runif(length(sim_pu_polygons))
  sim_pu_polygons$spp2 <- c(NA, rpois(length(sim_pu_polygons) - 1, 5))
  # create problem
  x <- problem(sim_pu_polygons, c("spp1", "spp2"), "cost")
  print(x)
  x
  # run tests
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), length(sim_pu_polygons))
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_polygons$cost, ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  rij <- Matrix::sparseMatrix(i = c(rep(1, length(sim_pu_polygons)),
                                    rep(2, length(sim_pu_polygons) -1)),
                              j = c(seq_len(length(sim_pu_polygons)),
                                    seq_len(length(sim_pu_polygons))[-1]),
                              x = c(sim_pu_polygons$spp1,
                                    sim_pu_polygons$spp2[-1]),
                              dims = c(2, length(sim_pu_polygons)))
  rij <- list(rij)
  expect_true(all(x$data$rij_matrix[[1]] == rij[[1]]))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]),  c("spp1", "spp2"))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(colSums(sim_pu_polygons@data[, c("spp1", "spp2")],
                                   na.rm = TRUE), ncol = 1))
  expect_equal(colnames(x$feature_abundances_in_planning_units()), "cost")
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               c("spp1", "spp2"))
  expect_error(x$feature_targets())
})

test_that("x=SpatialPolygonsDataFrame, features=ZonesCharacter", {
  # simulate data
  data(sim_pu_zones_polygons)
  sim_pu_zones_polygons$spp1_1 <- runif(length(sim_pu_zones_polygons))
  sim_pu_zones_polygons$spp2_1 <- c(NA, rpois(length(sim_pu_zones_polygons) - 1,
                                    5))
  sim_pu_zones_polygons$spp1_2 <- runif(length(sim_pu_zones_polygons))
  sim_pu_zones_polygons$spp2_2 <- runif(length(sim_pu_zones_polygons))
  # create problem
  x <- problem(sim_pu_zones_polygons,
               zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2"),
                     zone_names = c("z1", "z2"),
                     feature_names = c("spp1", "spp2")),
               c("cost_1", "cost_2"))
  print(x)
  x
  # run tests
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$zone_names(), c("z1", "z2"))
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 2)
  expect_equal(x$number_of_planning_units(), length(sim_pu_zones_polygons))
  expect_equivalent(x$planning_unit_costs(),
                    as.matrix(sim_pu_zones_polygons@data[, c("cost_1",
                                                             "cost_2")]))
  expect_equal(colnames(x$planning_unit_costs()), c("z1", "z2"))
  r1 <- Matrix::sparseMatrix(i = c(rep(1, length(sim_pu_zones_polygons)),
                                   rep(2, length(sim_pu_zones_polygons) - 1)),
                             j = c(seq_len(length(sim_pu_zones_polygons)),
                                   seq_len(length(sim_pu_zones_polygons))[-1]),
                             x = c(sim_pu_zones_polygons$spp1_1,
                                   sim_pu_zones_polygons$spp2_1[-1]),
                             dims = c(2, length(sim_pu_zones_polygons)))
  r2 <- Matrix::sparseMatrix(i = c(rep(1, length(sim_pu_zones_polygons)),
                                   rep(2, length(sim_pu_zones_polygons))),
                             j = c(seq_len(length(sim_pu_zones_polygons)),
                                   seq_len(length(sim_pu_zones_polygons))),
                             x = c(sim_pu_zones_polygons$spp1_2,
                                   sim_pu_zones_polygons$spp2_2),
                             dims = c(2, length(sim_pu_zones_polygons)))
  rij <- list(r1, r2)
  expect_equal(names(x$data$rij_matrix), c("z1", "z2"))
  expect_true(all(x$data$rij_matrix[[1]] == rij[[1]]))
  expect_true(all(x$data$rij_matrix[[2]] == rij[[2]]))
  expect_equivalent(x$feature_abundances_in_planning_units(),
    matrix(colSums(sim_pu_zones_polygons@data[,
    c("spp1_1", "spp2_1", "spp1_2", "spp2_2")], na.rm = TRUE), ncol = 2))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               c("z1", "z2"))
  expect_error(x$feature_targets())
})

test_that("x=data.frame, features=character", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = runif(10),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  x <- problem(pu, c("spp1", "spp2"), "cost")
  print(x)
  x
  # run tests
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), 10)
  expect_equivalent(x$planning_unit_costs(), matrix(pu$cost, ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  expect_true(all(x$data$rij_matrix[[1]] ==
                  Matrix::sparseMatrix(i = c(rep(1, 10), rep(2, 9)),
                                       j = c(1:10, 1:9),
                                       x = c(pu$spp1, pu$spp2[1:9]),
                                       dims = c(2, 10))))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("spp1", "spp2"))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(colSums(pu[, c("spp1", "spp2")], na.rm = TRUE),
                           ncol = 1))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               c("spp1", "spp2"))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               c("cost"))
  expect_error(x$feature_targets())
})

test_that("x=data.frame, features=ZonesCharacter", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost_1 = runif(10), cost_2 = runif(10),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  # create problem
  x <- problem(pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
               c("cost_1", "cost_2"))
  print(x)
  x
  # run tests
  expect_equal(x$feature_names(), c("1", "2"))
  expect_equal(x$zone_names(), c("1", "2"))
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 2)
  expect_equal(x$number_of_planning_units(), 10)
  expect_equivalent(x$planning_unit_costs(), as.matrix(pu[, 2:3]))
  expect_equal(colnames(x$planning_unit_costs()), c("1", "2"))
  expect_equal(names(x$data$rij_matrix), c("1", "2"))
  expect_true(all(x$data$rij_matrix[[1]] ==
                  Matrix::sparseMatrix(i = c(rep(1, 10), rep(2, 9)),
                                       j = c(1:10, 1:9),
                                       x = c(pu$spp1_1, pu$spp2_1[1:9]),
                                       dims = c(2, 10))))
  expect_true(all(x$data$rij_matrix[[2]] ==
                  Matrix::sparseMatrix(i = c(rep(1, 10), rep(2, 10)),
                                       j = c(1:10, 1:10),
                                       x = c(pu$spp1_2, pu$spp2_2),
                                       dims = c(2, 10))))
  expect_equal(names(x$data$rij_matrix), c("1", "2"))
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("1", "2"))
  expect_equal(rownames(x$data$rij_matrix[[2]]), c("1", "2"))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(colSums(pu[, 4:7], na.rm = TRUE), ncol = 2))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               c("1", "2"))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               c("1", "2"))
  expect_error(x$feature_targets())
})

test_that("x=data.frame, features=data.frame (single zone)", {
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
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), 5)
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), 10)
  expect_equivalent(x$planning_unit_costs(), matrix(pu$cost, ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  expect_equivalent(x$data$rij_matrix[[1]],
                    Matrix::sparseMatrix(i = rij[[2]], j = rij[[1]],
                                         x = rij[[3]],
                                         dims = c(5, 10)))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), letters[1:5])
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    Matrix::rowSums(Matrix::sparseMatrix(i = rij[[2]],
                                                         j = rij[[1]],
                                                         x = rij[[3]])))
  expect_equal(rownames(x$feature_abundances_in_planning_units()), letters[1:5])
  expect_equal(colnames(x$feature_abundances_in_planning_units()), "cost")
  expect_error(x$feature_targets())
})

test_that("x=data.frame, features=data.frame (multiple zones)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost_1 = runif(10), cost_2 = runif(10))
  species <- data.frame(id = seq_len(5), name = letters[1:5], targets = 0.5)
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5), zone = 1:2)
  rij$amount <- runif(nrow(rij))
  z <- data.frame(id = 1:2, name = c("z1", "z2"))
  # create problem
  x <- problem(pu, species, rij, c("cost_1", "cost_2"), z)
  print(x)
  x
  # run tests
  expect_equal(x$feature_names(), letters[1:5])
  expect_equal(x$zone_names(), c("z1", "z2"))
  expect_equal(x$number_of_features(), 5)
  expect_equal(x$number_of_zones(), 2)
  expect_equal(x$number_of_planning_units(), 10)
  expect_equivalent(x$planning_unit_costs(), as.matrix(pu[, 2:3]))
  expect_equal(colnames(x$planning_unit_costs()), c("z1", "z2"))
  expect_equal(names(x$data$rij_matrix), c("z1", "z2"))
  expect_equivalent(x$data$rij_matrix[[1]],
                    Matrix::sparseMatrix(i = rij[[2]][rij[[3]] == 1],
                                         j = rij[[1]][rij[[3]] == 1],
                                         x = rij[[4]][rij[[3]] == 1],
                                         dims = c(5, 10)))
  expect_equivalent(x$data$rij_matrix[[2]],
                    Matrix::sparseMatrix(i = rij[[2]][rij[[3]] == 2],
                                         j = rij[[1]][rij[[3]] == 2],
                                         x = rij[[4]][rij[[3]] == 2],
                                         dims = c(5, 10)))
  expect_equal(rownames(x$data$rij_matrix[[1]]), letters[1:5])
  expect_equal(rownames(x$data$rij_matrix[[2]]), letters[1:5])
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(aggregate(rij[[4]], by = list(rij[[2]], rij[[3]]),
                                     sum)[[3]], ncol = 2))
  expect_equal(rownames(x$feature_abundances_in_planning_units()), letters[1:5])
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               c("z1", "z2"))
  expect_error(x$feature_targets())
})

test_that("x=numeric, features=data.frame", {
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
  expect_equal(x$zone_names(), "1")
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), 10)
  expect_equivalent(x$planning_unit_costs(),
                    matrix(pu$cost, ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "1")
  expect_equal(names(x$data$rij_matrix), "1")
  expect_equivalent(x$data$rij_matrix[[1]],
                    as(t(pu[, 3:4]), "sparseMatrix"))
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("spp1", "spp2"))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    rowSums(t(pu[, 3:4]), na.rm = TRUE))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               c("spp1", "spp2"))
  expect_equal(colnames(x$feature_abundances_in_planning_units()), "1")
  expect_error(x$feature_targets())
})

test_that("x=matrix, features=data.frame", {
  # simulate data
  pu <- data.frame(id = seq_len(10),
                   cost_1 = runif(10), cost_2 = runif(10),
                   spp1_1 = runif(10), spp2_1 = runif(10),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  # create problem
  x <- problem(as.matrix(pu[, 2:3]),
               data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7]))))
  print(x)
  x
  # run tests
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$zone_names(), c("1", "2"))
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 2)
  expect_equal(x$number_of_planning_units(), 10)
  expect_equivalent(x$planning_unit_costs(),
                    as.matrix(pu[, 2:3]))
  expect_equal(colnames(x$planning_unit_costs()), c("1", "2"))
  expect_equal(names(x$data$rij_matrix), c("1", "2"))
  expect_equivalent(x$data$rij_matrix[[1]],
                    as(t(pu[, 4:5]), "sparseMatrix"))
  expect_equivalent(x$data$rij_matrix[[2]],
                    as(t(pu[, 6:7]), "sparseMatrix"))
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("spp1", "spp2"))
  expect_equal(rownames(x$data$rij_matrix[[2]]), c("spp1", "spp2"))
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(colSums(pu[, 4:7]), ncol = 2))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               c("spp1", "spp2"))
  expect_equal(colnames(x$feature_abundances_in_planning_units()), c("1", "2"))
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
