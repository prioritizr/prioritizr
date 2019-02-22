context("problem")

test_that("x=Raster, features=RasterStack", {
  # load data
  data(sim_pu_raster, sim_features)
  # create problem
  x <- problem(sim_pu_raster, sim_features)
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), names(sim_pu_raster))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(),
    length(raster::Which(!is.na(sim_pu_raster), cells = TRUE)))
  expect_equal(x$number_of_total_units(), raster::ncell(sim_pu_raster))
  expect_equal(x$planning_unit_indices(),
               raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  expect_equal(problem(sim_pu_raster, sim_pu_raster)$number_of_features(), 1L)
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(),
               matrix(sim_pu_raster[[1]][!is.na(sim_pu_raster)], ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), names(sim_pu_raster))
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(x$feature_abundances_in_planning_units(),
    matrix(raster::cellStats(raster::mask(sim_features, sim_pu_raster),
                             "sum"), ncol = 1))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               x$zone_names())
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               x$feature_names())
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
    matrix(raster::cellStats(sim_features, "sum"), ncol = 1))
  expect_equal(colnames(x$feature_abundances_in_total_units()),
               x$zone_names())
  expect_equal(rownames(x$feature_abundances_in_total_units()),
               x$feature_names())
  # tests for rij_matrix field
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_raster, sim_features)))
  expect_equal(names(x$data$rij_matrix), x$zone_names())
  expect_equal(rownames(x$data$rij_matrix[[1]]), x$feature_names())
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=RasterStack, features=ZonesRaster", {
  # load data
  data(sim_pu_zones_stack, sim_features_zones)
  # create problem
  x <- problem(sim_pu_zones_stack, sim_features_zones)
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), feature_names(sim_features_zones))
  expect_equal(x$zone_names(), zone_names(sim_features_zones))
  expect_equal(x$number_of_features(), number_of_features(sim_features_zones))
  expect_equal(x$number_of_zones(), number_of_zones(sim_features_zones))
  expect_equal(x$number_of_planning_units(),
               raster::cellStats(max(!is.na(sim_pu_zones_stack)), "sum"))
  expect_equal(x$planning_unit_indices(),
               raster::Which(max(!is.na(sim_pu_zones_stack)) > 0, cells = TRUE))
  expect_equal(x$number_of_total_units(), raster::ncell(sim_pu_zones_stack))
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(),
                    sim_pu_zones_stack[raster::Which(
                      max(!is.na(sim_pu_zones_stack)) == 1)])
  expect_equal(colnames(x$planning_unit_costs()),
               zone_names(sim_features_zones))
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(x$feature_abundances_in_planning_units(),
    sapply(seq_len(raster::nlayers(sim_pu_zones_stack)), function(i) {
      raster::cellStats(raster::mask(sim_features_zones[[i]],
                                     sim_pu_zones_stack[[i]]), "sum")
  }))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
              zone_names(sim_features_zones))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
              feature_names(sim_features_zones))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
    sapply(seq_len(raster::nlayers(sim_pu_zones_stack)), function(i) {
      raster::cellStats(sim_features_zones[[i]], "sum")
  }))
  expect_equal(colnames(x$feature_abundances_in_total_units()),
              zone_names(sim_features_zones))
  expect_equal(rownames(x$feature_abundances_in_total_units()),
              feature_names(sim_features_zones))
  # tests for rij_matrix field
  expect_equivalent(x$data$rij_matrix,
                    lapply(seq_len(raster::nlayers(sim_pu_zones_stack)),
                          function(i) rij_matrix(sim_pu_zones_stack[[i]],
                                                 sim_features_zones[[i]])))
  expect_equal(names(x$data$rij_matrix), zone_names(sim_features_zones))
  expect_equivalent(sapply(x$data$rij_matrix, rownames),
               matrix(feature_names(sim_features_zones),
                      ncol = number_of_zones(sim_features_zones),
                      nrow = number_of_features(sim_features_zones)))
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=SpatialPolygonsDataFrame, features=RasterStack", {
  # load data
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons$cost[1:5] <- NA
  # create problem
  x <- problem(sim_pu_polygons, sim_features, "cost")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_polygons$cost)))
  expect_equal(x$planning_unit_indices(), which(!is.na(sim_pu_polygons$cost)))
  expect_equal(x$number_of_total_units(), nrow(sim_pu_polygons))
  expect_equal(problem(sim_pu_polygons, sim_pu_raster,
                       "cost")$number_of_features(), 1L)
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_polygons$cost[!is.na(sim_pu_polygons$cost)],
                           ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    Matrix::rowSums(x$data$rij_matrix[[1]]))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               "cost")
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               names(sim_features))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    matrix(colSums(raster::extract(sim_features,
                                                   sim_pu_polygons,
                                                   "sum", na.rm = TRUE)),
                           ncol = 1))
  expect_equal(colnames(x$feature_abundances_in_total_units()),
               "cost")
  expect_equal(rownames(x$feature_abundances_in_total_units()),
               names(sim_features))
  # tests for rij_matrix field
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_polygons[
                      !is.na(sim_pu_polygons[[1]]), ], sim_features)))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), names(sim_features))
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=SpatialPolygonsDataFrame, features=ZonesRaster", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  sim_pu_zones_polygons[5, paste0("cost_", 1:3)] <- NA
  sim_pu_zones_polygons[4, "cost_1"] <- NA
  # create problem
  x <- problem(sim_pu_zones_polygons, sim_features_zones, paste0("cost_", 1:3))
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), feature_names(sim_features_zones))
  expect_equal(x$zone_names(), zone_names(sim_features_zones))
  expect_equal(x$number_of_features(), raster::nlayers(sim_features_zones[[1]]))
  expect_equal(x$number_of_planning_units(), nrow(sim_pu_zones_polygons) - 1)
  expect_equal(x$planning_unit_indices(),
               c(seq_len(4), seq(6, nrow(sim_pu_zones_polygons))))
  expect_equal(x$number_of_total_units(), nrow(sim_pu_polygons))
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(), as.matrix(
    sim_pu_zones_polygons@data[-5, paste0("cost_", 1:3)]))
  expect_equal(colnames(x$planning_unit_costs()),
               zone_names(sim_features_zones))
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(x$feature_abundances_in_planning_units(),
    sapply(seq_along(x$data$rij_matrix), function(i) {
      pos1 <- x$planning_unit_indices()
      pos2 <- which(!is.na(sim_pu_zones_polygons@data[[paste0("cost_", i)]]))
      pos3 <- match(pos2, pos1)
      Matrix::rowSums(x$data$rij_matrix[[i]][, pos3, drop = FALSE])
    }))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               zone_names(sim_features_zones))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               feature_names(sim_features_zones))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    sapply(lapply(sim_features_zones, raster::extract,
                                          sim_pu_zones_polygons, "sum",
                                          na.rm = TRUE),
                                   colSums, na.rm = TRUE))
  expect_equal(colnames(x$feature_abundances_in_total_units()),
               zone_names(sim_features_zones))
  expect_equal(rownames(x$feature_abundances_in_total_units()),
               feature_names(sim_features_zones))
  # tests for rij_matrix field
  expect_equivalent(x$data$rij_matrix,
                    lapply(sim_features_zones, rij_matrix,
                           x = sim_pu_zones_polygons[-5, ]))
  expect_equal(names(x$data$rij_matrix), zone_names(sim_features_zones))
  expect_equivalent(sapply(x$data$rij_matrix, rownames),
               matrix(feature_names(sim_features_zones),
                      ncol = number_of_zones(sim_features_zones),
                      nrow = number_of_features(sim_features_zones)))
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=SpatialLinesDataFrame, features=RasterStack", {
  # load data
  data(sim_pu_lines, sim_features)
  sim_pu_lines$cost[1:5] <- NA
  # create problem
  x <- problem(sim_pu_lines, sim_features, "cost")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_lines$cost)))
  expect_equal(x$planning_unit_indices(), which(!is.na(sim_pu_lines$cost)))
  expect_equal(x$number_of_total_units(), nrow(sim_pu_lines))
  expect_equal(problem(sim_pu_lines, sim_pu_raster,
                       "cost")$number_of_features(), 1L)
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_lines$cost[!is.na(sim_pu_lines$cost)],
                           ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    Matrix::rowSums(x$data$rij_matrix[[1]]))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               "cost")
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               names(sim_features))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    matrix(rowSums(rij_matrix(sim_pu_lines, sim_features),
                                   na.rm = TRUE), ncol = 1))
  expect_equal(colnames(x$feature_abundances_in_total_units()),
               "cost")
  expect_equal(rownames(x$feature_abundances_in_total_units()),
               names(sim_features))
  # tests for rij_matrix field
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_lines[
                      !is.na(sim_pu_lines[[1]]), ], sim_features)))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), names(sim_features))
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=SpatialPointsDataFrame, features=RasterStack", {
  # load data
  data(sim_pu_points, sim_features)
  sim_pu_points$cost[1:5] <- NA
  # create problem
  x <- problem(sim_pu_points, sim_features, "cost")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), raster::nlayers(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_points$cost)))
  expect_equal(x$planning_unit_indices(), which(!is.na(sim_pu_points$cost)))
  expect_equal(x$number_of_total_units(), nrow(sim_pu_points))
  expect_equal(problem(sim_pu_points, sim_pu_raster,
                       "cost")$number_of_features(), 1L)
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_points$cost[!is.na(sim_pu_points$cost)],
                           ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    Matrix::rowSums(x$data$rij_matrix[[1]]))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               "cost")
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               names(sim_features))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    matrix(rowSums(rij_matrix(sim_pu_points, sim_features),
                                   na.rm = TRUE), ncol = 1))
  expect_equal(colnames(x$feature_abundances_in_total_units()),
               "cost")
  expect_equal(rownames(x$feature_abundances_in_total_units()),
               names(sim_features))
  # tests for rij_matrix field
  expect_equivalent(x$data$rij_matrix,
                    list(rij_matrix(sim_pu_points[
                      !is.na(sim_pu_points[[1]]), ], sim_features)))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), names(sim_features))
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=SpatialPolygonsDataFrame, features=character", {
  # simulate data
  data(sim_pu_polygons)
  sim_pu_polygons$cost[2] <- NA
  sim_pu_polygons$spp1 <- runif(length(sim_pu_polygons))
  sim_pu_polygons$spp2 <- c(NA, rpois(length(sim_pu_polygons) - 1, 5))
  # create problem
  x <- problem(sim_pu_polygons, c("spp1", "spp2"), "cost")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), length(sim_pu_polygons) - 1)
  expect_equal(x$planning_unit_indices(), c(1, seq(3, length(sim_pu_polygons))))
  expect_equal(x$number_of_total_units(), length(sim_pu_polygons))
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(),
                    matrix(sim_pu_polygons$cost[-2], ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(colSums(sim_pu_polygons@data[-2, c("spp1", "spp2")],
                                   na.rm = TRUE), ncol = 1))
  expect_equal(colnames(x$feature_abundances_in_planning_units()), "cost")
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               c("spp1", "spp2"))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    matrix(colSums(sim_pu_polygons@data[, c("spp1", "spp2")],
                                   na.rm = TRUE), ncol = 1))
  expect_equal(colnames(x$feature_abundances_in_total_units()), "cost")
  expect_equal(rownames(x$feature_abundances_in_total_units()),
               c("spp1", "spp2"))
  # tests for rij_matrix field
  rij <- Matrix::sparseMatrix(i = c(rep(1, length(sim_pu_polygons) - 1),
                                    rep(2, length(sim_pu_polygons) - 2)),
                              j = c(seq_len(length(sim_pu_polygons) - 1),
                                    seq_len(length(sim_pu_polygons) - 1)[-1]),
                              x = c(sim_pu_polygons$spp1[-2],
                                    sim_pu_polygons$spp2[c(-1, -2)]),
                              dims = c(2, length(sim_pu_polygons) - 1))
  rij <- list(rij)
  expect_true(all(x$data$rij_matrix[[1]] == rij[[1]]))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]),  c("spp1", "spp2"))
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=SpatialPolygonsDataFrame, features=ZonesCharacter", {
  # simulate data
  data(sim_pu_zones_polygons)
  sim_pu_zones_polygons$cost_1[2] <- NA
  sim_pu_zones_polygons[3, c("cost_1", "cost_2")] <- NA
  sim_pu_zones_polygons$spp1_1 <- runif(length(sim_pu_zones_polygons))
  sim_pu_zones_polygons$spp2_1 <- c(NA, rpois(length(sim_pu_zones_polygons) - 1,
                                    5))
  sim_pu_zones_polygons$spp1_2 <- runif(length(sim_pu_zones_polygons))
  sim_pu_zones_polygons$spp2_2 <- runif(length(sim_pu_zones_polygons))
  sim_pu_zones_polygons <- sim_pu_zones_polygons[1:5, ]
  # create problem
  x <- problem(sim_pu_zones_polygons,
               zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2"),
                     zone_names = c("z1", "z2"),
                     feature_names = c("spp1", "spp2")),
               c("cost_1", "cost_2"))
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$zone_names(), c("z1", "z2"))
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 2)
  expect_equal(x$number_of_planning_units(), length(sim_pu_zones_polygons) - 1)
  expect_equal(x$planning_unit_indices(),
               c(c(1, 2), seq(4, length(sim_pu_zones_polygons))))
  expect_equal(x$number_of_total_units(), length(sim_pu_zones_polygons))
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(),
                    as.matrix(sim_pu_zones_polygons@data[-3, c("cost_1",
                                                               "cost_2")]))
  expect_equal(colnames(x$planning_unit_costs()), c("z1", "z2"))
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    matrix(c(sum(sim_pu_zones_polygons$spp1_1[
               !is.na(sim_pu_zones_polygons$cost_1)], na.rm = TRUE),
             sum(sim_pu_zones_polygons$spp2_1[
               !is.na(sim_pu_zones_polygons$cost_1)], na.rm = TRUE),
             sum(sim_pu_zones_polygons$spp1_2[
               !is.na(sim_pu_zones_polygons$cost_2)], na.rm = TRUE),
             sum(sim_pu_zones_polygons$spp2_2[
               !is.na(sim_pu_zones_polygons$cost_2)], na.rm = TRUE)),
           ncol = 4))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               c("z1", "z2"))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               c("spp1", "spp2"))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
    matrix(colSums(sim_pu_zones_polygons@data[,
    c("spp1_1", "spp2_1", "spp1_2", "spp2_2")], na.rm = TRUE), ncol = 2))
  expect_equal(colnames(x$feature_abundances_in_total_units()),
               c("z1", "z2"))
  expect_equal(rownames(x$feature_abundances_in_total_units()),
               c("spp1", "spp2"))
  # tests for rij_matrix field
  r1 <- Matrix::sparseMatrix(i = c(rep(1, length(sim_pu_zones_polygons) - 1),
                                   rep(2, length(sim_pu_zones_polygons) - 2)),
                             j = c(seq_len(length(sim_pu_zones_polygons) - 1),
                                   seq_len(length(sim_pu_zones_polygons) -
                                           1)[-1]),
                             x = c(sim_pu_zones_polygons$spp1_1[-3],
                                   sim_pu_zones_polygons$spp2_1[c(-1, -3)]),
                             dims = c(2, length(sim_pu_zones_polygons) - 1))
  r2 <- Matrix::sparseMatrix(i = c(rep(1, length(sim_pu_zones_polygons) - 1),
                                   rep(2, length(sim_pu_zones_polygons) - 1)),
                             j = c(seq_len(length(sim_pu_zones_polygons) - 1),
                                   seq_len(length(sim_pu_zones_polygons) - 1)),
                             x = c(sim_pu_zones_polygons$spp1_2[-3],
                                   sim_pu_zones_polygons$spp2_2[-3]),
                             dims = c(2, length(sim_pu_zones_polygons) - 1))
  rij <- list(r1, r2)
  expect_equal(names(x$data$rij_matrix), c("z1", "z2"))
  expect_true(all(x$data$rij_matrix[[1]] == rij[[1]]))
  expect_true(all(x$data$rij_matrix[[2]] == rij[[2]]))
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=data.frame, features=character", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(runif(1), NA, runif(8)),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  x <- problem(pu, c("spp1", "spp2"), "cost")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), 9)
  expect_equal(x$planning_unit_indices(), which(!is.na(pu$cost)))
  expect_equal(x$number_of_total_units(), 10)
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(), matrix(pu$cost[-2], ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(colSums(pu[-2, c("spp1", "spp2")], na.rm = TRUE),
                           ncol = 1))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               c("spp1", "spp2"))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               c("cost"))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    matrix(colSums(pu[, c("spp1", "spp2")], na.rm = TRUE),
                           ncol = 1))
  expect_equal(rownames(x$feature_abundances_in_total_units()),
               c("spp1", "spp2"))
  expect_equal(colnames(x$feature_abundances_in_total_units()),
               c("cost"))
  # tests for rij_matrix field
  expect_true(all(x$data$rij_matrix[[1]] ==
                  Matrix::sparseMatrix(i = c(rep(1, 9), rep(2, 8)),
                                       j = c(1:9, 1:8),
                                       x = c(pu$spp1[-2], pu$spp2[c(-2, -10)]),
                                       dims = c(2, 9))))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("spp1", "spp2"))
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=data.frame, features=ZonesCharacter", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost_1 = c(NA, NA, runif(8)),
                   cost_2 = c(0.3, NA, runif(8)),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  # create problem
  x <- problem(pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
               c("cost_1", "cost_2"))
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), c("1", "2"))
  expect_equal(x$zone_names(), c("1", "2"))
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 2)
  expect_equal(x$number_of_planning_units(), 9)
  expect_equal(x$planning_unit_indices(), c(1, seq(3, nrow(pu))))
  expect_equal(x$number_of_total_units(), 10)
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(), as.matrix(pu[-2, 2:3]))
  expect_equal(colnames(x$planning_unit_costs()), c("1", "2"))
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    matrix(c(sum(pu$spp1_1[!is.na(pu$cost_1)], na.rm = TRUE),
             sum(pu$spp2_1[!is.na(pu$cost_1)], na.rm = TRUE),
             sum(pu$spp1_2[!is.na(pu$cost_2)], na.rm = TRUE),
             sum(pu$spp2_2[!is.na(pu$cost_2)], na.rm = TRUE)),
           ncol = 4))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               c("1", "2"))
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               c("1", "2"))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    matrix(colSums(pu[, 4:7], na.rm = TRUE), ncol = 2))
  expect_equal(rownames(x$feature_abundances_in_total_units()),
               c("1", "2"))
  expect_equal(colnames(x$feature_abundances_in_total_units()),
               c("1", "2"))
  # tests for rij_matrix field
  expect_equal(names(x$data$rij_matrix), c("1", "2"))
  expect_true(all(x$data$rij_matrix[[1]] ==
                  Matrix::sparseMatrix(i = c(rep(1, 9), rep(2, 8)),
                                       j = c(1:9, 1:8),
                                       x = c(pu$spp1_1[-2],
                                             pu$spp2_1[c(-2, -10)]),
                                       dims = c(2, 9))))
  expect_true(all(x$data$rij_matrix[[2]] ==
                  Matrix::sparseMatrix(i = c(rep(1, 9), rep(2, 9)),
                                       j = c(1:9, 1:9),
                                       x = c(pu$spp1_2[-2], pu$spp2_2[-2]),
                                       dims = c(2, 9))))
  expect_equal(names(x$data$rij_matrix), c("1", "2"))
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("1", "2"))
  expect_equal(rownames(x$data$rij_matrix[[2]]), c("1", "2"))
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=data.frame, features=data.frame (single zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(0.1, NA, runif(8)))
  species <- data.frame(id = seq_len(5), name = letters[1:5], targets = 0.5)
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5))
  rij$amount <- runif(nrow(rij))
  # create problem
  x <- problem(pu, species, rij, "cost")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), letters[1:5])
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), 5)
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), 9)
  expect_equal(x$planning_unit_indices(), which(!is.na(pu$cost)))
  expect_equal(x$number_of_total_units(), 10)
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(), matrix(pu$cost[-2], ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  # tests for feature_abundances_in_planning_units field
  rij2 <- rij[rij$pu != 2, ]
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    Matrix::rowSums(Matrix::sparseMatrix(i = rij2[[2]],
                                                         j = rij2[[1]],
                                                         x = rij2[[3]])))
  expect_equal(rownames(x$feature_abundances_in_planning_units()), letters[1:5])
  expect_equal(colnames(x$feature_abundances_in_planning_units()), "cost")
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    Matrix::rowSums(Matrix::sparseMatrix(i = rij[[2]],
                                                         j = rij[[1]],
                                                         x = rij[[3]])))
  expect_equal(rownames(x$feature_abundances_in_total_units()), letters[1:5])
  expect_equal(colnames(x$feature_abundances_in_total_units()), "cost")
  # tests for rij_matrix field
  rij2 <- rij[rij$pu != 2, ]
  rij2$pu <- match(rij2$pu, pu$id[-2])
  expect_equivalent(x$data$rij_matrix[[1]],
                    Matrix::sparseMatrix(i = rij2[[2]], j = rij2[[1]],
                                         x = rij2[[3]],
                                         dims = c(5, 9)))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), letters[1:5])
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=data.frame, features=data.frame (multiple zones)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost_1 = c(0.1, NA, runif(8)),
                   cost_2 = c(NA, NA, runif(8)))
  species <- data.frame(id = seq_len(5), name = letters[1:5], targets = 0.5)
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5), zone = 1:2)
  rij$amount <- runif(nrow(rij))
  z <- data.frame(id = 1:2, name = c("z1", "z2"))
  # create problem
  x <- problem(pu, species, rij, c("cost_1", "cost_2"), z)
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), letters[1:5])
  expect_equal(x$zone_names(), c("z1", "z2"))
  expect_equal(x$number_of_features(), 5)
  expect_equal(x$number_of_zones(), 2)
  expect_equal(x$number_of_planning_units(), 9)
  expect_equal(x$planning_unit_indices(), c(1, seq(3, nrow(pu))))
  expect_equal(x$number_of_total_units(), 10)
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(), as.matrix(pu[-2, 2:3]))
  expect_equal(colnames(x$planning_unit_costs()), c("z1", "z2"))
  # tests for feature_abundances_in_planning_units field
  rij2 <- rij
  rij2 <- rij2[!(rij2$pu %in% pu$id[is.na(pu$cost_1)] & rij2$zone == 1), ]
  rij2 <- rij2[!(rij2$pu %in% pu$id[is.na(pu$cost_2)] & rij2$zone == 2), ]
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    matrix(aggregate(rij2[[4]], by = list(rij2[[2]], rij2[[3]]),
                                     sum)[[3]], ncol = 2))
  expect_equal(rownames(x$feature_abundances_in_planning_units()), letters[1:5])
  expect_equal(colnames(x$feature_abundances_in_planning_units()),
               c("z1", "z2"))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    matrix(aggregate(rij[[4]], by = list(rij[[2]], rij[[3]]),
                                     sum)[[3]], ncol = 2))
  expect_equal(rownames(x$feature_abundances_in_total_units()), letters[1:5])
  expect_equal(colnames(x$feature_abundances_in_total_units()),
               c("z1", "z2"))
  # tests for rij_matrix field
  rij2 <- rij[rij$pu != 2, ]
  rij2$pu <- match(rij2$pu, seq_len(9))
  expect_equal(names(x$data$rij_matrix), c("z1", "z2"))
  expect_equivalent(x$data$rij_matrix[[1]],
                    Matrix::sparseMatrix(i = rij2[[2]][rij2[[3]] == 1],
                                         j = rij2[[1]][rij2[[3]] == 1],
                                         x = rij2[[4]][rij2[[3]] == 1],
                                         dims = c(5, 9)))
  expect_equivalent(x$data$rij_matrix[[2]],
                    Matrix::sparseMatrix(i = rij2[[2]][rij2[[3]] == 2],
                                         j = rij2[[1]][rij2[[3]] == 2],
                                         x = rij2[[4]][rij2[[3]] == 2],
                                         dims = c(5, 9)))
  expect_equal(rownames(x$data$rij_matrix[[1]]), letters[1:5])
  expect_equal(rownames(x$data$rij_matrix[[2]]), letters[1:5])
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=numeric, features=data.frame", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  x <- problem(pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               as.matrix(t(pu[, 3:4])))
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$zone_names(), "1")
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), 9)
  expect_equal(x$planning_unit_indices(), which(!is.na(pu$cost)))
  expect_equal(x$number_of_total_units(), 10)
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(),
                    matrix(pu$cost[-2], ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "1")
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    rowSums(t(pu[-2, 3:4]), na.rm = TRUE))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               c("spp1", "spp2"))
  expect_equal(colnames(x$feature_abundances_in_planning_units()), "1")
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    rowSums(t(pu[, 3:4]), na.rm = TRUE))
  expect_equal(rownames(x$feature_abundances_in_total_units()),
               c("spp1", "spp2"))
  expect_equal(colnames(x$feature_abundances_in_total_units()), "1")
  # tests for rij_matrix field
  expect_equal(names(x$data$rij_matrix), "1")
  expect_equivalent(x$data$rij_matrix[[1]],
                    as(t(pu[-2, 3:4]), "sparseMatrix"))
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("spp1", "spp2"))
  # test that calling targets before they have been inititalized throws error
  expect_error(x$feature_targets())
})

test_that("x=matrix, features=data.frame", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost_1 = c(NA, NA, runif(8)),
                   cost_2 = c(0.3, NA, runif(8)),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  # create problem
  x <- problem(as.matrix(pu[, 2:3]),
               data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7]))))
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$zone_names(), c("1", "2"))
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 2)
  expect_equal(x$number_of_planning_units(), 9)
  expect_equal(x$planning_unit_indices(), c(1, seq(3, nrow(pu))))
  expect_equal(x$number_of_total_units(), 10)
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(),
                    as.matrix(pu[-2, 2:3]))
  expect_equal(colnames(x$planning_unit_costs()), c("1", "2"))
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    matrix(c(sum(pu$spp1_1[!is.na(pu$cost_1)], na.rm = TRUE),
             sum(pu$spp2_1[!is.na(pu$cost_1)], na.rm = TRUE),
             sum(pu$spp1_2[!is.na(pu$cost_2)], na.rm = TRUE),
             sum(pu$spp2_2[!is.na(pu$cost_2)], na.rm = TRUE)),
           ncol = 4))
  expect_equal(rownames(x$feature_abundances_in_planning_units()),
               c("spp1", "spp2"))
  expect_equal(colnames(x$feature_abundances_in_planning_units()), c("1", "2"))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    matrix(colSums(pu[, 4:7], na.rm = TRUE), ncol = 2))
  expect_equal(rownames(x$feature_abundances_in_total_units()),
               c("spp1", "spp2"))
  expect_equal(colnames(x$feature_abundances_in_total_units()), c("1", "2"))
  # tests for rij_matrix field
  expect_equal(names(x$data$rij_matrix), c("1", "2"))
  expect_equivalent(x$data$rij_matrix[[1]],
                    as(t(pu[-2, 4:5]), "sparseMatrix"))
  expect_equivalent(x$data$rij_matrix[[2]],
                    as(t(pu[-2, 6:7]), "sparseMatrix"))
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("spp1", "spp2"))
  expect_equal(rownames(x$data$rij_matrix[[2]]), c("spp1", "spp2"))
  # test that calling targets before they have been inititalized throws error
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
  data(sim_pu_polygons, sim_pu_lines, sim_pu_points, sim_pu_raster,
       sim_features)
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
  data(sim_pu_polygons, sim_features)
  p1 <- problem(sim_pu_polygons, sim_features, "cost")
  p2 <- p1 %>% add_locked_in_constraints("locked_in")
  expect_equal(p1$constraints$length(), 0)
  expect_equal(p2$constraints$length(), 1)
})

test_that("get parameter methods", {
  data(sim_pu_raster, sim_features)
  x <- problem(sim_pu_raster, sim_features) %>%
       add_max_features_objective(400) %>%
       add_absolute_targets(0.1) %>%
       add_boundary_penalties(200) %>%
       add_binary_decisions() %>%
       add_shuffle_portfolio(100) %>%
       add_locked_in_constraints(1) %>%
       add_default_solver()
  id1 <- x$objective$parameters$find("budget")
  id2 <- x$solver$parameters$find("gap")
  id3 <- x$constraints[[x$constraints$ids()[1]]]$parameters$ids()[1]
  id4 <- x$portfolio$parameters$find("number_solutions")
  id5 <- x$penalties[[x$penalties$ids()[1]]]$parameters$find("penalty")
  expect_equal(x$get_objective_parameter(as.Id(id1)), 400)
  expect_equal(x$get_solver_parameter(as.Id(id2)), 0.1)
  expect_equal(x$get_constraint_parameter(as.Id(id3)),
               tibble::tibble(pu = 1, zone = factor("layer"), status = 1))
  expect_equal(x$get_portfolio_parameter(as.Id(id4)), 100)
  expect_equal(x$get_penalty_parameter(as.Id(id5)), 200)
})

test_that("set parameter methods", {
  data(sim_pu_raster, sim_features)
  x <- problem(sim_pu_raster, sim_features) %>%
       add_max_features_objective(400) %>%
       add_absolute_targets(0.1) %>%
       add_boundary_penalties(200) %>%
       add_binary_decisions() %>%
       add_shuffle_portfolio(100) %>%
       add_locked_in_constraints(1) %>%
       add_default_solver()
  id1 <- x$objective$parameters$find("budget")
  id2 <- x$solver$parameters$find("gap")
  id3 <- x$constraints[[x$constraints$ids()[1]]]$parameters$ids()[1]
  id4 <- x$portfolio$parameters$find("number_solutions")
  id5 <- x$penalties[[x$penalties$ids()[1]]]$parameters$find("penalty")
  expect_equal(x$get_objective_parameter(as.Id(id1)), 400)
  expect_equal(x$get_solver_parameter(as.Id(id2)), 0.1)
  expect_equal(x$get_constraint_parameter(as.Id(id3)),
               tibble::tibble(pu = 1, zone = factor("layer"), status = 1))
  expect_equal(x$get_portfolio_parameter(as.Id(id4)), 100)
  expect_equal(x$get_penalty_parameter(as.Id(id5)), 200)
  x$set_objective_parameter(as.Id(id1), 90)
  x$set_solver_parameter(as.Id(id2), 0.2)
  x$set_constraint_parameter(as.Id(id3),
                             tibble::tibble(pu = 2, zone = factor("layer"),
                             status = 1))
  x$set_portfolio_parameter(as.Id(id4), 300L)
  x$set_penalty_parameter(as.Id(id5), 500)
  expect_equal(x$get_objective_parameter(as.Id(id1)), 90)
  expect_equal(x$get_solver_parameter(as.Id(id2)), 0.2)
  expect_equal(x$get_constraint_parameter(as.Id(id3)),
               tibble::tibble(pu = 2, zone = factor("layer"), status = 1))
  expect_equal(x$get_portfolio_parameter(as.Id(id4)), 300)
  expect_equal(x$get_penalty_parameter(as.Id(id5)), 500)
})

test_that("render parameter methods", {
  data(sim_pu_raster, sim_features)
  x <- problem(sim_pu_raster, sim_features) %>%
       add_max_features_objective(400) %>%
       add_absolute_targets(0.1) %>%
       add_boundary_penalties(100) %>%
       add_binary_decisions() %>%
       add_shuffle_portfolio(100) %>%
       add_neighbor_constraints(1) %>%
       add_default_solver()
  id1 <- x$objective$parameters$find("budget")
  id2 <- x$solver$parameters$find("gap")
  id3 <- x$constraints[[x$constraints$ids()[1]]]$
           parameters$find("number of neighbors")
  id4 <- x$portfolio$parameters$find("number_solutions")
  id5 <- x$penalties[[x$penalties$ids()[1]]]$parameters$find("penalty")
  expect_is(x$render_objective_parameter(as.Id(id1)), "shiny.tag")
  expect_is(x$render_solver_parameter(as.Id(id2)), "shiny.tag")
  expect_is(x$render_constraint_parameter(as.Id(id3)), "shiny.tag")
  expect_is(x$render_portfolio_parameter(as.Id(id4)), "shiny.tag")
  expect_is(x$render_penalty_parameter(as.Id(id5)),
            c("shiny.tag", "shiny.tag.list"))
})

test_that("render all parameters methods", {
  data(sim_pu_raster, sim_features)
  x <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(0.1) %>%
       add_boundary_penalties(100) %>%
       add_binary_decisions() %>%
       add_shuffle_portfolio(100) %>%
       add_neighbor_constraints(1) %>%
       add_default_solver()
  expect_is(x$render_all_objective_parameters(), "shiny.tag")
  expect_is(x$render_all_solver_parameters(), "shiny.tag")
  expect_is(x$render_all_constraint_parameters(), "shiny.tag")
  expect_is(x$render_all_portfolio_parameters(), "shiny.tag")
  expect_is(x$render_all_penalty_parameters(), "shiny.tag")
})
