context("problem (deprecated)")

test_that("x = RasterLayer, features = RasterStack", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  expect_warning(
    x <- problem(raster::raster(sim_pu_raster), raster::stack(sim_features)),
    "deprecated"
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), names(sim_pu_raster))
  expect_equal(x$number_of_features(), terra::nlyr(sim_features))
  expect_equal(
    x$number_of_planning_units(),
    length(terra::cells(is.na(sim_pu_raster), 0)[[1]])
  )
  expect_equal(number_of_planning_units(x), x$number_of_planning_units())
  expect_equal(x$number_of_total_units(), terra::ncell(sim_pu_raster))
  expect_equal(
    x$planning_unit_indices(),
    terra::cells(is.na(sim_pu_raster), 0)[[1]]
  )
  expect_equal(problem(sim_pu_raster, sim_pu_raster)$number_of_features(), 1L)
  # tests for planning_unit_costs field
  expect_equivalent(
    x$planning_unit_costs(),
    matrix(sim_pu_raster[[1]][!is.na(sim_pu_raster)], ncol = 1)
  )
  expect_equal(colnames(x$planning_unit_costs()), names(sim_pu_raster))
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    matrix(
      terra::global(
        terra::mask(sim_features, sim_pu_raster), "sum", na.rm = TRUE
      )[[1]],
    )
  )
  expect_equal(
    colnames(x$feature_abundances_in_planning_units()),
    x$zone_names()
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    x$feature_names()
  )
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    matrix(terra::global(sim_features, "sum", na.rm = TRUE)[[1]], ncol = 1)
  )
  expect_equal(
    colnames(x$feature_abundances_in_total_units()),
    x$zone_names()
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    x$feature_names()
  )
  # tests for rij_matrix field
  expect_equivalent(
    x$data$rij_matrix,
    list(rij_matrix(sim_pu_raster, sim_features))
  )
  expect_equal(names(x$data$rij_matrix), x$zone_names())
  expect_equal(rownames(x$data$rij_matrix[[1]]), x$feature_names())
  # test that calling targets before they have been initialized throws error
  expect_tidy_error(x$feature_targets())
})

test_that("x = RasterStack, features = ZonesRaster", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  expect_warning(
    x <- problem(
      raster::stack(sim_zones_pu_raster),  as.ZonesRaster(sim_zones_features)
    ),
    "deprecated"
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), feature_names(sim_zones_features))
  expect_equal(x$zone_names(), zone_names(sim_zones_features))
  expect_equal(x$number_of_features(), number_of_features(sim_zones_features))
  expect_equal(x$number_of_zones(), number_of_zones(sim_zones_features))
  expect_equal(
    x$number_of_planning_units(),
    terra::global(max(!is.na(sim_zones_pu_raster)), "sum")[[1]]
  )
  expect_equal(
    x$planning_unit_indices(),
    terra::cells(min(is.na(sim_zones_pu_raster)), 0)[[1]]
  )
  expect_equal(x$number_of_total_units(), terra::ncell(sim_zones_pu_raster))
  # tests for planning_unit_costs field
  expect_equivalent(
    x$planning_unit_costs(),
    sim_zones_pu_raster[min(is.na(sim_zones_pu_raster)) == 0]
  )
  expect_equal(
    colnames(x$planning_unit_costs()),
    zone_names(sim_zones_features)
  )
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    sapply(seq_len(terra::nlyr(sim_zones_pu_raster)), function(i) {
      terra::global(
        terra::mask(sim_zones_features[[i]], sim_zones_pu_raster[[i]]),
        "sum", na.rm = TRUE
      )[[1]]
    })
  )
  expect_equal(
    colnames(x$feature_abundances_in_planning_units()),
    zone_names(sim_zones_features)
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    feature_names(sim_zones_features)
  )
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    sapply(seq_len(terra::nlyr(sim_zones_pu_raster)), function(i) {
      terra::global(sim_zones_features[[i]], "sum", na.rm = TRUE)[[1]]
    })
  )
  expect_equal(
    colnames(x$feature_abundances_in_total_units()),
    zone_names(sim_zones_features)
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    feature_names(sim_zones_features)
  )
  # tests for rij_matrix field
  expect_equivalent(
    x$data$rij_matrix,
    lapply(
      seq_len(terra::nlyr(sim_zones_pu_raster)), function(i) {
        rij_matrix(sim_zones_pu_raster[[i]], sim_zones_features[[i]])
      }
    )
  )
  expect_equal(names(x$data$rij_matrix), zone_names(sim_zones_features))
  expect_equivalent(
    sapply(x$data$rij_matrix, rownames),
    matrix(
      feature_names(sim_zones_features),
      ncol = number_of_zones(sim_zones_features),
      nrow = number_of_features(sim_zones_features)
    )
  )
  # test that calling targets before they have been initialized throws error
  expect_tidy_error(x$feature_targets())
})

test_that("x = Spatial, features = RasterStack", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # update data
  sim_pu_polygons$cost[1:5] <- NA
  # create problem
  expect_warning(
    x <- problem(
      sf::as_Spatial(sim_pu_polygons), raster::stack(sim_features), "cost"
    ),
    "deprecated"
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), terra::nlyr(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_polygons$cost)))
  expect_equal(x$planning_unit_indices(), which(!is.na(sim_pu_polygons$cost)))
  expect_equal(x$number_of_total_units(), nrow(sim_pu_polygons))
  # tests for planning_unit_costs field
  expect_equivalent(
    x$planning_unit_costs(),
    matrix(sim_pu_polygons$cost[!is.na(sim_pu_polygons$cost)], ncol = 1)
  )
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    Matrix::rowSums(x$data$rij_matrix[[1]])
  )
  expect_equal(
    colnames(x$feature_abundances_in_planning_units()),
    "cost"
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    names(sim_features)
  )
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    matrix(
      colSums(
        terra::extract(
          sim_features, terra::vect(sim_pu_polygons), "sum", na.rm = TRUE,
          ID = FALSE
        )
      ),
      ncol = 1
    )
  )
  expect_equal(
    colnames(x$feature_abundances_in_total_units()),
    "cost"
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    names(sim_features)
  )
  # tests for rij_matrix field
  expect_equivalent(
    x$data$rij_matrix,
    list(
      rij_matrix(
        sim_pu_polygons[!is.na(sim_pu_polygons[[1]]), ],
        sim_features
      )
    )
  )
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), names(sim_features))
  # test that calling targets before they have been initialized throws error
  expect_tidy_error(x$feature_targets())
})

test_that("x = Spatial, features = ZonesRaster", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # update data
  sim_zones_pu_polygons[5, paste0("cost_", 1:3)] <- NA
  sim_zones_pu_polygons[4, "cost_1"] <- NA
  # create problem
  expect_warning(
    x <- problem(
      sf::as_Spatial(sim_zones_pu_polygons), as.ZonesRaster(sim_zones_features),
      paste0("cost_", 1:3)
    ),
    "deprecated"
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), feature_names(sim_zones_features))
  expect_equal(x$zone_names(), zone_names(sim_zones_features))
  expect_equal(x$number_of_features(), terra::nlyr(sim_zones_features[[1]]))
  expect_equal(x$number_of_planning_units(), nrow(sim_zones_pu_polygons) - 1)
  expect_equal(
    x$planning_unit_indices(),
    c(seq_len(4), seq(6, nrow(sim_zones_pu_polygons)))
  )
  expect_equal(x$number_of_total_units(), nrow(sim_zones_pu_polygons))
  # tests for planning_unit_costs field
  expect_equivalent(
    x$planning_unit_costs(),
    as.matrix(
      sf::st_drop_geometry(sim_zones_pu_polygons)[-5, paste0("cost_", 1:3)]
    )
  )
  expect_equal(
    colnames(x$planning_unit_costs()),
    zone_names(sim_zones_features)
  )
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    sapply(seq_along(x$data$rij_matrix), function(i) {
      pos1 <- x$planning_unit_indices()
      pos2 <- which(!is.na(sim_zones_pu_polygons[[paste0("cost_", i)]]))
      pos3 <- match(pos2, pos1)
      Matrix::rowSums(x$data$rij_matrix[[i]][, pos3, drop = FALSE])
    })
  )
  expect_equal(
    colnames(x$feature_abundances_in_planning_units()),
    zone_names(sim_zones_features)
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    feature_names(sim_zones_features)
  )
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    sapply(
      lapply(
        sim_zones_features, terra::extract, terra::vect(sim_zones_pu_polygons),
        "sum", na.rm = TRUE, ID = FALSE
      ),
      colSums,
      na.rm = TRUE
    )
  )
  expect_equal(
    colnames(x$feature_abundances_in_total_units()),
    zone_names(sim_zones_features)
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    feature_names(sim_zones_features)
  )
  # tests for rij_matrix field
  expect_equivalent(
    x$data$rij_matrix,
    lapply(sim_zones_features, rij_matrix, x = sim_zones_pu_polygons[-5, ])
  )
  expect_equal(names(x$data$rij_matrix), zone_names(sim_zones_features))
  expect_equivalent(
    sapply(x$data$rij_matrix, rownames),
    matrix(
      feature_names(sim_zones_features),
      ncol = number_of_zones(sim_zones_features),
      nrow = number_of_features(sim_zones_features)
    )
  )
  # test that calling targets before they have been initialized throws error
  expect_tidy_error(x$feature_targets())
})

test_that("x = Spatial, features = character", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  # update data
  sim_pu_polygons$cost[2] <- NA
  sim_pu_polygons$spp1 <- runif(nrow(sim_pu_polygons))
  sim_pu_polygons$spp2 <- c(NA, rpois(nrow(sim_pu_polygons) - 1, 5))
  # create problem
  expect_warning(
    x <- problem(sf::as_Spatial(sim_pu_polygons), c("spp1", "spp2"), "cost"),
    "deprecated"
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), nrow(sim_pu_polygons) - 1)
  expect_equal(x$planning_unit_indices(), c(1, seq(3, nrow(sim_pu_polygons))))
  expect_equal(x$number_of_total_units(), nrow(sim_pu_polygons))
  # tests for planning_unit_costs field
  expect_equivalent(
    x$planning_unit_costs(),
    matrix(sim_pu_polygons$cost[-2], ncol = 1)
  )
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    matrix(
      colSums(
        sf::st_drop_geometry(sim_pu_polygons)[-2, c("spp1", "spp2")],
        na.rm = TRUE
      ),
      ncol = 1
    )
  )
  expect_equal(
    colnames(x$feature_abundances_in_planning_units()),
    "cost"
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    c("spp1", "spp2")
  )
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    matrix(
      colSums(
        sf::st_drop_geometry(sim_pu_polygons)[, c("spp1", "spp2")],
        na.rm = TRUE
      ),
      ncol = 1
    )
  )
  expect_equal(
    colnames(x$feature_abundances_in_total_units()),
    "cost"
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    c("spp1", "spp2")
  )
  # tests for rij_matrix field
  rij <- Matrix::sparseMatrix(
    i = c(
      rep(1, nrow(sim_pu_polygons) - 1),
      rep(2, nrow(sim_pu_polygons) - 2)
    ),
    j = c(
      seq_len(nrow(sim_pu_polygons) - 1),
      seq_len(nrow(sim_pu_polygons) - 1)[-1]
    ),
    x = c(
      sim_pu_polygons$spp1[-2],
      sim_pu_polygons$spp2[c(-1, -2)]
    ),
    dims = c(2, nrow(sim_pu_polygons) - 1)
  )
  rij <- list(rij)
  expect_true(all(x$data$rij_matrix[[1]] == rij[[1]]))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]),  c("spp1", "spp2"))
  # test that calling targets before they have been initialized throws error
  expect_tidy_error(x$feature_targets())
})

test_that("x = Spatial, features = ZonesCharacter", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  # update data
  sim_zones_pu_polygons$cost_1[2] <- NA
  sim_zones_pu_polygons[3, c("cost_1", "cost_2")] <- NA
  sim_zones_pu_polygons$spp1_1 <- runif(nrow(sim_zones_pu_polygons))
  sim_zones_pu_polygons$spp2_1 <-
    c(NA, rpois(nrow(sim_zones_pu_polygons) - 1, 5))
  sim_zones_pu_polygons$spp1_2 <- runif(nrow(sim_zones_pu_polygons))
  sim_zones_pu_polygons$spp2_2 <- runif(nrow(sim_zones_pu_polygons))
  sim_zones_pu_polygons <- sim_zones_pu_polygons[1:5, ]
  # create problem
  expect_warning(
    x <- problem(
      sf::as_Spatial(sim_zones_pu_polygons),
      zones(
        c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2"),
        zone_names = c("z1", "z2"),
        feature_names = c("spp1", "spp2")
      ),
      c("cost_1", "cost_2")
    ),
    "deprecated"
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), c("spp1", "spp2"))
  expect_equal(x$zone_names(), c("z1", "z2"))
  expect_equal(x$number_of_features(), 2)
  expect_equal(x$number_of_zones(), 2)
  expect_equal(x$number_of_planning_units(), nrow(sim_zones_pu_polygons) - 1)
  expect_equal(
    x$planning_unit_indices(),
    c(c(1, 2), seq(4, nrow(sim_zones_pu_polygons)))
  )
  expect_equal(x$number_of_total_units(), nrow(sim_zones_pu_polygons))
  # tests for planning_unit_costs field
  expect_equivalent(
    x$planning_unit_costs(),
    as.matrix(
      sf::st_drop_geometry(sim_zones_pu_polygons)[-3, c("cost_1", "cost_2")]
    )
  )
  expect_equal(colnames(x$planning_unit_costs()), c("z1", "z2"))
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    matrix(
      c(
        sum(
          sim_zones_pu_polygons$spp1_1[!is.na(sim_zones_pu_polygons$cost_1)],
          na.rm = TRUE
        ),
        sum(
          sim_zones_pu_polygons$spp2_1[!is.na(sim_zones_pu_polygons$cost_1)],
          na.rm = TRUE
        ),
        sum(
          sim_zones_pu_polygons$spp1_2[!is.na(sim_zones_pu_polygons$cost_2)],
          na.rm = TRUE
        ),
        sum(
          sim_zones_pu_polygons$spp2_2[!is.na(sim_zones_pu_polygons$cost_2)],
          na.rm = TRUE
        )
      ),
      ncol = 4
    )
  )
  expect_equal(
    colnames(x$feature_abundances_in_planning_units()),
    c("z1", "z2")
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    c("spp1", "spp2")
  )
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    matrix(
      colSums(
        sf::st_drop_geometry(sim_zones_pu_polygons)[,
          c("spp1_1", "spp2_1", "spp1_2", "spp2_2")
        ],
        na.rm = TRUE
      ),
      ncol = 2
    )
  )
  expect_equal(
    colnames(x$feature_abundances_in_total_units()),
    c("z1", "z2")
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    c("spp1", "spp2")
  )
  # tests for rij_matrix field
  r1 <- Matrix::sparseMatrix(
    i = c(
      rep(1, nrow(sim_zones_pu_polygons) - 1),
      rep(2, nrow(sim_zones_pu_polygons) - 2)),
    j = c(
      seq_len(nrow(sim_zones_pu_polygons) - 1),
      seq_len(nrow(sim_zones_pu_polygons) - 1)[-1]
    ),
    x = c(
      sim_zones_pu_polygons$spp1_1[-3],
      sim_zones_pu_polygons$spp2_1[c(-1, -3)]
    ),
    dims = c(2, nrow(sim_zones_pu_polygons) - 1)
  )
  r2 <- Matrix::sparseMatrix(
    i = c(
      rep(1, nrow(sim_zones_pu_polygons) - 1),
      rep(2, nrow(sim_zones_pu_polygons) - 1)),
    j = c(
      seq_len(nrow(sim_zones_pu_polygons) - 1),
      seq_len(nrow(sim_zones_pu_polygons) - 1)
    ),
    x = c(
      sim_zones_pu_polygons$spp1_2[-3],
      sim_zones_pu_polygons$spp2_2[-3]
    ),
    dims = c(2, nrow(sim_zones_pu_polygons) - 1)
  )
  rij <- list(r1, r2)
  expect_equal(names(x$data$rij_matrix), c("z1", "z2"))
  expect_true(all(x$data$rij_matrix[[1]] == rij[[1]]))
  expect_true(all(x$data$rij_matrix[[2]] == rij[[2]]))
  # test that calling targets before they have been initialized throws error
  expect_tidy_error(x$feature_targets())
})

test_that("x = sf, features = RasterStack", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # update data
  sim_pu_polygons$cost[1:5] <- NA
  # create problem
  expect_warning(
    x <- problem(sim_pu_polygons, raster::stack(sim_features), "cost"),
    "deprecated"
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), terra::nlyr(sim_features))
  expect_equal(x$number_of_planning_units(), sum(!is.na(sim_pu_polygons$cost)))
  expect_equal(x$planning_unit_indices(), which(!is.na(sim_pu_polygons$cost)))
  expect_equal(x$number_of_total_units(), nrow(sim_pu_polygons))
  # tests for planning_unit_costs field
  expect_equivalent(
    x$planning_unit_costs(),
    matrix(sim_pu_polygons$cost[!is.na(sim_pu_polygons$cost)], ncol = 1)
  )
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    Matrix::rowSums(x$data$rij_matrix[[1]])
  )
  expect_equal(
    colnames(x$feature_abundances_in_planning_units()),
     "cost"
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    names(sim_features)
  )
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    matrix(
      colSums(
        terra::extract(
          sim_features, sim_pu_polygons, "sum", ID = FALSE, na.rm = TRUE)
        ),
      ncol = 1
    )
  )
  expect_equal(
    colnames(x$feature_abundances_in_total_units()),
    "cost"
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    names(sim_features)
  )
  # tests for rij_matrix field
  expect_equivalent(
    x$data$rij_matrix,
    list(
      rij_matrix(
        sim_pu_polygons[!is.na(sim_pu_polygons[[1]]), ],
        sim_features
      )
    )
  )
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), names(sim_features))
  # test that calling targets before they have been initialized throws error
  expect_tidy_error(x$feature_targets())
})

test_that("x = sf, features = ZonesRaster", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  sim_zones_pu_polygons[5, paste0("cost_", 1:3)] <- NA
  sim_zones_pu_polygons[4, "cost_1"] <- NA
  # create problem
  expect_warning(
    x <- problem(
      sim_zones_pu_polygons, as.ZonesRaster(sim_zones_features),
      paste0("cost_", 1:3)
    ),
    "deprecated"
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), feature_names(sim_zones_features))
  expect_equal(x$zone_names(), zone_names(sim_zones_features))
  expect_equal(x$number_of_features(), terra::nlyr(sim_zones_features[[1]]))
  expect_equal(x$number_of_planning_units(), nrow(sim_zones_pu_polygons) - 1)
  expect_equal(
    x$planning_unit_indices(),
    c(seq_len(4), seq(6, nrow(sim_zones_pu_polygons)))
  )
  expect_equal(x$number_of_total_units(), nrow(sim_zones_pu_polygons))
  # tests for planning_unit_costs field
  expect_equivalent(
    x$planning_unit_costs(),
    as.matrix(
      sf::st_drop_geometry(sim_zones_pu_polygons)[-5, paste0("cost_", 1:3)]
    )
  )
  expect_equal(
    colnames(x$planning_unit_costs()),
    zone_names(sim_zones_features)
  )
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    sapply(seq_along(x$data$rij_matrix), function(i) {
      pos1 <- x$planning_unit_indices()
      pos2 <- which(
        !is.na(
          sf::st_drop_geometry(sim_zones_pu_polygons)[[paste0("cost_", i)]]
        )
      )
      pos3 <- match(pos2, pos1)
      Matrix::rowSums(x$data$rij_matrix[[i]][, pos3, drop = FALSE])
    })
  )
  expect_equal(
    colnames(x$feature_abundances_in_planning_units()),
    zone_names(sim_zones_features)
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    feature_names(sim_zones_features)
  )
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    sapply(
      lapply(
        sim_zones_features, terra::extract, terra::vect(sim_zones_pu_polygons),
        "sum", ID = FALSE, na.rm = TRUE
      ),
      colSums,
      na.rm = TRUE
    )
  )
  expect_equal(
    colnames(x$feature_abundances_in_total_units()),
    zone_names(sim_zones_features)
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    feature_names(sim_zones_features)
  )
  # tests for rij_matrix field
  expect_equivalent(
    x$data$rij_matrix,
    lapply(
      sim_zones_features, rij_matrix, x = sim_zones_pu_polygons[-5, ]
    )
  )
  expect_equal(names(x$data$rij_matrix), zone_names(sim_zones_features))
  expect_equivalent(
    sapply(x$data$rij_matrix, rownames),
    matrix(
      feature_names(sim_zones_features),
      ncol = number_of_zones(sim_zones_features),
      nrow = number_of_features(sim_zones_features)
    )
  )
  # test that calling targets before they have been initialized throws error
  expect_tidy_error(x$feature_targets())
})
