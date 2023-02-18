context("problem")

test_that("x = SpatRaster, features = SpatRaster", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  sim_pu_raster <- terra::setValues(
    sim_pu_raster, runif(terra::ncell(sim_pu_raster), -1, 1)
  )
  sim_features[[1]] <-  terra::setValues(
    sim_features[[1]], runif(terra::ncell(sim_features[[1]]), -1, 1)
  )
  expect_warning(x <- problem(sim_pu_raster, sim_features), "negative")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
  suppressMessages(x)
  suppressMessages(x$print())
  suppressMessages(x$show())
  suppressMessages(x$repr())
  # tests for integer fields
  expect_equal(x$feature_names(), names(sim_features))
  expect_equal(x$zone_names(), names(sim_pu_raster))
  expect_equal(x$number_of_features(), terra::nlyr(sim_features))
  expect_equal(
    x$number_of_planning_units(),
    length(terra::cells(is.na(sim_pu_raster), 0)[[1]])
  )
  expect_equal(x$number_of_total_units(), terra::ncell(sim_pu_raster))
  expect_equal(
    x$planning_unit_indices(),
    terra::cells(is.na(sim_pu_raster), 0)[[1]]
  )
  # tests for planning_unit_costs field
  expect_equivalent(
    x$planning_unit_costs(),
    sim_pu_raster[[1]][!is.na(sim_pu_raster)]
  )
  expect_equal(colnames(x$planning_unit_costs()), names(sim_pu_raster))
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    matrix(
      terra::global(
        terra::mask(sim_features, sim_pu_raster), "sum", na.rm = TRUE
      )[[1]]
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
  expect_error(x$feature_targets())
})

test_that("x = SpatRaster, features = ZonesSpatRaster", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  x <- problem(sim_zones_pu_raster, sim_zones_features)
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), feature_names(sim_zones_features))
  expect_equal(x$zone_names(), zone_names(sim_zones_features))
  expect_equal(x$number_of_features(), number_of_features(sim_zones_features))
  expect_equal(x$number_of_zones(), number_of_zones(sim_zones_features))
  expect_equal(
    x$number_of_planning_units(),
    terra::global(max(!is.na(sim_zones_pu_raster)), "sum", na.rm = TRUE)[[1]]
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
  expect_equivalent(x$feature_abundances_in_total_units(),
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
      seq_len(terra::nlyr(sim_zones_pu_raster)),
      function(i) rij_matrix(sim_zones_pu_raster[[i]], sim_zones_features[[i]])
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
  expect_error(x$feature_targets())
})

test_that("x = sf, features = SpatRaster", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  x <- problem(sim_pu_polygons, sim_features, "cost")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
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
        exactextractr::exact_extract(
          sim_features, sim_pu_polygons, "sum", progress = FALSE
        )
      ),
      ncol = 1
    ),
    tolerance = 1e-6
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
        sim_pu_polygons[!is.na(sim_pu_polygons[[1]]), ], sim_features
      )
    )
  )
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), names(sim_features))
  # test that calling targets before they have been initialized throws error
  expect_error(x$feature_targets())
})

test_that("x = sf, features = ZonesSpatRaster", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # update data
  sim_zones_pu_polygons[5, paste0("cost_", 1:3)] <- NA
  # create problem
  x <- problem(sim_zones_pu_polygons, sim_zones_features, paste0("cost_", 1:3))
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
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
        sim_zones_features,
        function(x) {
          terra::extract(
            x, terra::vect(sim_zones_pu_polygons), "sum", na.rm = TRUE,
            ID = FALSE
          )
        }
      ),
      colSums,
      na.rm = TRUE
    ),
    tolerance = 1e-6
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
    lapply(sim_zones_features, rij_matrix,  x = sim_zones_pu_polygons[-5, ])
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
  expect_error(x$feature_targets())
})

test_that("x = sf, features = character", {
  # create data
  sim_pu_polygons <- get_sim_pu_polygons()
  # update data
  sim_pu_polygons$cost[2] <- NA
  sim_pu_polygons$spp1 <- runif(nrow(sim_pu_polygons))
  sim_pu_polygons$spp2 <- c(NA, rpois(nrow(sim_pu_polygons) - 1, 5))
  # create problem
  x <- problem(sim_pu_polygons, c("spp1", "spp2"), "cost")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
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
  expect_error(x$feature_targets())
})

test_that("x = sf, features = ZonesCharacter", {
  # create data
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
  x <- problem(
    sim_zones_pu_polygons,
    zones(
      c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2"),
      zone_names = c("z1", "z2"),
      feature_names = c("spp1", "spp2")),
    c("cost_1", "cost_2")
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
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
      rep(2, nrow(sim_zones_pu_polygons) - 2)
    ),
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
  expect_error(x$feature_targets())
})

test_that("x = data.frame, features = character", {
  # create data
  pu <- data.frame(
    id = seq_len(10), cost = c(runif(1), NA, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  x <- problem(pu, c("spp1", "spp2"), "cost")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
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
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    matrix(colSums(pu[-2, c("spp1", "spp2")], na.rm = TRUE), ncol = 1)
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    c("spp1", "spp2")
  )
  expect_equal(
    colnames(x$feature_abundances_in_planning_units()),
    c("cost")
  )
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    matrix(colSums(pu[, c("spp1", "spp2")], na.rm = TRUE), ncol = 1)
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    c("spp1", "spp2")
  )
  expect_equal(
    colnames(x$feature_abundances_in_total_units()),
    c("cost")
  )
  # tests for rij_matrix field
  expect_true(
    all(
      x$data$rij_matrix[[1]] ==
        Matrix::sparseMatrix(
          i = c(rep(1, 9), rep(2, 8)),
          j = c(1:9, 1:8),
          x = c(pu$spp1[-2], pu$spp2[c(-2, -10)]),
          dims = c(2, 9)
        )
    )
  )
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("spp1", "spp2"))
  # test that calling targets before they have been initialized throws error
  expect_error(x$feature_targets())
})

test_that("x=data.frame, features=ZonesCharacter", {
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)), cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10), spp2_2 = runif(10)
  )
  # create problem
  x <- problem(
    pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
    c("cost_1", "cost_2")
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
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
    matrix(
      c(
        sum(pu$spp1_1[!is.na(pu$cost_1)], na.rm = TRUE),
        sum(pu$spp2_1[!is.na(pu$cost_1)], na.rm = TRUE),
        sum(pu$spp1_2[!is.na(pu$cost_2)], na.rm = TRUE),
        sum(pu$spp2_2[!is.na(pu$cost_2)], na.rm = TRUE)
      ),
      ncol = 4
    )
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    c("1", "2")
  )
  expect_equal(
    colnames(x$feature_abundances_in_planning_units()),
    c("1", "2")
  )
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    matrix(colSums(pu[, 4:7], na.rm = TRUE), ncol = 2)
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    c("1", "2")
  )
  expect_equal(
    colnames(x$feature_abundances_in_total_units()),
    c("1", "2")
  )
  # tests for rij_matrix field
  expect_equal(names(x$data$rij_matrix), c("1", "2"))
  expect_true(
    all(
      x$data$rij_matrix[[1]] ==
        Matrix::sparseMatrix(
          i = c(rep(1, 9), rep(2, 8)),
          j = c(1:9, 1:8),
          x = c(pu$spp1_1[-2], pu$spp2_1[c(-2, -10)]),
          dims = c(2, 9)
        )
    )
  )
  expect_true(
    all(
      x$data$rij_matrix[[2]] ==
        Matrix::sparseMatrix(
          i = c(rep(1, 9), rep(2, 9)),
          j = c(1:9, 1:9),
          x = c(pu$spp1_2[-2], pu$spp2_2[-2]),
          dims = c(2, 9)
        )
    )
  )
  expect_equal(names(x$data$rij_matrix), c("1", "2"))
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("1", "2"))
  expect_equal(rownames(x$data$rij_matrix[[2]]), c("1", "2"))
  # test that calling targets before they have been initialized throws error
  expect_error(x$feature_targets())
})

test_that("x = data.frame, features = data.frame (single zone)", {
  # create data
  pu <- data.frame(id = seq_len(10), cost = c(0.1, NA, runif(8)))
  species <- data.frame(id = seq_len(5), name = letters[1:5], targets = 0.5)
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5))
  rij$amount <- runif(nrow(rij))
  # create problem
  x <- problem(pu, species, rij, "cost")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
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
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    Matrix::rowSums(
      Matrix::sparseMatrix(
        i = rij2[[2]],
        j = rij2[[1]],
        x = rij2[[3]]
      )
    )
  )
  expect_equal(rownames(x$feature_abundances_in_planning_units()), letters[1:5])
  expect_equal(colnames(x$feature_abundances_in_planning_units()), "cost")
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    Matrix::rowSums(
      Matrix::sparseMatrix(
        i = rij[[2]],
        j = rij[[1]],
        x = rij[[3]]
      )
    )
  )
  expect_equal(rownames(x$feature_abundances_in_total_units()), letters[1:5])
  expect_equal(colnames(x$feature_abundances_in_total_units()), "cost")
  # tests for rij_matrix field
  rij2 <- rij[rij$pu != 2, ]
  rij2$pu <- match(rij2$pu, pu$id[-2])
  expect_equivalent(
    x$data$rij_matrix[[1]],
    Matrix::sparseMatrix(
      i = rij2[[2]],
      j = rij2[[1]],
      x = rij2[[3]],
      dims = c(5, 9)
    )
  )
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), letters[1:5])
  # test that calling targets before they have been initialized throws error
  expect_error(x$feature_targets())
})

test_that("x=data.frame, features=data.frame (multiple zones)", {
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(0.1, NA, runif(8)), cost_2 = c(NA, NA, runif(8))
  )
  species <- data.frame(id = seq_len(5), name = letters[1:5], targets = 0.5)
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5), zone = 1:2)
  rij$amount <- runif(nrow(rij))
  z <- data.frame(id = 1:2, name = c("z1", "z2"))
  # create problem
  x <- problem(pu, species, rij, c("cost_1", "cost_2"), z)
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
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
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    matrix(
      aggregate(rij2[[4]], by = list(rij2[[2]], rij2[[3]]), sum)[[3]],
      ncol = 2
    )
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    letters[1:5]
  )
  expect_equal(
    colnames(x$feature_abundances_in_planning_units()),
    c("z1", "z2")
  )
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    matrix(
      aggregate(rij[[4]], by = list(rij[[2]], rij[[3]]), sum)[[3]],
      ncol = 2
    )
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    letters[1:5]
  )
  expect_equal(
    colnames(x$feature_abundances_in_total_units()),
    c("z1", "z2")
  )
  # tests for rij_matrix field
  rij2 <- rij[rij$pu != 2, ]
  rij2$pu <- match(rij2$pu, seq_len(9))
  expect_equal(names(x$data$rij_matrix), c("z1", "z2"))
  expect_equivalent(
    x$data$rij_matrix[[1]],
    Matrix::sparseMatrix(
      i = rij2[[2]][rij2[[3]] == 1],
      j = rij2[[1]][rij2[[3]] == 1],
      x = rij2[[4]][rij2[[3]] == 1],
      dims = c(5, 9)
    )
  )
  expect_equivalent(
    x$data$rij_matrix[[2]],
    Matrix::sparseMatrix(
      i = rij2[[2]][rij2[[3]] == 2],
      j = rij2[[1]][rij2[[3]] == 2],
      x = rij2[[4]][rij2[[3]] == 2],
      dims = c(5, 9)
    )
  )
  expect_equal(rownames(x$data$rij_matrix[[1]]), letters[1:5])
  expect_equal(rownames(x$data$rij_matrix[[2]]), letters[1:5])
  # test that calling targets before they have been initialized throws error
  expect_error(x$feature_targets())
})

test_that("x = numeric, features = data.frame, rij_matrix = matrix", {
  # create data
  pu <- data.frame(
    id = seq_len(10), cost = c(0.2, NA, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  x <- problem(
    pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    as.matrix(t(pu[, 3:4]))
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
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
  expect_equivalent(
    x$planning_unit_costs(),
    matrix(pu$cost[-2], ncol = 1)
  )
  expect_equal(colnames(x$planning_unit_costs()), "1")
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    rowSums(t(pu[-2, 3:4]), na.rm = TRUE)
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    c("spp1", "spp2")
  )
  expect_equal(colnames(x$feature_abundances_in_planning_units()), "1")
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    rowSums(t(pu[, 3:4]), na.rm = TRUE)
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    c("spp1", "spp2")
  )
  expect_equal(colnames(x$feature_abundances_in_total_units()), "1")
  # tests for rij_matrix field
  expect_equal(names(x$data$rij_matrix), "1")
  expect_equivalent(
    x$data$rij_matrix[[1]],
    as(t(pu[-2, 3:4]), "sparseMatrix")
  )
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("spp1", "spp2"))
  # test that calling targets before they have been initialized throws error
  expect_error(x$feature_targets())
})

test_that("x = matrix, features = data.frame, rij_matrix = matrix", {
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)), cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10), spp2_2 = runif(10)
  )
  # create problem
  x <- problem(
    as.matrix(pu[, 2:3]),
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))
  )
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(summary(x))
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
  expect_equivalent(
    x$planning_unit_costs(),
    as.matrix(pu[-2, 2:3])
  )
  expect_equal(colnames(x$planning_unit_costs()), c("1", "2"))
  # tests for feature_abundances_in_planning_units field
  expect_equivalent(
    x$feature_abundances_in_planning_units(),
    matrix(
      c(
        sum(pu$spp1_1[!is.na(pu$cost_1)], na.rm = TRUE),
        sum(pu$spp2_1[!is.na(pu$cost_1)], na.rm = TRUE),
        sum(pu$spp1_2[!is.na(pu$cost_2)], na.rm = TRUE),
        sum(pu$spp2_2[!is.na(pu$cost_2)], na.rm = TRUE)
      ),
      ncol = 4
    )
  )
  expect_equal(
    rownames(x$feature_abundances_in_planning_units()),
    c("spp1", "spp2")
  )
  expect_equal(colnames(x$feature_abundances_in_planning_units()), c("1", "2"))
  # tests for feature_abundances_in_total_units field
  expect_equivalent(
    x$feature_abundances_in_total_units(),
    matrix(colSums(pu[, 4:7], na.rm = TRUE), ncol = 2)
  )
  expect_equal(
    rownames(x$feature_abundances_in_total_units()),
    c("spp1", "spp2")
  )
  expect_equal(colnames(x$feature_abundances_in_total_units()), c("1", "2"))
  # tests for rij_matrix field
  expect_equal(names(x$data$rij_matrix), c("1", "2"))
  expect_equivalent(
    x$data$rij_matrix[[1]],
    as(t(pu[-2, 4:5]), "sparseMatrix")
  )
  expect_equivalent(
    x$data$rij_matrix[[2]],
    as(t(pu[-2, 6:7]), "sparseMatrix")
  )
  expect_equal(rownames(x$data$rij_matrix[[1]]), c("spp1", "spp2"))
  expect_equal(rownames(x$data$rij_matrix[[2]]), c("spp1", "spp2"))
  # test that calling targets before they have been initialized throws error
  expect_error(x$feature_targets())
})

test_that("x = matrix, features = data.frame, rij_matrix = dgCMatrix", {
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)), cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10), spp2_2 = runif(10)
  )
  rij <- list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))
  # create problem
  x1 <- problem(
    as.matrix(pu[, 2:3]),
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    rij_matrix = rij
  )
  x2 <- problem(
    as.matrix(pu[, 2:3]),
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    rij_matrix = lapply(rij, as_Matrix, "dgCMatrix")
  )
  # verify that object can be printed
  suppressMessages(print(x1))
  suppressMessages(x1)
  suppressMessages(print(x2))
  suppressMessages(x2)
  # tests
  expect_equal(x1$data, x2$data)
})

test_that("invalid problem inputs (invalid input class)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # tests
  expect_tidy_error(problem(1, sim_features))
  expect_tidy_error(problem(sim_pu_polygons, sim_pu_polygons, "cost"))
  expect_tidy_error(problem(c(sim_pu_raster, sim_pu_raster), sim_features))
})

test_that("invalid problem inputs (all planning units have NA costs)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update data
  sim_pu_polygons$cost <- NA_real_
  sim_pu_raster <- raster::setValues(sim_pu_raster, NA_real_)
  sim_pu_polygons$spp_1 <- runif(nrow(sim_pu_polygons))
  sim_pu_polygons$spp_2 <- runif(nrow(sim_pu_polygons))
  # tests
  expect_tidy_error(
    problem(sim_pu_raster, sim_features),
    "missing"
  )
  expect_tidy_error(
    problem(sim_pu_polygons, sim_features, "cost"),
    "missing"
  )
  expect_tidy_error(
    problem(sim_pu_polygons, c("spp_1", "spp_2"), "cost"),
    "missing"
  )
  expect_tidy_error(
    problem(sf::st_drop_geometry(sim_pu_polygons), c("spp_1", "spp_2"), "cost"),
    "missing"
  )
})

test_that("problematic problem inputs (planning units with negative values)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update feature data
  sim_pu_raster <- sim_pu_raster * -1
  sim_pu_polygons$cost <- runif(nrow(sim_pu_polygons)) * -1
  sim_pu_polygons$spp_1 <- runif(nrow(sim_pu_polygons))
  sim_pu_polygons$spp_2 <- runif(nrow(sim_pu_polygons))
  # tests
  expect_warning(
    problem(sim_pu_raster, sim_features),
    "has negative values"
  )
  expect_warning(
    problem(sim_pu_polygons, sim_features, "cost"),
    "has negative values"
  )
  expect_warning(
    problem(sim_pu_polygons, c("spp_1", "spp_2"), "cost"),
    "has negative values"
  )
  expect_warning(
    problem(sim_pu_polygons, c("spp_1", "spp_2"), "cost"),
    "has negative values"
  )
  expect_warning(
    problem(sf::st_drop_geometry(sim_pu_polygons), c("spp_1", "spp_2"), "cost"),
    "has negative values"
  )
})

test_that("problematic problem inputs (feature contains negative values)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update feature data
  sim_features <- sim_features * -1
  sim_pu_polygons$cost <- runif(nrow(sim_pu_polygons))
  sim_pu_polygons$spp_1 <- runif(nrow(sim_pu_polygons)) * -1
  sim_pu_polygons$spp_2 <- runif(nrow(sim_pu_polygons))
  # tests
  expect_warning(
    problem(sim_pu_raster, sim_features),
    "has negative values"
  )
  expect_warning(
    problem(sim_pu_polygons, sim_features, "cost"),
    "has negative values"
  )
  expect_warning(
    problem(sim_pu_polygons, c("spp_1", "spp_2"), "cost"),
    "has negative values"
  )
  expect_warning(
    problem(sim_pu_polygons, c("spp_1", "spp_2"), "cost"),
    "has negative values"
  )
  expect_warning(
    problem(sf::st_drop_geometry(sim_pu_polygons), c("spp_1", "spp_2"), "cost"),
    "has negative values"
  )
})

test_that("problematic problem inputs (feature contains only zero values)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update feature data
  sim_features[[1]] <- sim_features[[1]] * 0
  sim_pu_polygons$cost <- runif(nrow(sim_pu_polygons))
  sim_pu_polygons$spp_1 <- 0
  sim_pu_polygons$spp_2 <- runif(nrow(sim_pu_polygons))
  # tests
  expect_warning(
    problem(sim_pu_raster, sim_features),
    "has only zero values"
  )
  expect_warning(
    problem(sim_pu_polygons, sim_features, "cost"),
    "has only zero values"
  )
  expect_warning(
    problem(sim_pu_polygons, c("spp_1", "spp_2"), "cost"),
    "has only zero values"
  )
  expect_warning(
    problem(sim_pu_polygons, c("spp_1", "spp_2"), "cost"),
    "has only zero values"
  )
  expect_warning(
    problem(sf::st_drop_geometry(sim_pu_polygons), c("spp_1", "spp_2"), "cost"),
    "has only zero values"
  )
})

test_that("inheritance", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  p1 <- problem(sim_pu_polygons, sim_features, "cost")
  # create a modified copy of the problem
  p2 <- p1 %>% add_locked_in_constraints("locked_in")
  # test that the original problem remains unchanged
  expect_length(p1$constraints, 0)
  expect_length(p2$constraints, 1)
})
