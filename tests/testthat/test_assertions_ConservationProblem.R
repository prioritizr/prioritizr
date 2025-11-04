test_that("all_is_valid_total_unit_ids (data.frame)", {
  # import data
  sim_pu_data <- get_sim_pu_polygons()
  sim_pu_data <- sf::st_drop_geometry(sim_pu_data)[1:5, , drop = FALSE]
  sim_pu_data$id <- c(1, 3, 90, 5, 2)
  sim_pu_data$cost <- c(1, NA, 3, 4, 8)
  sim_pu_data$spp_1 <- runif(5)
  sim_pu_data$spp_2 <- runif(5)
  sim_pu_data$spp_3 <- runif(5)
  # create problem
  p <- problem(sim_pu_data, c("spp_1", "spp_2", "spp_3"), cost_column = "cost")
  # tests
  expect_true(all_is_valid_total_unit_ids(p, 3))
  expect_true(all_is_valid_total_unit_ids(p, 90))
  expect_true(all_is_valid_total_unit_ids(p, c(3, 90)))
  expect_false(all_is_valid_total_unit_ids(p, 1000))
  expect_false(all_is_valid_total_unit_ids(p, c(3, 90, 1000)))
  expect_false(all_is_valid_total_unit_ids(p, c(3, 90, NA_real_)))
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "valid identifiers"
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, c(3, 90, 1000, runif(10)))),
    "valid identifiers"
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, c(3, 90, NA_real_))),
    "valid identifiers"
  )
})

test_that("all_is_valid_total_unit_ids (sf)", {
  # import data
  sim_pu_data <- get_sim_pu_polygons()
  sim_pu_data <- sim_pu_data[1:5, , drop = FALSE]
  sim_pu_data$cost <- c(1, NA, 3, 4, 8)
  sim_pu_data$spp_1 <- runif(5)
  sim_pu_data$spp_2 <- runif(5)
  sim_pu_data$spp_3 <- runif(5)
  # create problem
  p <- problem(sim_pu_data, c("spp_1", "spp_2", "spp_3"), cost_column = "cost")
  # tests
  expect_true(all_is_valid_total_unit_ids(p, 1))
  expect_true(all_is_valid_total_unit_ids(p, 2))
  expect_true(all_is_valid_total_unit_ids(p, c(1, 2)))
  expect_false(all_is_valid_total_unit_ids(p, 1000))
  expect_false(all_is_valid_total_unit_ids(p, c(1, 2, 1000)))
  expect_false(all_is_valid_total_unit_ids(p, c(1, 2, NA_real_)))
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "valid identifiers"
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, c(1, 2, 1000, runif(10)))),
    "valid identifiers"
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, c(1, 2, NA_real_))),
    "valid identifiers"
  )
})

test_that("all_is_valid_total_unit_ids (assorted data types)", {
  # import data
  sim_pu_data <- get_sim_pu_polygons()
  sim_pu_data <- sim_pu_data[1:5, , drop = FALSE]
  sim_pu_data$id <- seq_len(5)
  sim_pu_data$spp_1 <- runif(5)
  sim_pu_data$spp_2 <- runif(5)
  # tests
  ## sf
  p <- problem(sim_pu_data, c("spp_1", "spp_2"), cost_column = "cost")
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "row numbers"
  )
  ## Spatial
  p <- suppressWarnings(problem(
    sf::as_Spatial(sim_pu_data),
    c("spp_1", "spp_2"),
    cost_column = "cost"
  ))
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "row numbers"
  )
  ## data.frame
  p <- problem(
    sf::st_drop_geometry(sim_pu_data),
    c("spp_1", "spp_2"),
    cost_column = "cost"
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "column"
  )
  ## matrix
  p <- problem(
    matrix(sim_pu_data$cost, ncol = 1),
    data.frame(id = seq_len(2), name = c("spp_1", "spp_2")),
    as.matrix(t(sf::st_drop_geometry(sim_pu_data[, c("spp_1", "spp_2")])))
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "row numbers"
  )
  ## numeric
  suppressMessages(
    p <- problem(
      sim_pu_data$cost,
      data.frame(id = seq_len(2), name = c("spp_1", "spp_2")),
      as.matrix(t(sf::st_drop_geometry(sim_pu_data[, c("spp_1", "spp_2")])))
    )
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "row numbers"
  )
})

test_that(
  "assert_can_calculate_area_based_targets (raster features, geodetic)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  terra::crs(sim_pu_raster) <- terra::crs("epsg:4326")
  terra::crs(sim_features) <- terra::crs("epsg:4326")
  # build message
  msg <- try(
    problem(sim_pu_raster, sim_features) |>
    add_auto_targets(spec_jung_targets()) |>
    add_min_set_objective() |>
    add_binary_decisions(),
    silent = TRUE
  )
  # run tests
  expect_true(grepl("add_auto_targets", msg, fixed = TRUE))
  expect_true(grepl("jung_targets", msg, fixed = TRUE))
  expect_true(grepl("geodetic", msg, fixed = TRUE))
})

test_that(
  "assert_can_calculate_area_based_targets (raster features, NA crs)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # build message
  msg <- try(
    problem(sim_pu_raster, sim_features) |>
    add_auto_targets(spec_jung_targets()) |>
    add_min_set_objective() |>
    add_binary_decisions(),
    silent = TRUE
  )
  # run tests
  expect_true(grepl("add_auto_targets", msg, fixed = TRUE))
  expect_true(grepl("jung_targets", msg, fixed = TRUE))
  expect_true(grepl("defined", msg, fixed = TRUE))
})

test_that(
  "assert_can_calculate_area_based_targets (non-raster features, NA crs)", {
  # load data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_pu_polygons$spp_1 <- 1
  sim_pu_polygons$spp_2 <- 2
  # build message
  msg <- try(
    problem(
      sim_pu_polygons, c("spp_1", "spp_2"), cost_column = "cost",
      feature_units = c("km^2", NA)
    ) |>
    add_auto_targets(spec_jung_targets()) |>
    add_min_set_objective() |>
    add_binary_decisions(),
    silent = TRUE
  )
  # run tests
  expect_true(grepl("add_auto_targets", msg, fixed = TRUE))
  expect_true(grepl("jung_targets", msg, fixed = TRUE))
  expect_true(grepl("spp_2", msg, fixed = TRUE))
})

test_that("is_pu_spatially_explicit (raster data)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # build problem
  p <- problem(sim_pu_raster, sim_features)
  # tests
  expect_true(is_pu_spatially_explicit(p))
})

test_that("is_pu_spatially_explicit (sf data)", {
  # load data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_pu_polygons$spp_1 <- 1
  sim_pu_polygons$spp_2 <- 2
  # build message
  p <- problem(sim_pu_polygons, c("spp_1", "spp_2"), cost_column = "cost")
  # tests
  expect_true(is_pu_spatially_explicit(p))
})

test_that("is_pu_spatially_explicit (data.frame data)", {
  # load data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_pu_polygons$id <- seq_len(nrow(sim_pu_polygons))
  sim_pu_polygons$spp_1 <- 1
  sim_pu_polygons$spp_2 <- 2
  # build message
  p <- problem(
    sf::st_drop_geometry(sim_pu_polygons),
    c("spp_1", "spp_2"),
    cost_column = "cost"
  )
  # tests
  expect_false(is_pu_spatially_explicit(p))
  expect_error(assert(is_pu_spatially_explicit(p)), "spatially explicit")
})

test_that("is_pu_comparable_raster", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features)
  # tests
  expect_true(is_pu_comparable_raster(p, sim_pu_raster))
  expect_error(
    {
      sim_pu_raster <- get_sim_pu_raster()
      terra::crs(sim_pu_raster) <- as.character(sf::st_crs(4326))[[2]]
      assert(is_pu_comparable_raster(p, sim_pu_raster))
    },
    "comparable"
  )
  expect_error(
    {
      sim_pu_raster <- get_sim_pu_raster()
      sim_pu_raster <- terra::crop(
        sim_pu_raster, terra::ext(0.1, 0.5, 0.1, 0.5)
      )
      assert(is_pu_comparable_raster(p, sim_pu_raster))
    },
    "comparable"
  )
  expect_error(is_pu_comparable_raster(sim_pu_raster, "a"))
})

test_that("has_single_zone (single zone problem)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # tests
  expect_true(has_single_zone(p))
})

test_that("has_single_zone (multiple zones problem)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  # tests
  expect_false(has_single_zone(p))
  expect_error(assert(has_single_zone(p)), "single zone")
})
