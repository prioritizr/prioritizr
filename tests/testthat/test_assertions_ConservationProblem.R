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

test_that("all_comparable_problem (single zone, TRUE)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # build problems
  p1 <- problem(sim_pu_raster, sim_features[[1:3]]) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <- problem(sim_pu_raster, sim_features[[4:5]]) %>%
    add_min_shortfall_objective(
      budget = 0.2 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]) %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions()
  # run tests
  expect_true(all_comparable_problem(p1, p2))
  expect_no_failure(multi_problem(p1, p2))
})

test_that(
  "all_comparable_problem (single zone, different planning unit class)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create additional data
  pu2 <- data.frame(
    id = seq(20,29), cost = c(runif(1), NA, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
  )
  # build problems
  p1 <- problem(sim_pu_raster, sim_features[[1:3]]) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <- problem(pu2, c("spp1", "spp2"), "cost")
  # run tests
  expect_false(all_comparable_problem(p1, p2))
  expect_error(
    assert(all_comparable_problem(p1, p2)),
    "planning unit classes"
  )
})

test_that(
  "all_comparable_problem (single zone, different planning unit ids)", {
  # create data
  ## note that planning unit ids mismatch
  pu <- data.frame(
    id = seq_len(10), cost = c(runif(1), NA, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
  )
  pu2 <- data.frame(
    id = seq(20,28), cost = c(runif(1), NA, runif(7)),
    spp1 = runif(9), spp2 = c(rpois(8, 4), NA)
  )
  # create problems
  p1 <- problem(pu, c("spp1", "spp2"), "cost")
  p2 <- problem(pu2, c("spp1", "spp2"), "cost")
  # run tests
  expect_false(all_comparable_problem(p1, p2))
  expect_error(
    assert(all_comparable_problem(p1, p2)),
    "planning unit indices"
  )
})

test_that(
  "all_comparable_problem (multiple zones, TRUE)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # build problems
  p1 <- problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
    add_binary_decisions()
  p2 <- problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
    add_binary_decisions()
  # run tests
  expect_true(all_comparable_problem(p1, p2))
  expect_no_failure(assert(all_comparable_problem(p1, p2)))
})

test_that(
  "all_comparable_problem (multiple zones, different planning unit class)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create additional data
  pu2 <- data.frame(
    id = seq_len(9),
    cost_1 = c(NA, NA, runif(7)), cost_2 = c(0.3, NA, runif(7)),
    spp1_1 = runif(9), spp2_1 = c(rpois(8, 4), NA),
    spp1_2 = runif(9), spp2_2 = runif(9)
  )
  # build problems
  p1 <- problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
    add_binary_decisions()
  p2 <- problem(
    pu2, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
    c("cost_1", "cost_2")
  )
  # run tests
  expect_false(all_comparable_problem(p1, p2))
  expect_error(
    all_comparable_problem(p1, p2),
    "planning unit class"
  )
})

test_that("all_comparable_problem (multiple zones, different zone names)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create additional data
  sim_zones_features2 <- sim_zones_features1
  attr(sim_zones_features2, "zone_names") <- c("zone1", "zone2", "zone3")
  # build problems
  p1 <- problem(sim_zones_pu_raster, sim_zones_features1) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
    add_binary_decisions()
  p2 <- problem(sim_zones_pu_raster, sim_zones_features2) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
    add_binary_decisions()
  # run tests
  expect_false(all_comparable_problem(p1, p2))
  expect_error(
    assert(all_comparable_problem(p1, p2)),
    "zone names"
  )
})

test_that(
  "all_comparable_problem (multiple zones, different planning unit ids)", {
  # create data
  pu1 <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)), cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10), spp2_2 = runif(10)
  )
  pu2 <- data.frame(
    id = seq_len(9),
    cost_1 = c(NA, NA, runif(7)), cost_2 = c(0.3, NA, runif(7)),
    spp1_1 = runif(9), spp2_1 = c(rpois(8, 4), NA),
    spp1_2 = runif(9), spp2_2 = runif(9)
  )
  # create problems
  p1 <- problem(
    pu1, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
    c("cost_1", "cost_2")
  )
  p2 <- problem(
    pu2, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
    c("cost_1", "cost_2")
  )
  # run tests
  expect_false(all_comparable_problem(p1, p2))
  expect_error(
    assert(all_comparable_problem(p1, p2)),
    "planning unit indices"
  )
})
