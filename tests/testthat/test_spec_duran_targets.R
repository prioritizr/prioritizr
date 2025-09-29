test_that("raster features", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 2e5, 0, 2e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # compute values in km^2
  fs <- p$feature_abundances_km2_in_total_units()
  # compute values in absolute units
  fa <- p$feature_abundances_in_total_units()
  # define parameters
  prob_target <- runif(terra::nlyr(sim_features))
  hist_area <- fs * runif(terra::nlyr(sim_features), 1, 3)
  # create problem
  p <-
    p %>%
    add_auto_targets(
      method = spec_duran_targets(
        probability_target = prob_target,
        historical_area = hist_area,
        area_units = "km2"
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## set criteria
  correct_targets <- hist_area * (prob_target^(1 / 0.25))
  correct_targets <- pmin(correct_targets, fs)
  ## calculate targets in absolute units
  correct_targets <- fa * (correct_targets / fs)
  # run tests
  print(p)
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, seq_len(terra::nlyr(sim_features)))
  expect_equal(unlist(targets$zone), rep(1, terra::nlyr(sim_features)))
  expect_equal(targets$value, c(correct_targets))
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("invalid inputs", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # enlarge spatial extent
  terra::ext(sim_pu_raster) <- c(
    0, nrow(sim_pu_raster) * 1000,
    0, ncol(sim_pu_raster) * 1000
  )
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective()
  # run tests
  ## prob_target
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_duran_targets(
        probability_target = c(0.1, NA, 0.3, 0.4, 0.9),
        historical_area = c(100, 300, 400, 500, 20),
        area_units = "km2"
      )
    ),
    "missing"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_duran_targets(
        probability_target = c(0.1, 0.3, 0.4, 0.9),
        historical_area = c(100, 300, 400, 500, 20),
        area_units = "km2"
      )
    ),
    "length"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_duran_targets(
        probability_target = c(0.1, -0.2, 0.3, 0.4, 0.9),
        historical_area = c(100, 300, 400, 500, 20),
        area_units = "km2"
      )
    ),
    "1"
  )
  ## historical_area
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_duran_targets(
        probability_target = c(0.1, 0.2, 0.3, 0.4, 0.9),
        historical_area = c(100, NA, 400, 500, 20),
        area_units = "km2"
      )
    ),
    "missing"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_duran_targets(
        probability_target = c(0.1, 0.2, 0.3, 0.4, 0.9),
        historical_area = c(100, 400, 500, 20),
        area_units = "km2"
      )
    ),
    "length"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_duran_targets(
        probability_target = c(0.1, 0.2, 0.3, 0.4, 0.9),
        historical_area = c(100, -3000, 400, 500, 20),
        area_units = "km2"
      )
    ),
    "negative"
  )
})
