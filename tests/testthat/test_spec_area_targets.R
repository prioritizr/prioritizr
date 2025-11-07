test_that("raster features", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 1e5, 0, 1e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # compute values in km^2
  fs <- p$feature_abundances_km2_in_total_units()
  # compute values in absolute units
  fa <- p$feature_abundances_in_total_units()
  # create problem
  p <-
    p %>%
    add_auto_targets(
      method = spec_area_targets(
        targets = c(1, 2, 5, 10, 100),
        area_units = c("km2", "km2", "km2", "km2", "ha")
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## calculate targets
  correct_targets <- as_km2(
    c(1, 2, 5, 10, 100),
    c("km2", "km2", "km2", "km2", "ha")
  )
  ## apply target caps
  correct_targets <- pmin(correct_targets, fs)
  ## calculate targets in absolute units
  correct_targets <- c(fa) * (correct_targets / fs)
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

test_that("single value arguments", {
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
  p0 <- problem(sim_pu_raster, sim_features)
  # create problems
  p1 <-
    p0 %>%
    add_auto_targets(
      method = spec_area_targets(
        targets = rep(5, 5),
        area_units = rep("km2", 5)
      )
    )
  p2 <-
    p0 %>%
    add_auto_targets(
      method = spec_area_targets(
        targets = rep(5, 1),
        area_units = rep("km2", 1)
      )
    )
  # run tests
  expect_equal(p1$targets$output(p1), p2$targets$output(p2))
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
  ## targets
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_area_targets(
        targets = c(1, NA, 5, 10, 100),
        area_units = c("km2", "km2", "km2", "km2", "ha")
      )
    ),
    "missing"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_area_targets(
        targets = c(1, -2, 5, 10, 100),
        area_units = c("km2", "km2", "km2", "km2", "ha")
      )
    ),
    "negative"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_area_targets(
        targets = letters[5],
        area_units = c("km2", "km2", "km2", "km2", "ha")
      )
    ),
    "numeric"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_area_targets(
        targets = c(1, 2, 5, 10),
        area_units = c("km2", "km2", "km2", "km2", "ha")
      )
    ),
    "length"
  )
  ## area_units
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_area_targets(
        targets = c(1, 2, 5, 10, 100),
        area_units = c("km2", NA, "km2", "km2", "ha")
      )
    ),
    "unit"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_area_targets(
        targets = c(1, 2, 5, 10, 100),
        area_units = c("km2", "km2", "km2", "ha")
      )
    ),
    "length"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_area_targets(
        targets = c(1, 2, 5, 10, 100),
        area_units = c("km2", "g", "km2", "km2", "ha")
      )
    ),
    "area"
  )
})
