test_that("raster features (linear)", {
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
      method = interpolated_area_targets(
        rare_area_threshold = 3000,
        rare_relative_target = 0.4,
        rare_area_target = 2000,
        rare_method = "max",
        common_area_threshold = 5000,
        common_relative_target = 0.1,
        common_area_target = 6000,
        common_method = "min",
        cap_area_target = 7000,
        interp_method = "linear",
        area_units = "km^2"
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## run interpolation with relative targets
  correct_targets <- fs * linear_interpolation(fs, 3000, 0.4, 5000, 0.1)
  ## apply absolute targets
  idx <- fs < 3000
  correct_targets[idx] <- pmax(correct_targets[idx], 2000)
  idx <- fs > 5000
  correct_targets[idx] <- pmin(correct_targets[idx], 6000)
  ## apply target caps
  correct_targets <- pmin(correct_targets, 7000)
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

test_that("raster features (loglinear)", {
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
      method = interpolated_area_targets(
        rare_area_threshold = 3000,
        rare_relative_target = 0.4,
        rare_area_target = 2000,
        rare_method = "max",
        common_area_threshold = 5000,
        common_relative_target = 0.1,
        common_area_target = 6000,
        common_method = "min",
        cap_area_target = 7000,
        interp_method = "log10",
        area_units = "km^2"
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## run interpolation with relative targets
  correct_targets <- fs * loglinear_interpolation(fs, 3000, 0.4, 5000, 0.1)
  ## apply absolute targets
  idx <- fs < 3000
  correct_targets[idx] <- pmax(correct_targets[idx], 2000)
  idx <- fs > 5000
  correct_targets[idx] <- pmin(correct_targets[idx], 6000)
  ## apply target caps
  correct_targets <- pmin(correct_targets, 7000)
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

test_that("non-raster features", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_data <- terra::as.data.frame(c(sim_pu_raster, sim_features))
  names(sim_data) <-
    c("cost", paste0("ft_", seq_len(terra::nlyr(sim_features))))
  sim_data$id <- seq_len(nrow(sim_data))
  # create problem
  p <- problem(
    sim_data,
    paste0("ft_", seq_len(terra::nlyr(sim_features))),
    cost_column = "cost",
    feature_units = rep("km^2", terra::nlyr(sim_features))
  )
  # compute values in km^2
  fs <- p$feature_abundances_km2_in_total_units()
  # compute values in absolute units
  fa <- p$feature_abundances_in_total_units()
  # create problem
  p <-
    p %>%
    add_auto_targets(
      method = interpolated_area_targets(
        rare_area_threshold = 40,
        rare_relative_target = 0.4,
        rare_area_target = 20,
        rare_method = "max",
        common_area_threshold = 55,
        common_relative_target = 0.1,
        common_area_target = 30,
        common_method = "min",
        cap_area_target = 60,
        interp_method = "linear",
        area_units = "km^2"
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## run interpolation with relative targets
  correct_targets <- fs * linear_interpolation(fs, 40, 0.4, 55, 0.1)
  ## apply absolute targets
  idx <- fs < 40
  correct_targets[idx] <- pmax(correct_targets[idx], 20)
  idx <- fs > 55
  correct_targets[idx] <- pmin(correct_targets[idx], 30)
  ## apply target caps
  correct_targets <- pmin(correct_targets, 60)
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

test_that("add_interpolated_area_targets()", {
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
  # create problem with interpolated_area_targets()
  p1 <-
    p %>%
    add_auto_targets(
      method = interpolated_area_targets(
        rare_area_threshold = 30,
        rare_relative_target = 0.4,
        rare_area_target = 2000,
        rare_method = "max",
        common_area_threshold = 50,
        common_relative_target = 0.1,
        common_area_target = 70,
        common_method = "min",
        cap_area_target = 7000,
        interp_method = "log10",
        area_units = "km^2"
      )
    )
  # create problem with add_interpolated_area_targets()
  p2 <-
    p %>%
    add_interpolated_area_targets(
      rare_area_threshold = 30,
      rare_relative_target = 0.4,
      rare_area_target = 2000,
      rare_method = "max",
      common_area_threshold = 50,
      common_relative_target = 0.1,
      common_area_target = 70,
      common_method = "min",
      cap_area_target = 7000,
      interp_method = "log10",
      area_units = "km^2"
    )
  # run tests
  expect_equal(
    p1$targets$output(p),
    p2$targets$output(p)
  )
})

test_that("invalid inputs", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 1e5, 0, 1e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective()
  # run tests
  ## rare_area_threshold
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      NA_real_, 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "missing"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      "a", 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      -1, 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "0"
  )
  ## rare_relative_target
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, NA_real_, 2000, "max",
       5000, 0.1, 6000, "min",
       7000, "linear", "km^2"
    ),
    "missing"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, "a", 2000, "max",
      5000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, -0.1, 2000, "max",
      5000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "between"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 1.1, 2000, "max",
      5000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "0"
  )
  ## rare_area_target
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, Inf, "max",
      5000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "finite"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, "a", "max",
      5000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, -1, "max",
      6000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "0"
  )
  ## rare_method
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "a",
      5000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "min"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, 1,
      5000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "string"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, NA_character_,
      5000, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "missing"
  )
  ## common_area_threshold
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
       NA_real_, 0.1, 6000, "min",
       7000, "linear", "km^2"
    ),
    "missing"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      "a", 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      -1, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "0"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      1000, 0.4, 2000, "max",
      500, 0.1, 6000, "min",
      7000, "linear", "km^2"
    ),
    "less than or equal to"
  )
  ## common_relative_target
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, NA_real_, 6000, "min",
      7000, "linear", "km^2"
    ),
    "missing"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, "a", 6000, "min",
       7000, "linear", "km^2"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 1.1, 6000, "min",
       7000, "linear", "km^2"
    ),
    "between"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, -0.2, 6000, "min",
      7000, "linear", "km^2"
    ),
    "between"
  )
  ## common_area_target
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, Inf, "min",
       7000, "linear", "km^2"
    ),
    "finite"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, "a", "min",
       7000, "linear", "km^2"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, -1, "min",
      7000, "linear", "km^2"
    ),
    "0"
  )
  ## common_method
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, "greg",
      7000, "linear", "km^2"
    ),
    "min"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, 1,
      7000, "linear", "km^2"
    ),
    "string"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, NA_character_,
      7000, "linear", "km^2"
    ),
    "missing"
  )
  ## cap_area_target
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      Inf, "linear", "km^2"
    ),
    "finite"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      "a", "linear", "km^2"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      -1, "linear", "km^2"
    ),
    "0"
  )
  ## interp_method
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      7000, "greg", "km^2"
    ),
    "linear"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      7000, 1, "km^2"
    ),
    "string"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      7000, NA_character_, "km^2"
    ),
    "missing"
  )
  ## area_units
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      7000, "linear", 1
    ),
    "area"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      7000, "linear", NA_character_
    ),
    "area"
  )
  expect_tidy_error(
    add_interpolated_area_targets(
      p,
      4000, 0.4, 2000, "max",
      5000, 0.1, 6000, "min",
      7000, "linear", "m"
    ),
    "area"
  )
})
