test_that("raster features (linear)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # enlarge spatial extent
  terra::ext(sim_pu_raster) <- c(0, 1e5, 0, 1e5)
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # compute values in km^2
  fa <- p$feature_abundances_in_total_units()
  # create problem
  p <-
    p %>%
    add_auto_targets(
      method = interpolated_absolute_targets(
        rare_absolute_threshold = 30,
        rare_relative_target = 0.4,
        rare_absolute_target = 20,
        rare_method = "max",
        common_absolute_threshold = 50,
        common_relative_target = 0.1,
        common_absolute_target = 60,
        common_method = "min",
        cap_absolute_target = 70,
        interp_method = "linear"
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## run interpolation with relative targets
  correct_targets <- c(fa) * linear_interpolation(fa, 30, 0.4, 50, 0.1)
  ## apply absolute targets
  idx <- fa < 30
  correct_targets[idx] <- pmax(correct_targets[idx], 20)
  idx <- fa > 50
  correct_targets[idx] <- pmin(correct_targets[idx], 60)
  ## apply target caps
  correct_targets <- pmin(correct_targets, 70)
  correct_targets <- pmin(correct_targets, fa)
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
  expect_equal(targets$value, correct_targets)
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("raster features (loglinear)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # enlarge spatial extent
  terra::ext(sim_pu_raster) <- c(0, 1e5, 0, 1e5)
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # compute values in km^2
  fa <- p$feature_abundances_in_total_units()
  # create problem
  p <-
    p %>%
    add_auto_targets(
      method = interpolated_absolute_targets(
        rare_absolute_threshold = 30,
        rare_relative_target = 0.4,
        rare_absolute_target = 20,
        rare_method = "max",
        common_absolute_threshold = 50,
        common_relative_target = 0.1,
        common_absolute_target = 60,
        common_method = "min",
        cap_absolute_target = 70,
        interp_method = "log10"
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## run interpolation with relative targets
  correct_targets <- c(fa) * loglinear_interpolation(fa, 30, 0.4, 50, 0.1)
  ## apply absolute targets
  idx <- fa < 30
  correct_targets[idx] <- pmax(correct_targets[idx], 20)
  idx <- fa > 50
  correct_targets[idx] <- pmin(correct_targets[idx], 60)
  ## apply target caps
  correct_targets <- pmin(correct_targets, 7000)
  correct_targets <- pmin(correct_targets, fa)
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
  expect_equal(targets$value, correct_targets)
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("non-raster features (linear)", {
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
    cost_column = "cost"
  )
  # compute values in km^2
  fa <- c(p$feature_abundances_in_total_units())
  # create problem
  p <-
    p %>%
    add_auto_targets(
      method = interpolated_absolute_targets(
        rare_absolute_threshold = 40,
        rare_relative_target = 0.4,
        rare_absolute_target = 20,
        rare_method = "max",
        common_absolute_threshold = 55,
        common_relative_target = 0.1,
        common_absolute_target = 30,
        common_method = "min",
        cap_absolute_target = 60,
        interp_method = "linear"
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## run interpolation with relative targets
  correct_targets <- c(fa) * linear_interpolation(fa, 40, 0.4, 55, 0.1)
  ## apply absolute targets
  idx <- fa < 40
  correct_targets[idx] <- pmax(correct_targets[idx], 20)
  idx <- fa > 55
  correct_targets[idx] <- pmin(correct_targets[idx], 30)
  ## apply target caps
  correct_targets <- pmin(correct_targets, 60)
  correct_targets <- pmin(correct_targets, fa)
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
  expect_equal(targets$value, correct_targets)
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("add_interpolated_absolute_targets", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # enlarge spatial extent
  terra::ext(sim_pu_raster) <- c(0, 1e5, 0, 1e5)
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # create problem with interpolated_absolute_targets()
  p1 <-
    p %>%
    add_auto_targets(
      method = interpolated_absolute_targets(
        rare_absolute_threshold = 30,
        rare_relative_target = 0.4,
        rare_absolute_target = 2000,
        rare_method = "max",
        common_absolute_threshold = 50,
        common_relative_target = 0.1,
        common_absolute_target = 70,
        common_method = "min",
        cap_absolute_target = 7000,
        interp_method = "log10"
      )
    )
  # create problem with add_interpolated_absolute_targets()
  p2 <-
    p %>%
    add_interpolated_absolute_targets(
      rare_absolute_threshold = 30,
      rare_relative_target = 0.4,
      rare_absolute_target = 2000,
      rare_method = "max",
      common_absolute_threshold = 50,
      common_relative_target = 0.1,
      common_absolute_target = 70,
      common_method = "min",
      cap_absolute_target = 7000,
      interp_method = "log10"
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
  # enlarge spatial extent
  terra::ext(sim_pu_raster) <- c(0, 1e5, 0, 1e5)
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective()
  # run tests
  ## rare_threshold
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, NA_real_, 0.4, 2000, "max", 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "missing"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, "a", 0.4, 2000, "max", 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, -1, 0.4, 2000, "max", 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "0"
  )
  ## rare_relative_target
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, NA_real_, 2000, "max", 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "missing"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, "a", 2000, "max", 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, -0.1, 2000, "max", 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "between"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 1.1, 2000, "max", 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "0"
  )
  ## rare_absolute_target
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, Inf, "max", 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "finite"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, "a", "max", 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, -1, "max", 6000, 0.1, 6000, "min", 7000, "linear"
    ),
    "0"
  )
  ## rare_method
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "a", 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "min"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, 1, 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "string"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, NA_character_, 5000, 0.1, 6000, "min", 7000, "linear"
    ),
    "missing"
  )
  ## common_threshold
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", NA_real_, 0.1, 6000, "min", 7000, "linear"
    ),
    "missing"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", "a", 0.1, 6000, "min", 7000, "linear"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", -1, 0.1, 6000, "min", 7000, "linear"
    ),
    "0"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 1000, 0.4, 2000, "max", 500, 0.1, 6000, "min", 7000, "linear"
    ),
    "less than or equal to"
  )
  ## common_relative_target
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, NA_real_, 6000, "min", 7000, "linear"
    ),
    "missing"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, "a", 6000, "min", 7000, "linear"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 1.1, 6000, "min", 7000, "linear"
    ),
    "between"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, -0.2, 6000, "min", 7000, "linear"
    ),
    "between"
  )
  ## common_absolute_target
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, Inf, "min", 7000, "linear"
    ),
    "finite"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, "a", "min", 7000, "linear"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, -1, "min", 7000, "linear"
    ),
    "0"
  )
  ## common_method
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, 6000, "greg", 7000, "linear"
    ),
    "min"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, 6000, 1, 7000, "linear"
    ),
    "string"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, 6000, NA_character_, 7000, "linear"
    ),
    "missing"
  )
  ## cap_threshold
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, 6000, "min", Inf, "linear"
    ),
    "finite"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, 6000, "min", "a", "linear"
    ),
    "number"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, 6000, "min", -1, "linear"
    ),
    "0"
  )
  ## interp_method
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, 6000, "min", 7000, "greg"
    ),
    "linear"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, 6000, "min", 7000, 1
    ),
    "string"
  )
  expect_tidy_error(
    add_interpolated_absolute_targets(
      p, 4000, 0.4, 2000, "max", 5000, 0.1, 6000, "min", 7000, NA_character_
    ),
    "missing"
  )
})
