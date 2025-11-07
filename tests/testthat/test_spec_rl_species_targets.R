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
      method = spec_rl_species_targets(
        status = "CR",
        criterion_a = "A2",
        criterion_b = "B1",
        prop_uplift = 0.15,
        method = "max",
        cap_area_target = 7000,
        area_units = "km^2"
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## set criteria
  pu <- 0.15
  bc <- (1 - 0.8) * (1 + pu)
  ac <- 100 * (1 + pu)
  correct_targets <- pmin(pmin(pmax(ac, fs * bc), 7000), fs)
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
      method = spec_rl_species_targets(
        status = "CR",
        criterion_a = "A2",
        criterion_b = "B1",
        prop_uplift = 0.15,
        method = "min",
        cap_area_target = NA,
        area_units = "km^2"
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## set criteria
  pu <- 0.15
  bc <- (1 - 0.8) * (1 + pu)
  ac <- 100 * (1 + pu)
  correct_targets <- pmin(pmin(pmin(ac, fs * bc)), fs)
  ## calculate targets in absolute units
  correct_targets <- fa * (correct_targets / fs)
  # run tests
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
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 1e5, 0, 1e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # run tests
  ## status
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        NA, "A2", "B2", 0.15, "max", 7000, "km^2"
      )
    ) %>%
    compile(),
    "string"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        NA_character_, "A2", "B2", 0.15, "max", 7000, "km^2"
      )
    ) %>%
    compile(),
    "missing"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "greg", "A2", "B2", 0.15, "max", 7000, "km^2"
      )
    ) %>%
    compile(),
    "must"
  )
  ## criterion_a
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", NA, "B2", 0.15, "max", 7000, "km^2"
      )
    ) %>%
    compile(),
    "string"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", NA_character_, "B2", 0.15, "max", 7000, "km^2"
      )
    ) %>%
    compile(),
    "missing"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "greg", "B2", 0.15, "max", 7000, "km^2"
      )
    ) %>%
    compile(),
    "must be"
  )
  ## criterion_b
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", NA, 0.15, "max", 7000, "km^2"
      )
    ) %>%
    compile(),
    "string"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", NA_character_, 0.15, "max", 7000, "km^2"
      )
    ) %>%
    compile(),
    "missing"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "greg", 0.15, "max", 7000, "km^2"
      )
    ) %>%
    compile(),
    "must"
  )
  ## prop_uplift
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "B2", -1, "max", "greg", "km^2"
      )
    ) %>%
    compile(),
    "0"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "B2", NA_real_, "max", -1, "km^2"
      )
    ) %>%
    compile(),
    "missing"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "B2", "greg", "max", -1, "km^2"
      )
    ) %>%
    compile(),
    "number"
  )
  ## method
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "B2", 0.15, "greg", 7000, "km^2"
      )
    ) %>%
    compile(),
    "must"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "B2", 0.15, 1, 7000, "km^2"
      )
    ) %>%
    compile(),
    "string"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "B2", 0.15, NA_character_, 7000, "km^2"
      )
    ) %>%
    compile(),
    "missing"
  )
  ## cap_area_target
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "B2", 0.15, "max", -1, "km^2"
      )
    ) %>%
    compile(),
    "0"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "B2", 0.15, "max", "greg", "km^2"
      )
    ) %>%
    compile(),
    "number"
  )
  ## area_units
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "B2", 0.15, "max", 7000, NA_character_
      )
    ) %>%
    compile(),
    "unit"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "B2", 0.15, "max", 7000, 1
      )
    ) %>%
    compile(),
    "unit"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_rl_species_targets(
        "CR", "A2", "B2", 0.15, "max", 7000, "greg"
      )
    ) %>%
    compile(),
    "unit"
  )
})
