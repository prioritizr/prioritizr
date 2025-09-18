test_that("raster features", {
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
  p <- problem(sim_pu_raster, sim_features)
  # compute values in km^2
  fs <- p$feature_abundances_km2_in_total_units()
  # compute values in absolute units
  fa <- p$feature_abundances_in_total_units()
  # create problem
  p <-
    p %>%
    add_auto_targets(
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = c(1, 5, 1, 4, 1),
        density_units = "km2",
        cap_area_target = 40,
        area_units = "km2"
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  targets
  # calculate correct targets
  ## run interpolation with relative targets
  correct_targets <- c(10, 10, 1, 20, 50) / c(1, 5, 1, 4, 1)
  ## apply target caps
  correct_targets <- pmin(correct_targets, 40)
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
  ## pop_size_targets
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, NA, 1, 20, 50),
        pop_density = c(1, 5, 1, 4, 1),
        density_units = "km2",
        cap_area_target = 40,
        area_units = "km2"
      )
    ),
    "missing"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 1, 20, 50),
        pop_density = c(1, 5, 1, 4, 1),
        density_units = "km2",
        cap_area_target = 40,
        area_units = "km2"
      )
    ),
    "length"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = letters[seq_len(5)],
        pop_density = c(1, 5, 1, 4, 1),
        density_units = "km2",
        cap_area_target = 40,
        area_units = "km2"
      )
    ),
    "numeric"
  )
  ## pop_density
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = c(1, NA, 1, 4, 1),
        density_units = "km2",
        cap_area_target = 40,
        area_units = "km2"
      )
    ),
    "missing"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = c(1, 1, 4, 1),
        density_units = "km2",
        cap_area_target = 40,
        area_units = "km2"
      )
    ),
    "length"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = letters[seq_len(5)],
        density_units = "km2",
        cap_area_target = 40,
        area_units = "km2"
      )
    ),
    "numeric"
  )
  ## density_units
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = c(1, 5, 1, 4, 1),
        density_units = c("km2", "m2"),
        cap_area_target = 40,
        area_units = "km2"
      )
    ),
    "length"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = c(1, 5, 1, 4, 1),
        density_units = "greg",
        cap_area_target = 40,
        area_units = "km2"
      )
    ),
    "unit"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = c(1, 5, 1, 4, 1),
        density_units = "g",
        cap_area_target = 40,
        area_units = "km2"
      )
    ),
    "area"
  )
  ## cap_area_target
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = c(1, 5, 1, 4, 1),
        density_units = "km2",
        cap_area_target = "greg",
        area_units = "km2"
      )
    ),
    "number"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = c(1, 5, 1, 4, 1),
        density_units = "km2",
        cap_area_target = c(1, 2),
        area_units = "km2"
      )
    ),
    "number"
  )
  ## area_units
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = c(1, 5, 1, 4, 1),
        density_units = "km2",
        cap_area_target = 40,
        area_units = c("km2", "m2")
      )
    ),
    "value"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = c(1, 5, 1, 4, 1),
        density_units = "lm2",
        cap_area_target = 40,
        area_units = "greg"
      )
    ),
    "unit"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_pop_size_targets(
        pop_size_targets = c(10, 10, 1, 20, 50),
        pop_density = c(1, 5, 1, 4, 1),
        density_units = "km2",
        cap_area_target = 40,
        area_units = "g"
      )
    ),
    "area"
  )
})
