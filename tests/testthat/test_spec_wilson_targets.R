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
  fa <- p$feature_abundances_km2_in_total_units()
  # create problem
  p <-
    p %>%
    add_auto_targets(
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates = c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  targets
  # calculate correct targets
  ## run interpolation with relative targets
  mean_growth_rates <- c(0.001, 0.5, 1, 0.3, 0.3)
  var_growth_rates <- c(0.0001, 0.0001, 0.0001, 0.0001, 0.1)
  b <- (2 * mean_growth_rates / var_growth_rates) - 1
  pop_size_target <- ((100000 * var_growth_rates * (b^2)) / 2)^(1 / b)
  correct_targets <- pop_size_target
  ## apply target caps
  correct_targets <- pmin(correct_targets, 5)
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
      method = spec_wilson_targets(
        mean_growth_rates = rep(0.5, 5),
        var_growth_rates = rep(0.1, 5),
        pop_density = rep(2, 5),
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
      )
    )
  p2 <-
    p0 %>%
    add_auto_targets(
      method = spec_wilson_targets(
        mean_growth_rates = rep(0.5, 1),
        var_growth_rates = rep(0.1, 1),
        pop_density = rep(2, 1),
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
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
  ## mean_growth_rates
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, NA, 1, 0.3, 0.3),
        var_growth_rates = c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "missing"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 1, 0.3, 0.3),
        var_growth_rates = c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "length"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = letters[seq_len(5)],
        var_growth_rates = c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "numeric"
  )
  ## var_growth_rates
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates = c(0.0001, NA, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "missing"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates = c(0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "length"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates = letters[seq_len(5)],
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "numeric"
  )
  ## pop_density
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates = c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, NA, 1, 1, 1),
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "missing"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates = c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "length"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates =c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = letters[seq_len(5)],
        density_units = "km2",
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "numeric"
  )
  ## density_units
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates = c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = c("km2", "m2"),
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "length"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates = c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "greg",
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "unit"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates =c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "g",
        cap_area_target = 5,
        area_units = "km2"
      )
    ),
    "area"
  )
  ## cap_area_target
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates =c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = c(1, 5),
        area_units = "km2"
      )
    ),
    "number"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates =c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = "greg",
        area_units = "km2"
      )
    ),
    "number"
  )
  ## area_units
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates =c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
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
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates =c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = 40,
        area_units = "greg"
      )
    ),
    "unit"
  )
  expect_tidy_error(
    add_auto_targets(
      p,
      method = spec_wilson_targets(
        mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
        var_growth_rates =c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
        pop_density = c(1, 1, 1, 1, 1),
        density_units = "km2",
        cap_area_target = 40,
        area_units = "g"
      )
    ),
    "area"
  )
})
