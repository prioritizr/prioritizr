test_that("relative targets", {
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
      method = spec_min_targets(
        spec_area_targets(
          targets = c(1, 2, 5, 10, 100),
          area_units = c("km2", "km2", "km2", "km2", "ha")
        ),
        spec_area_targets(
          targets = c(5, 1, 5, 100, 10),
          area_units = "km2"
        )
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  correct_targets <- as_km2(
    c(1, 1, 5, 10, 100),
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

test_that("absolute targets", {
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
      method = spec_min_targets(
        spec_absolute_targets(
          targets = c(1, 2, 5, 10, 100)
        ),
        spec_absolute_targets(
          targets = c(5, 1, 5, 100, 10)
        )
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  correct_targets <- c(1, 1, 5, 10, 10)
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
  ## x
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_min_targets()
    ),
    "missing"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_min_targets(1)
    ),
    "method"
  )
  ## ...
  expect_tidy_error(
    p %>%
    add_auto_targets(
      method = spec_min_targets(spec_jung_targets(), 1)
    ),
    "method"
  )
  ## chaining errors
  msg <- try(
    p |>
    add_auto_targets(
      method = spec_min_targets(
        spec_jung_targets(), spec_area_targets("a", "km2")
      )
    ),
    silent = TRUE
  )
  expect_true(grepl("add_auto_targets", msg, fixed = TRUE))
  expect_length(strsplit(msg, "spec_min_targets(", fixed = TRUE)[[1]], 3)
  expect_true(grepl("spec_area_targets", msg, fixed = TRUE))
  expect_true(grepl("numeric", msg, fixed = TRUE))
  ## chaining nested errors
  msg <- try(
    p %>%
    add_auto_targets(
      method = spec_min_targets(
        spec_jung_targets(),
        spec_min_targets(spec_area_targets("a", "km2"))
      )
    ),
    silent = TRUE
  )
  expect_true(grepl("add_auto_targets", msg, fixed = TRUE))
  expect_length(strsplit(msg, "spec_min_targets(", fixed = TRUE)[[1]], 4)
  expect_true(grepl("spec_area_targets", msg, fixed = TRUE))
  expect_true(grepl("numeric", msg, fixed = TRUE))
})
