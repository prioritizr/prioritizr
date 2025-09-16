test_that("defaults", {
  # create object
  x <- spec_polak_targets()
  # tests
  expect_equal(x$args$rare_area_threshold, 1000)
  expect_equal(x$args$rare_relative_target, 1)
  expect_equal(x$args$rare_area_target, NA_real_)
  expect_equal(x$args$rare_method, "max")
  expect_equal(x$args$common_area_threshold, 10000)
  expect_equal(x$args$common_relative_target, 0.1)
  expect_equal(x$args$common_area_target, NA_real_)
  expect_equal(x$args$common_method, "max")
  expect_equal(x$args$cap_area_target, 1e6)
  expect_equal(x$args$interp_method, "linear")
  expect_equal(x$args$area_units, "km^2")
})

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
    add_auto_targets(method = spec_polak_targets())
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## run interpolation with relative targets
  correct_targets <- fs * linear_interpolation(fs, 1000, 1, 10000, 0.1)
  ## apply target caps
  correct_targets <- pmin(correct_targets, 1e6)
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
