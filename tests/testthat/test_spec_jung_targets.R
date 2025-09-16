test_that("defaults", {
  # create object
  x <- spec_jung_targets()
  # run tests
  expect_equal(x$args$status, "VU")
  expect_equal(x$args$criterion_a, "A2")
  expect_equal(x$args$criterion_b, "B2")
  expect_equal(x$args$prop_uplift, 0.1)
  expect_equal(x$args$method, "max")
  expect_equal(x$args$cap_area_target, 1e6)
  expect_equal(x$args$area_units, "km^2")
})

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
  # create problem
  p <-
    p %>%
    add_auto_targets(method = spec_jung_targets())
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## set criteria
  pu <- 0.1
  bc <- (1 - 0.3) * (1 + pu)
  ac <- 2000 * (1 + pu)
  correct_targets <- pmin(pmin(pmax(ac, fs * bc), 1e6), fs)
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
