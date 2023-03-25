test_that("no cap", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # set total abundance of some features much higher than other features
  for (i in seq_len(terra::nlyr(sim_features)))
    sim_features[[i]] <- sim_features[[i]] ^ i
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_loglinear_targets(10, 1, 35, 0.1)
  # calculate absolute targets
  targets <- p$targets$output()
  # calculate expected targets
  values <- loglinear_interpolation(
    p$feature_abundances_in_total_units()[, 1], 10, 1, 35, 0.1
  ) * p$feature_abundances_in_total_units()[, 1]
  # tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, seq_len(terra::nlyr(sim_features)))
  expect_equal(unlist(targets$zone), rep(1, terra::nlyr(sim_features)))
  expect_equal(targets$value, unname(values))
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("cap", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # set total abundance of some features much higher than other features
  for (i in seq_len(terra::nlyr(sim_features)))
    sim_features[[i]] <- sim_features[[i]] ^ i
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
      add_loglinear_targets(10, 1, 35, 0.1, 70, 5)
  # calculate absolute targets
  targets <- p$targets$output()
  # calculate expected targets
  values <- loglinear_interpolation(
    p$feature_abundances_in_total_units()[, 1], 10, 1, 35, 0.1
  ) * p$feature_abundances_in_total_units()[, 1]
  values[p$feature_abundances_in_total_units()[, 1] > 70] <- 5
  # tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, seq_len(terra::nlyr(sim_features)))
  expect_equal(unlist(targets$zone), rep(1, terra::nlyr(sim_features)))
  expect_equal(targets$value, unname(values))
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("feature abundances", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # set total abundance of some features much higher than other features
  for (i in seq_len(terra::nlyr(sim_features)))
    sim_features[[i]] <- sim_features[[i]] ^ i
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_loglinear_targets(10, 1, 35, 0.1, abundances = seq_len(5))
  # calculate absolute targets
  targets <- p$targets$output()
  # calculate expected targets
  values <- loglinear_interpolation(seq_len(5), 10, 1, 35, 0.1) * seq_len(5)
  # tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, seq_len(terra::nlyr(sim_features)))
  expect_equal(unlist(targets$zone), rep(1, terra::nlyr(sim_features)))
  expect_equal(targets$value, values)
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("invalid inputs", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # tests
  expect_tidy_error(add_loglinear_targets(p, 200, 1, 100, 0.1))
  expect_tidy_error(add_loglinear_targets(p, NA, 1, 35, 0.1))
  expect_tidy_error(add_loglinear_targets(p, 10, NA, 35, 0.1))
  expect_tidy_error(add_loglinear_targets(p, 10, 1, NA, 0.1))
  expect_tidy_error(add_loglinear_targets(p, 10, 1, 35, NA))
  expect_tidy_error(add_loglinear_targets(p, 10, 1, 35, 0.1, NA, 10))
  expect_tidy_error(add_loglinear_targets(p, 10, 1, 35, 0.1, 10, NA, c(1, 2)))
  expect_tidy_error(add_loglinear_targets(p, 10, 1, 35, 0.1, 10, NA, c(1, NA, 3:6)))
  expect_tidy_error(add_loglinear_targets(p, 10, 1, 35, 0.1, 10, NA, c(1, -1, 3:6)))
})
