context("add_loglinear_targets")

test_that("no cap", {
  # load data
  data(sim_pu_raster, sim_features)
  # set total abundance of some features much higher than other features
  for (i in seq_len(raster::nlayers(sim_features)))
    sim_features[[i]] <- sim_features[[i]] ^ i
  # create problem
  p <- problem(sim_pu_raster, sim_features) %>%
       add_loglinear_targets(10, 1, 35, 0.1)
  # calculate absolute targets
  targets <- p$targets$output()
  # calculate expected targets
  values <- loglinear_interpolation(p$feature_abundances_in_total_units()[, 1],
                                    10, 1, 35, 0.1) *
            p$feature_abundances_in_total_units()[, 1]
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, seq_len(raster::nlayers(sim_features)))
  expect_equivalent(unlist(targets$zone), rep(1, raster::nlayers(sim_features)))
  expect_equivalent(targets$value, values)
  expect_equal(targets$sense, rep(">=", raster::nlayers(sim_features)))
})

test_that("cap", {
  # load data
  data(sim_pu_raster, sim_features)
  # set total abundance of some features much higher than other features
  for (i in seq_len(raster::nlayers(sim_features)))
    sim_features[[i]] <- sim_features[[i]] ^ i
  # create problem
  p <- problem(sim_pu_raster, sim_features) %>%
       add_loglinear_targets(10, 1, 35, 0.1, 70, 5)
  # calculate absolute targets
  targets <- p$targets$output()
  # calculate expected targets
  values <- loglinear_interpolation(p$feature_abundances_in_total_units()[, 1],
                                    10, 1, 35, 0.1) *
            p$feature_abundances_in_total_units()[, 1]
  values[p$feature_abundances_in_total_units()[, 1] > 70] <- 5
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, seq_len(raster::nlayers(sim_features)))
  expect_equivalent(unlist(targets$zone), rep(1, raster::nlayers(sim_features)))
  expect_equivalent(targets$value, values)
  expect_equal(targets$sense, rep(">=", raster::nlayers(sim_features)))
})

test_that("invalid inputs", {
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features)
  expect_error(add_loglinear_targets(p, 200, 1, 100, 0.1))
  expect_error(add_loglinear_targets(p, NA, 1, 35, 0.1))
  expect_error(add_loglinear_targets(p, 10, NA, 35, 0.1))
  expect_error(add_loglinear_targets(p, 10, 1, NA, 0.1))
  expect_error(add_loglinear_targets(p, 10, 1, 35, NA))
  expect_error(add_loglinear_targets(p, 10, 1, 35, 0.1, NA, 10))
  expect_error(add_loglinear_targets(p, 10, 1, 35, 0.1, 10, NA))
})
