context("add_manual_targets")

test_that("add_manual_targets (default, single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  # create problem
  p <- problem(sim_pu_raster, sim_features) %>%
       add_manual_targets(data.frame(feature = names(sim_features)[-1],
                                     target = seq_len(4),
                                     type = "absolute"))
  # calculate absolute targets
  targets <- p$targets$output()
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, seq_len(raster::nlayers(sim_features))[-1])
  expect_equivalent(unlist(targets$zone),
                    rep(1, raster::nlayers(sim_features) - 1))
  expect_equal(targets$value, as.numeric(seq_len(4)))
  expect_equal(targets$sense, rep(">=", raster::nlayers(sim_features) - 1))
})

test_that("add_manual_targets (explicit, single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  # create problem
  p <- problem(sim_pu_raster, sim_features) %>%
       add_manual_targets(data.frame(feature = names(sim_features)[-1],
                                     target = 2:5,
                                     zone = list("1")[rep(1, 4)],
                                     sense = c(">=", "=", "<=", "="),
                                     type = "absolute"))
  # calculate absolute targets
  targets <- p$targets$output()
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, seq_len(raster::nlayers(sim_features))[-1])
  expect_equivalent(unlist(targets$zone),
                    rep(1, raster::nlayers(sim_features) - 1))
  expect_equal(targets$value, 2:5)
  expect_equal(targets$sense, c(">=", "=", "<=", "="))
})

test_that("add_manual_targets (default, multiple zones)", {
  # load data
  data(sim_pu_zones_stack, sim_features_zones)
  # create problem
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_manual_targets(data.frame(
         feature = feature_names(sim_features_zones)[c(1, 1, 2, 3)],
         zone = zone_names(sim_features_zones)[c(1, 2, 1, 3)],
         target = 4:7,
         type = "absolute"))
  # calculate absolute targets
  targets <- p$targets$output()
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, c(1, 1, 2, 3))
  expect_equivalent(unlist(targets$zone), c(1, 2, 1, 3))
  expect_equal(targets$value, 4:7)
  expect_equal(targets$sense, rep(">=", 4))
})

test_that("add_manual_targets (explicit, multiple zones)", {
  # load data
  data(sim_pu_zones_stack, sim_features_zones)
  # create problem
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_manual_targets(tibble::tibble(
         feature = feature_names(sim_features_zones)[c(1, 1, 2, 3)],
         zone = list("zone_1", "zone_2", "zone_1", c("zone_1", "zone_2")),
         sense = c(">=", "<=", "=", ">="),
         target = 4:7,
         type = "absolute"))
  # calculate absolute targets
  targets <- p$targets$output()
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, c(1, 1, 2, 3))
  expect_equivalent(targets$zone, list(1, 2, 1, c(1, 2)))
  expect_equal(targets$value, 4:7)
  expect_equal(targets$sense, c(">=", "<=", "=", ">="))
})

test_that("add_manual_targets (invalid input)", {
  # load data
  data(sim_pu_zones_stack, sim_features_zones)
  # create problem
  p <- problem(sim_pu_zones_stack, sim_features_zones)
  # tests
  expect_error(add_manual_targets(p, data.frame()))
  expect_error(add_manual_targets(p, data.frame(
    feature = "a",
    zone = zone_names(sim_features_zones)[1],
    type = "absolute",
    target = 1)))
  expect_error(add_manual_targets(p, data.frame(
    feature = feature_names(sim_features_zones)[1],
    zone = "a",
    type = "absolute",
    target = 1)))
  expect_error(add_manual_targets(p, data.frame(
    feature = feature_names(sim_features_zones)[1],
    zone = zone_names(sim_features_zones)[1],
    type = "a",
    target = 1)))
  expect_error(add_manual_targets(p, data.frame(
    feature = feature_names(sim_features_zones)[1],
    zone = zone_names(sim_features_zones)[1],
    type = "absolute",
    target = NA)))
  expect_error(add_manual_targets(p, data.frame(
    feature = feature_names(sim_features_zones)[1],
    zone = zone_names(sim_features_zones)[1],
    type = "absolute",
    target = Inf)))
  expect_error(add_manual_targets(p, data.frame(
    feature = feature_names(sim_features_zones)[1],
    zone = zone_names(sim_features_zones)[1],
    type = "absolute",
    target = "a")))
  expect_error(add_manual_targets(p, data.frame(
    feature = feature_names(sim_features_zones)[1],
    zone = zone_names(sim_features_zones)[1],
    sense = "a",
    type = "absolute",
    target = 1)))
  expect_error(add_manual_targets(p, data.frame(
    feature = feature_names(sim_features_zones)[1],
    zone = zone_names(sim_features_zones)[1],
    sense = NA_character_,
    type = "absolute",
    target = 1)))
})
