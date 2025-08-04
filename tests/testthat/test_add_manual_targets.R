test_that("add_manual_targets (default, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_manual_targets(
      data.frame(
        feature = names(sim_features)[-1],
        target = seq_len(4),
        type = "absolute"
      )
    )
  # calculate absolute targets
  targets <- p$targets$output(p)
  # tests
  print(p)
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, seq_len(terra::nlyr(sim_features))[-1])
  expect_equal(unlist(targets$zone), rep(1, terra::nlyr(sim_features) - 1))
  expect_equal(targets$value, as.numeric(seq_len(4)))
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features) - 1))
})

test_that("add_manual_targets (mixed, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_manual_targets(
      data.frame(
        feature = names(sim_features)[-1],
        target = c(0.1, 1, 2, 3),
        type = c("relative", rep("absolute", 3))
      )
    )
  # calculate absolute targets
  targets <- p$targets$output(p)
  # run tests
  print(p)
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, seq_len(terra::nlyr(sim_features))[-1])
  expect_equal(unlist(targets$zone), rep(1, terra::nlyr(sim_features) - 1))
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features) - 1))
  expect_equal(
    targets$value,
    c(0.1 * terra::global(sim_features[[2]], "sum", na.rm = TRUE)[[1]], 1, 2, 3)
  )
})

test_that("add_manual_targets (explicit, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_manual_targets(
      tibble::tibble(
        feature = names(sim_features)[-1],
        zone = list("layer")[rep(1, 4)],
        sense = c(">=", "=", "<=", "="),
        type = "absolute",
        target = 2:5
      )
    )
  # calculate absolute targets
  targets <- p$targets$output(p)
  # tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, seq_len(terra::nlyr(sim_features))[-1])
  expect_equal(unlist(targets$zone), rep(1, terra::nlyr(sim_features) - 1))
  expect_equal(targets$value, 2:5)
  expect_equal(targets$sense, c(">=", "=", "<=", "="))
})

test_that("add_manual_targets (default, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_manual_targets(
      data.frame(
        feature = feature_names(sim_zones_features)[c(1, 1, 2, 3)],
        zone = zone_names(sim_zones_features)[c(1, 2, 1, 3)],
        target = 4:7,
        type = "absolute"
      )
    )
  # calculate absolute targets
  targets <- p$targets$output(p)
  # tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, c(1, 1, 2, 3))
  expect_equal(unlist(targets$zone), c(1, 2, 1, 3))
  expect_equal(targets$value, 4:7)
  expect_equal(targets$sense, rep(">=", 4))
})

test_that("add_manual_targets (explicit, multiple zones, negative)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_manual_targets(
      tibble::tibble(
        feature = feature_names(sim_zones_features)[c(1, 1, 2, 3)],
        zone = list("zone_1", "zone_2", "zone_1", c("zone_1", "zone_2")),
        sense = c(">=", "<=", "=", ">="),
        target = 4:7,
        type = "absolute"
      )
    )
  # calculate absolute targets
  targets <- p$targets$output(p)
  # tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, c(1, 1, 2, 3))
  expect_equal(targets$zone, list(1, 2, 1, c(1, 2)))
  expect_equal(targets$value, 4:7)
  expect_equal(targets$sense, c(">=", "<=", "=", ">="))
})

test_that("add_manual_targets (explicit, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  expect_warning(
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_manual_targets(
      tibble::tibble(
        feature = feature_names(sim_zones_features)[c(1, 1, 2, 3)],
        zone = list("zone_1", "zone_2", "zone_1", c("zone_1", "zone_2")),
        sense = c(">=", "<=", "=", ">="),
        target = c(-1, -2, 1, 2),
        type = "absolute"
      )
    ),
    "negative"
  )
  # calculate absolute targets
  targets <- p$targets$output(p)
  # tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, c(1, 1, 2, 3))
  expect_equal(targets$zone, list(1, 2, 1, c(1, 2)))
  expect_equal(targets$value, c(-1, -2, 1, 2))
  expect_equal(targets$sense, c(">=", "<=", "=", ">="))
})

test_that("add_manual_targets (invalid input)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  # tests
  expect_tidy_error(add_manual_targets(p, data.frame()))
  expect_tidy_error(
    add_manual_targets(
      p,
      data.frame(
        feature = "a",
        zone = zone_names(sim_zones_features)[1],
        type = "absolute",
        target = 1
      )
    )
  )
  expect_tidy_error(
    add_manual_targets(
      p,
      data.frame(
        feature = feature_names(sim_zones_features)[1],
        zone = "a",
        type = "absolute",
        target = 1
      )
    )
  )
  expect_tidy_error(
    add_manual_targets(
      p,
      data.frame(
        feature = feature_names(sim_zones_features)[1],
        zone = zone_names(sim_zones_features)[1],
        type = "a",
        target = 1
      )
    )
  )
  expect_tidy_error(
    add_manual_targets(
      p,
      data.frame(
        feature = feature_names(sim_zones_features)[1],
        zone = zone_names(sim_zones_features)[1],
        type = "absolute",
        target = NA
      )
    )
  )
  expect_tidy_error(
    add_manual_targets(
      p,
      data.frame(
        feature = feature_names(sim_zones_features)[1],
        zone = zone_names(sim_zones_features)[1],
        type = "absolute",
        target = Inf
      )
    )
  )
  expect_tidy_error(
    add_manual_targets(
      p,
      data.frame(
        feature = feature_names(sim_zones_features)[1],
        zone = zone_names(sim_zones_features)[1],
        type = "absolute",
        target = "a"
      )
    )
  )
  expect_tidy_error(
    add_manual_targets(
      p,
      data.frame(
        feature = feature_names(sim_zones_features)[1],
        zone = zone_names(sim_zones_features)[1],
        sense = "a",
        type = "absolute",
        target = 1
      )
    )
  )
  expect_tidy_error(
    add_manual_targets(
      p,
      data.frame(
        feature = feature_names(sim_zones_features)[1],
        zone = zone_names(sim_zones_features)[1],
        sense = NA_character_,
        type = "absolute",
        target = 1
      )
    )
  )
  expect_tidy_error(
    add_manual_targets(
      p,
      tibble::tibble(
        feature = feature_names(sim_zones_features)[[1]],
        zone = list(list(zone_names(sim_zones_features))),
        sense = ">=",
        type = "absolute",
        target = 1
      )
    )
  )
})
