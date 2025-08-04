test_that("numeric (1)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_auto_targets(relative_targets(0.1))
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
  expect_equal(targets$feature, seq_len(terra::nlyr(sim_features)))
  expect_equal(unlist(targets$zone), rep(1, terra::nlyr(sim_features)))
  expect_equal(
    targets$value,
     0.1 * terra::global(sim_features, "sum", na.rm = TRUE)[[1]]
  )
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("numeric (5)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  v <- runif(terra::nlyr(sim_features))
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_auto_targets(relative_targets(v))
  # calculate absolute targets
  targets <- p$targets$output(p)
  # run tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, seq_len(terra::nlyr(sim_features)))
  expect_equal(unlist(targets$zone), rep(1, terra::nlyr(sim_features)))
  expect_equal(
    targets$value,
    v * terra::global(sim_features, "sum", na.rm = TRUE)[[1]]
  )
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("invalid inputs", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective()
  # run tests
  expect_tidy_error(
    relative_targets(p, 0.1),
    "add_relative_targets"
  )
  expect_tidy_error(
    p %>% add_auto_targets(relative_targets("a")) %>% compile(),
    "numeric"
  )
  expect_tidy_error(
    p %>% add_auto_targets(relative_targets(c(0.1, 0.2))) %>% compile(),
    "length"
  )
  expect_tidy_error(
    p %>% add_auto_targets(relative_targets(NA_real_)) %>% compile(),
    "missing"
  )
  expect_tidy_error(
    p %>% add_auto_targets(relative_targets(-0.1)) %>% compile(),
    "between"
  )
  expect_tidy_error(
    p %>% add_auto_targets(relative_targets(1.1)) %>% compile(),
    "between"
  )
})
