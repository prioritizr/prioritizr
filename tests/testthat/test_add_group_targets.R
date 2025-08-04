test_that("works", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  groups <- c("a", "b", "b", "c", "a")
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 2e5, 0, 2e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problems
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_group_targets(
      groups = groups,
      method = list(
        "b" = jung_targets(),
        "a" = "polak",
        "c" = jung_targets(prop_uplift = 0.1)
      )
    ) %>%
    add_binary_decisions()
  # compute values in km^2
  fs <- p$feature_abundances_km2_in_total_units()
  # compute values in absolute units
  fa <- p$feature_abundances_in_total_units()
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## jung default
  jd_pu <- 0.1
  jd_bc <- (1 - 0.3) * (1 + jd_pu)
  jd_ac <- 2000 * (1 + jd_pu)
  jd_correct_targets <- pmin(pmin(pmax(jd_ac, fs * jd_bc), 1e6), fs)
  jd_correct_targets <- fa * (jd_correct_targets / fs)
  ## jung non-default
  njd_pu <- 0.1
  njd_bc <- (1 - 0.3) * (1 + njd_pu)
  njd_ac <- 2000 * (1 + njd_pu)
  njd_correct_targets <- pmin(pmin(pmax(njd_ac, fs * njd_bc), 1e6), fs)
  njd_correct_targets <- fa * (njd_correct_targets / fs)
  ## polak targets
  pd_correct_targets <- fs * linear_interpolation(fs, 1000, 1, 10000, 0.1)
  pd_correct_targets <- pmin(pd_correct_targets, 1e6)
  pd_correct_targets <- pmin(pd_correct_targets, fs)
  pd_correct_targets <- fa * (pd_correct_targets / fs)
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
    c(
      pd_correct_targets[[1]],
      jd_correct_targets[[2]],
      jd_correct_targets[[3]],
      njd_correct_targets[[4]],
      pd_correct_targets[[5]]
    )
  )
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("invalid arguments", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  groups <- c("a", "b", "b", "c", "a")
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 2e5, 0, 2e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problems
  p <- problem(sim_pu_raster, sim_features)
  # run tests
  ## groups
  expect_tidy_error(
    p %>%
    add_group_targets(
      groups = 1,
      method = list(a = "polak", b = "jung", c = "jung")
    ),
    "character"
  )
  expect_tidy_error(
    p %>%
    add_group_targets(
      groups = replace(groups, 1, NA_character_),
      method = list(a = "polak", b = "jung", c = "jung")
    ),
    "missing"
  )
  expect_tidy_error(
    p %>%
    add_group_targets(
      groups = rep(1, length(5)),
      method = list(a = "polak", b = "jung", c = "jung")
    ),
    "character"
  )
  ## method
  expect_tidy_error(
    p %>%
    add_group_targets(
      groups = groups,
      method = NA
    ),
    "method"
  )
  expect_tidy_error(
    p %>%
    add_group_targets(
      groups = groups,
      method = "greg"
    ),
    "list"
  )
  expect_tidy_error(
    p %>%
    add_group_targets(
      groups = groups,
      method = list()
    ),
    "name"
  )
  expect_tidy_error(
    p %>%
    add_group_targets(
      groups = groups,
      method = list(jung_targets())
    ),
    "name"
  )
  expect_tidy_error(
    p %>%
    add_group_targets(
      groups = groups,
      list(a = jung_targets(), c = "greg")
    ),
    "missing the following groups"
  )
  expect_tidy_error(
    p %>%
    add_group_targets(
      groups = groups,
      list(a = jung_targets(), b = "polak", c = "greg")
    ),
    "recognized method"
  )
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_group_targets(
      groups = groups,
      method = list("a" = "jung", "b" = "rodrigues", "c" = "polak")
    ),
    "zones"
  )
})

test_that("problematic inputs", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  groups <- c("a", "b", "b", "c", "a")
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 2e5, 0, 2e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problems
  p <- problem(sim_pu_raster, sim_features)
  # run tests
  expect_warning(
    p %>%
    add_group_targets(
      groups = groups,
      list(a = jung_targets(), b = "jung", d = "polak", c = "polak")
    ),
    "unused"
  )
})
