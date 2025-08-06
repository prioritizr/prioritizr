test_that("character", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 2e5, 0, 2e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problems
  p0 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_binary_decisions()
  p1 <- p0 %>% add_auto_targets(method = "jung")
  p2 <- p0 %>% add_auto_targets(method = jung_targets())
  # run tests
  expect_equal(as.list(compile(p1)), as.list(compile(p2)))
})

test_that("list (all relative values)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 2e5, 0, 2e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problems
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_auto_targets(method = list(
      jung_targets(),
      "polak",
      jung_targets(prop_uplift = 0.1),
      "polak",
      "polak"
    )) %>%
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
      jd_correct_targets[[1]],
      pd_correct_targets[[2]],
      njd_correct_targets[[3]],
      pd_correct_targets[[4]],
      pd_correct_targets[[5]]
    )
  )
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("list (mixed values)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 2e5, 0, 2e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problems
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_auto_targets(method = list(
      jung_targets(),
      "polak",
      jung_targets(prop_uplift = 0.1),
      "polak",
      absolute_targets(5)
    )) %>%
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
      jd_correct_targets[[1]],
      pd_correct_targets[[2]],
      njd_correct_targets[[3]],
      pd_correct_targets[[4]],
      5
    )
  )
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("invalid inputs", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 2e5, 0, 2e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problems
  p <- problem(sim_pu_raster, sim_features)
  # run tests
  expect_tidy_error(
    p %>% add_auto_targets(NA),
    "method"
  )
  expect_tidy_error(
    p %>% add_auto_targets(NA_character_),
    "method"
  )
  expect_tidy_error(
    p %>% add_auto_targets("greg"),
    "recognized method"
  )
  expect_tidy_error(
    p %>% add_auto_targets(list(jung_targets(), "polak")),
    "must specify a method for each"
  )
  expect_tidy_error(
    p %>% add_auto_targets(
      list(jung_targets(), "polak", "polak", "polak", "greg")
    ),
    "recognized method"
  )
  expect_tidy_error(
    p %>% add_auto_targets(list(greg())),
    "could not find"
  )
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_auto_targets("jung"),
    "zones"
  )
  expect_tidy_error(
    problem(get_sim_pu_raster(), get_sim_features()) %>%
    add_auto_targets("jung"),
    "area-based"
  )
  expect_tidy_error(
    problem(get_sim_pu_raster(), get_sim_features()) %>%
    add_auto_targets(list("jung")[rep(1, 5)]),
    "area-based"
  )
})
