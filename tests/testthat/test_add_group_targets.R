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
        "b" = spec_jung_targets(),
        "a" = "polak",
        "c" = spec_jung_targets(prop_uplift = 0.1)
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

test_that("compatible with all spec functions", {
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
  # override third feature to have all zeros
  sim_features[[3]] <- sim_features[[3]] * 0
  # define data for target setting
  groups <- c("b", "a", rep("b", terra::nlyr(sim_features) - 2L))
  ft_data <- tibble::tribble(
    ~feature, ~diversity, ~rare, ~vu, ~en, ~cr,
    "feature_1", TRUE, TRUE, FALSE, FALSE, TRUE,
    "feature_2", FALSE, FALSE, TRUE, FALSE, FALSE,
    "feature_3", FALSE, FALSE, TRUE, FALSE, FALSE,
    "feature_4", FALSE, FALSE, TRUE, FALSE, FALSE,
    "feature_5", FALSE, TRUE, FALSE, TRUE, TRUE
  )
  wt_data <- c(
    "diversity" = -0.5,
    "rare" = 0.2,
    "vu" = 0.3,
    "cr" = 0.9
  )
  methods = list(
    spec_area_targets = spec_area_targets(
      targets = c(1, 2, 5, 10, 100),
      area_units = c("km2", "km2", "km2", "km2", "ha")
    ),
    spec_duran_targets = spec_duran_targets(
      probability_target = rep(0.9, 5),
      historical_area = rep(10000, 5),
      area_units = "km2"
    ),
    spec_interp_absolute_targets = spec_interp_absolute_targets(
      rare_absolute_threshold = 30,
      rare_relative_target = 0.4,
      rare_absolute_target = 20,
      rare_method = "max",
      common_absolute_threshold = 50,
      common_relative_target = 0.1,
      common_absolute_target = 60,
      common_method = "min",
      cap_absolute_target = 70,
      interp_method = "linear"
    ),
    spec_interp_area_targets = spec_interp_area_targets(
      rare_area_threshold = 3000,
      rare_relative_target = 0.4,
      rare_area_target = 2000,
      rare_method = "max",
      common_area_threshold = 5000,
      common_relative_target = 0.1,
      common_area_target = 6000,
      common_method = "min",
      cap_area_target = 7000,
      interp_method = "linear",
      area_units = "km^2"
    ),
    spec_jung_targets = spec_jung_targets(),
    spec_max_targets = spec_max_targets(
      spec_area_targets(
        targets = c(1, 2, 5, 10, 100),
        area_units = c("km2", "km2", "km2", "km2", "ha")
      ),
      spec_area_targets(
        targets = c(5, 1, 5, 100, 10),
        area_units = "km2"
      )
    ),
    spec_min_targets = spec_min_targets(
      spec_area_targets(
        targets = c(1, 2, 5, 10, 100),
        area_units = c("km2", "km2", "km2", "km2", "ha")
      ),
      spec_area_targets(
        targets = c(5, 1, 5, 100, 10),
        area_units = "km2"
      )
    ),
    spec_polak_targets = spec_polak_targets(),
    spec_pop_size_targets = spec_pop_size_targets(
      pop_size_targets = c(10, 10, 1, 20, 50),
      pop_density = c(1, 5, 1, 4, 1),
      density_units = "km2",
      cap_area_target = 40,
      area_units = "km2"
    ),
    spec_relative_targets = spec_relative_targets(seq(0.1, 0.5, 0.1)),
    spec_rl_species_targets = spec_rl_species_targets(
      status = "CR",
      criterion_a = "A2",
      criterion_b = "B1",
      prop_uplift = 0.15,
      method = "max",
      cap_area_target = 7000,
      area_units = "km^2"
    ),
    spec_rodrigues_targets = spec_rodrigues_targets(),
    spec_rule_targets = spec_rule_targets(
      baseline_relative_target = 0.3,
      rules_relative_target = wt_data,
      data = ft_data,
      cap_area_target = 25000
    ),
    spec_sreekar_targets = spec_sreekar_targets(),
    spec_ward_targets = spec_ward_targets(),
    spec_watson_targets = spec_watson_targets(),
    spec_wilson_targets = spec_wilson_targets(
      mean_growth_rates = c(0.001, 0.5, 1, 0.3, 0.3),
      var_growth_rates = c(0.0001, 0.0001, 0.0001, 0.0001, 0.1),
      pop_density = c(1, 1, 1, 1, 1),
      density_units = "km2",
      cap_area_target = 5,
      area_units = "km2"
    )
  )
  # define initial problem
  expect_warning(
    p0 <- problem(sim_pu_raster, sim_features),
    "zero"
  )
  # run tests
  result <- lapply(seq_along(methods), function(i) {
    try({
      ## define problems
      p1 <-
        p0 %>%
        add_group_targets(
          groups = groups,
          method = list(a = spec_relative_targets(0.2), b = methods[[i]])
        )
      p2 <-
        p0 %>%
        add_auto_targets(method = methods[[i]])
      p3 <-
        p0 %>%
        add_auto_targets(method = spec_relative_targets(0.2))
      ## extract targets
      x1 <- p1$targets$output(p1)
      x2 <- p2$targets$output(p2)
      x3 <- p3$targets$output(p3)
      ## run tests
      return(
        identical(
          x1[groups == "b", , drop = FALSE],
          x2[groups == "b", , drop = FALSE]
        ) &&
        identical(
          x1[groups == "a", , drop = FALSE],
          x3[groups == "a", , drop = FALSE]
        ) &&
        all(is.finite(x1$value)) &&
        all(is.finite(x2$value)) &&
        all(is.finite(x3$value))
      )},
      silent = TRUE
    )
  })
  # display names of target specification functions that fail
  expect_equal(
    names(methods)[!vapply(result, isTRUE, logical(1))],
    character(0)
  )
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
      method = list(spec_jung_targets())
    ),
    "name"
  )
  expect_tidy_error(
    p %>%
    add_group_targets(
      groups = groups,
      list(a = spec_jung_targets(), c = "greg")
    ),
    "missing the following groups"
  )
  expect_tidy_error(
    p %>%
    add_group_targets(
      groups = groups,
      list(a = spec_jung_targets(), b = "polak", c = "greg")
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
      list(a = spec_jung_targets(), b = "jung", d = "polak", c = "polak")
    ),
    "unused"
  )
})
