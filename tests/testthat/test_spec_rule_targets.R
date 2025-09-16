test_that("raster feature data", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
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
    add_auto_targets(
      method = spec_rule_targets(
        baseline_relative_target = 0.3,
        rules_relative_target = wt_data,
        data = ft_data,
        cap_area_target = 25000
      )
    )
  # calculate targets
  targets <- p$targets$output(p)
  # calculate correct targets
  ## calculate relative targets
  correct_targets <- rowSums(
    as.matrix(ft_data[, names(wt_data)]) *
    matrix(
      unname(wt_data),
      byrow = TRUE, nrow = nrow(ft_data), ncol = length(wt_data)
    )
  )
  correct_targets <- 0.3 + correct_targets
  correct_targets <- pmax(correct_targets, 0)
  correct_targets <- pmin(correct_targets, 1)
  ## apply target cap
  correct_targets <- correct_targets * fs
  correct_targets <- pmin(correct_targets, 25000)
  correct_targets <- pmin(correct_targets, fs)
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

test_that("invalid inputs", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
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
  # define spatial properties
  terra::ext(sim_pu_raster) <- c(0, 2e5, 0, 2e5)
  terra::crs(sim_pu_raster) <- terra::crs("epsg:3857")
  terra::ext(sim_features) <- terra::ext(sim_pu_raster)
  terra::crs(sim_features) <- terra::crs(sim_pu_raster)
  # create problems
  p <- problem(sim_pu_raster, sim_features)
  # run tests
  ## baseline_relative_target
  expect_tidy_error(
    p %>%
    add_auto_targets(spec_rule_targets(NA, wt_data, ft_data)),
    "number"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(spec_rule_targets(-1.2, wt_data, ft_data)),
    "0"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(spec_rule_targets(1.2, wt_data, ft_data)),
    "1"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(spec_rule_targets(NA_real_, wt_data, ft_data)),
    "missing"
  )
  ## rules_relative_target
  expect_tidy_error(
    p %>%
    add_auto_targets(spec_rule_targets(0.3, "a", ft_data)),
    "numeric"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(
      spec_rule_targets(0.3, c(cr = NA_real_, vu = 0.2), ft_data)
    ),
    "missing"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(spec_rule_targets(0.3, c(cr = 0.1, vu = 1.2), ft_data)),
    "between"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(spec_rule_targets(0.3, c(), ft_data)),
    "numeric"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(spec_rule_targets(0.3, c(cr = 0.1, greg = 0.4), ft_data)),
    "names"
  )
  ## data
  expect_tidy_error(
    p %>%
    add_auto_targets(spec_rule_targets(0.3, wt_data, "a")),
    "data frame"
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(spec_rule_targets(0.3, wt_data, ft_data[-1, ])),
    "features are missing",
  )
  expect_tidy_error(
    p %>%
    add_auto_targets(spec_rule_targets(0.3, wt_data, ft_data[, -1])),
    "feature"
  )
  expect_tidy_error(
    {
      d <- ft_data;
      d$feature[[1]] <- "greg"
      p %>% add_auto_targets(spec_rule_targets(0.3, wt_data, d))
    },
    "feature names"
  )
  expect_tidy_error(
    {
      d <- ft_data
      d$feature[[1]] <- NA
      p %>% add_auto_targets(spec_rule_targets(0.3, wt_data, d))
    },
    "missing"
  )
  expect_tidy_error(
    {
      d <- ft_data;
      d$feature <- runif(nrow(d))
      p %>% add_auto_targets(spec_rule_targets(0.3, wt_data, d))
    },
    "numeric"
  )
  expect_tidy_error(
    {
      d <- ft_data;
      d$cr[[1]] <- NA
      p %>% add_auto_targets(spec_rule_targets(0.3, wt_data, d))
    },
    "missing"
  )
  expect_tidy_error(
    {
      d <- ft_data;
      d$cr <- runif(5)
      p %>% add_auto_targets(spec_rule_targets(0.3, wt_data, d))
    },
    "logical"
  )
})
