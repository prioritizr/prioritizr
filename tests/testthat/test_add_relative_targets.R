test_that("add_relative_targets (numeric(1), single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_relative_targets(0.1)
  # calculate relative targets
  targets <- p$targets$output()
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
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
  expect_equal(targets$value, c(0.1 * p$feature_abundances_in_total_units()))
})

test_that("add_relative_targets (numeric(5), single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_relative_targets(seq(0.1, 0.6, length.out = 5))
  # calculate relative targets
  targets <- p$targets$output()
  # tests
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
    c(seq(0.1, 0.6, length.out = 5) * p$feature_abundances_in_total_units())
  )
  expect_equal(
    targets$sense,
    rep(">=", terra::nlyr(sim_features))
  )
})

test_that("add_relative_targets (matrix, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_relative_targets(matrix(seq(0.1, 0.6, length.out = 5), ncol = 1))
  # calculate relative targets
  targets <- p$targets$output()
  # tests
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
    c(seq(0.1, 0.6, length.out = 5) * p$feature_abundances_in_total_units())
  )
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("add_relative_targets (character, single zone)", {
  # create data
  pu <- data.frame(id = seq_len(10), cost = runif(10))
  species <- data.frame(id = seq_len(5), name = letters[1:5], target = runif(5))
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5))
  rij$amount <- runif(nrow(rij))
  # create problem
  p <-
    problem(pu, species, rij, "cost") %>%
    add_relative_targets("target")
  # calculate relative targets
  targets <- p$targets$output()
  # tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, 1:5)
  expect_equal(unlist(targets$zone), rep(1, 5))
  expect_equal(
    targets$value,
    species$target * c(p$feature_abundances_in_total_units())
  )
  expect_equal(targets$sense, rep(">=", nrow(species)))
})

test_that("add_relative_targets (invalid input, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # tests
  ## wrong number of features
  expect_tidy_error(add_relative_targets(p, c(0.1, 0.2)))
  expect_tidy_error(add_relative_targets(p, matrix(0.1, ncol = 1, nrow = 3)))
  ## wrong number of zones
  expect_tidy_error(add_relative_targets(p, matrix(0.1, ncol = 2, nrow = 5)))
  expect_tidy_error(add_relative_targets(p, c("cost", "cost")))
  ## NA values
  expect_tidy_error(add_relative_targets(p, c(0.1, NA)))
  expect_tidy_error(add_relative_targets(p, c("cost", NA_character_)))
  ## targets that are outside of exceed zero and one
  expect_tidy_error(add_relative_targets(p, -0.2))
  expect_tidy_error(add_relative_targets(p, -1))
  expect_tidy_error(add_relative_targets(p, 1.2))
})

test_that("add_relative_targets (matrix, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create targets data
  m <- matrix(
    runif(
      terra::nlyr(sim_zones_features[[1]]) * terra::nlyr(sim_zones_pu_raster)
    ),
    ncol = terra::nlyr(sim_zones_pu_raster),
    nrow = terra::nlyr(sim_zones_features[[1]])
  )
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_relative_targets(m)
  # calculate relative targets
  targets <- p$targets$output()
  # tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(
    targets$feature,
    rep(
      seq_len(terra::nlyr(sim_zones_features[[1]])),
      terra::nlyr(sim_zones_pu_raster)
    )
  )
  expect_equal(
    unlist(targets$zone),
    rep(
      seq_len(terra::nlyr(sim_zones_pu_raster)),
      each = terra::nlyr(sim_zones_features[[1]])
    )
  )
  expect_equal(
    targets$value,
    c(m) * c(p$feature_abundances_in_total_units())
  )
  expect_equal(
    targets$sense,
    rep(
      ">=",
      terra::nlyr(sim_zones_features[[1]]) * terra::nlyr(sim_zones_pu_raster)
    )
  )
})

test_that("add_relative_targets (character, multiple zones)", {
  # import data
  pu <- data.frame(id = seq_len(10), cost_1 = runif(10), cost_2 = runif(10))
  species <- data.frame(
    id = seq_len(5), name = letters[1:5],
    target_1 = runif(5), target_2 = runif(5)
  )
  zone <- data.frame(id = seq_len(2), name = LETTERS[1:2])
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5), zone = seq_len(2))
  rij$amount <- runif(nrow(rij))
  # create problem
  p <-
    problem(pu, species, rij, c("cost_1", "cost_2"), zone) %>%
    add_relative_targets(c("target_1", "target_2"))
  # calculate relative targets
  targets <- p$targets$output()
  # tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, rep(1:5, 2))
  expect_equal(unlist(targets$zone), rep(seq_len(2), each = 5))
  expect_equal(
    targets$value,
    c(species$target_1, species$target_2) *
      c(p$feature_abundances_in_total_units())
  )
  expect_equal(targets$sense, rep(">=", 10))
})

test_that("add_relative_targets (invalid input, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  pu <- data.frame(id = seq_len(10), cost_1 = runif(10), cost_2 = runif(10))
  species <- data.frame(
    id = seq_len(5), name = letters[1:5],
    target_1 = runif(5), target_2 = c(runif(4), NA)
  )
  zone <- data.frame(id = seq_len(2), name = LETTERS[1:2])
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5), zone = seq_len(2))
  rij$amount <- runif(nrow(rij))
  # tests
  ## numeric targets
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  expect_tidy_error(add_relative_targets(p, 5))
  expect_tidy_error(
    add_relative_targets(p, rep(5, terra::nlyr(sim_zones_pu_raster)))
  )
  ## matrix targets
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  expect_tidy_error(add_relative_targets(p, matrix(1:5, ncol = 1)))
  expect_tidy_error(add_relative_targets(p, matrix(1:5, nrow = 1)))
  expect_tidy_error(
    add_relative_targets(
      p,
      matrix(
        c(
          seq_len(
            number_of_zones(sim_zones_features) *
            number_of_features(sim_zones_features)
          )[-1], NA
        ),
        ncol = number_of_zones(sim_zones_features),
        nrow = number_of_features(sim_zones_features)
      )
    )
  )
  ## character targets
  p <- problem(pu, species, rij, c("cost_1", "cost_2"), zone)
  expect_tidy_error(add_relative_targets(p, "target_1"))
  expect_tidy_error(add_relative_targets(p, c("target_1", "name")))
  expect_tidy_error(add_relative_targets(p, c("target_1", "target_2")))
})
