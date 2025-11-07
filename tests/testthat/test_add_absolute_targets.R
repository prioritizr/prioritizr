test_that("numeric(1) (single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_absolute_targets(5)
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
  expect_equal(targets$value, rep(5, terra::nlyr(sim_features)))
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("numeric(5) (single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_absolute_targets(5:9)
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
  expect_equal(targets$value, 5:9)
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("matrix (single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_absolute_targets(matrix(5:9, ncol = 1))
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
  expect_equal(targets$value, 5:9)
  expect_equal(targets$sense, rep(">=", terra::nlyr(sim_features)))
})

test_that("character (single zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = runif(10))
  species <- data.frame(
    id = seq_len(5),
    name = letters[1:5],
    target = runif(5)
  )
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5))
  rij$amount <- runif(nrow(rij))
  # create problem
  p <-
    problem(pu, species, rij, "cost") %>%
    add_absolute_targets("target")
  # calculate absolute targets
  targets <- p$targets$output(p)
  # run tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, 1:5)
  expect_equal(unlist(targets$zone), rep(1, 5))
  expect_equal(targets$value, species$target)
  expect_equal(targets$sense, rep(">=", nrow(species)))
})

test_that("invalid input (single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # tests
  ## wrong number of features
  expect_tidy_error(add_absolute_targets(p, c(1, 2)))
  expect_tidy_error(add_absolute_targets(p, matrix(1, ncol = 1, nrow = 3)))
  ## wong number of zones
  expect_tidy_error(add_absolute_targets(p, matrix(1, ncol = 2, nrow = 5)))
  expect_tidy_error(add_absolute_targets(p, c("cost", "cost")))
  ## NA values
  expect_tidy_error(add_absolute_targets(p, c(1, NA)))
  expect_tidy_error(add_absolute_targets(p, c("cost", NA_character_)))
  ## targets that exceed largest possible value
  expect_warning(add_absolute_targets(p, 1e+5))
})

test_that("matrix (multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  m <- matrix(
    seq_len(
      number_of_features(sim_zones_features) *
      number_of_zones(sim_zones_features)
    ),
    ncol = number_of_zones(sim_zones_features),
    nrow = number_of_features(sim_zones_features)
  )
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_absolute_targets(m)
  # calculate absolute targets
  targets <- p$targets$output(p)
  # run tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(
    targets$feature,
    rep(
      seq_len(number_of_features(sim_zones_features)),
      number_of_zones(sim_zones_features)
    )
  )
  expect_equal(
    unlist(targets$zone),
    rep(
      seq_len(number_of_zones(sim_zones_features)),
      each = number_of_features(sim_zones_features)
    )
  )
  expect_equal(targets$value, 1:15)
  expect_equal(
    targets$sense,
    rep(
      ">=",
      number_of_zones(sim_zones_features) *
        number_of_features(sim_zones_features)
    )
  )
})

test_that("character (multiple zones)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = runif(10),
    cost_2 = runif(10)
  )
  species <- data.frame(
    id = seq_len(5),
    name = letters[1:5],
    target_1 = runif(5),
    target_2 = runif(5)
  )
  zone <- data.frame(id = seq_len(2), name = LETTERS[1:2])
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5), zone = seq_len(2))
  rij$amount <- runif(nrow(rij))
  # create problem
  p <-
    problem(pu, species, rij, c("cost_1", "cost_2"), zone) %>%
    add_absolute_targets(c("target_1", "target_2"))
  # calculate absolute targets
  targets <- p$targets$output(p)
  # run tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(targets$feature, rep(1:5, 2))
  expect_equal(unlist(targets$zone), rep(seq_len(2), each = 5))
  expect_equal(targets$value, c(species$target_1, species$target_2))
  expect_equal(targets$sense, rep(">=", 10))
})

test_that("invalid input (multiple zones)", {
  # simulate data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  pu <- data.frame(
    id = seq_len(10), cost_1 = runif(10), cost_2 = runif(10)
  )
  species <- data.frame(
    id = seq_len(5), name = letters[1:5],
    target_1 = runif(5), target_2 = c(runif(4), NA)
  )
  zone <- data.frame(id = seq_len(2), name = LETTERS[1:2])
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5), zone = seq_len(2))
  rij$amount <- runif(nrow(rij))
  # numeric inputs
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  expect_tidy_error(add_absolute_targets(p, 5))
  expect_tidy_error(
    add_absolute_targets(p, rep(5, terra::nlyr(sim_zones_pu_raster)))
  )
  # matrix input
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  expect_tidy_error(add_absolute_targets(p, matrix(1:5, ncol = 1)))
  expect_tidy_error(add_absolute_targets(p, matrix(1:5, nrow = 1)))
  expect_tidy_error(
    add_absolute_targets(
      p,
      matrix(
        c(
          seq_len(
            number_of_zones(sim_zones_features) *
            number_of_features(sim_zones_features)
          )[-1],
          NA
        ),
        ncol = number_of_zones(sim_zones_features),
        nrow = number_of_features(sim_zones_features)
      )
    )
  )
  # character inputs
  p <- problem(pu, species, rij, c("cost_1", "cost_2"), zone)
  expect_tidy_error(add_absolute_targets(p, "target_1"))
  expect_tidy_error(add_absolute_targets(p, c("target_1", "name")))
  expect_tidy_error(add_absolute_targets(p, c("target_1", "target_2")))
})

test_that("matrix (multiple zones, negative data)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  m <- matrix(
    runif(
      number_of_features(sim_zones_features) *
        number_of_zones(sim_zones_features),
      -1,
      1
    ),
    ncol = number_of_zones(sim_zones_features),
    nrow = number_of_features(sim_zones_features)
  )
  # create problem
  expect_warning(
    p <-
      problem(sim_zones_pu_raster, sim_zones_features) %>%
      add_absolute_targets(m),
    "negative values"
  )
  # calculate absolute targets
  targets <- p$targets$output(p)
  # run tests
  expect_inherits(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_inherits(targets$feature, "integer")
  expect_inherits(targets$zone, "list")
  expect_inherits(targets$value, "numeric")
  expect_inherits(targets$sense, "character")
  expect_equal(
    targets$feature,
    rep(
      seq_len(number_of_features(sim_zones_features)),
      number_of_zones(sim_zones_features)
    )
  )
  expect_equal(
    unlist(targets$zone),
    rep(
      seq_len(number_of_zones(sim_zones_features)),
      each = number_of_features(sim_zones_features)
    )
  )
  expect_equal(targets$value, c(m))
  expect_equal(
    targets$sense,
    rep(
      ">=",
      number_of_zones(sim_zones_features) *
        number_of_features(sim_zones_features)
    )
  )
})

test_that("warnings", {
  # load data
  sim_pu_polygons <- get_sim_pu_polygons()
  # prepare data
  sim_pu_polygons$spp_1 <- runif(nrow(sim_pu_polygons))
  sim_pu_polygons$spp_2 <- runif(nrow(sim_pu_polygons))
  sim_pu_polygons$spp_3 <- runif(nrow(sim_pu_polygons))
  # create problem
  p <-
    problem(
      sim_pu_polygons,
      c("spp_1", "spp_2", "spp_3"),
      cost_column = "cost",
      feature_units = "km2"
    ) %>%
    add_min_set_objective()
  # run tests
  expect_warning(
    add_absolute_targets(p, 1),
    "spatial units"
  )
})
