context("add_relative_targets")

test_that("add_relative_targets (numeric(1), single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  # create problem
  p <- problem(sim_pu_raster, sim_features) %>%
       add_relative_targets(0.1)
  # calculate relative targets
  targets <- p$targets$output()
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, seq_len(raster::nlayers(sim_features)))
  expect_equivalent(unlist(targets$zone), rep(1, raster::nlayers(sim_features)))
  expect_equal(targets$sense, rep(">=", raster::nlayers(sim_features)))
  expect_equal(targets$value, c(0.1 * p$feature_abundances_in_total_units()))
})

test_that("add_relative_targets (numeric(5), single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  # create problem
  p <- problem(sim_pu_raster, sim_features) %>%
       add_relative_targets(seq(0.1, 0.6, length.out = 5))
  # calculate relative targets
  targets <- p$targets$output()
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, seq_len(raster::nlayers(sim_features)))
  expect_equivalent(unlist(targets$zone), rep(1, raster::nlayers(sim_features)))
  expect_equal(targets$value, c(seq(0.1, 0.6, length.out = 5) *
                                p$feature_abundances_in_total_units()))
  expect_equal(targets$sense, rep(">=", raster::nlayers(sim_features)))
})

test_that("add_relative_targets (matrix, single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  # create problem
  p <- problem(sim_pu_raster, sim_features) %>%
       add_relative_targets(matrix(seq(0.1, 0.6, length.out = 5), ncol = 1))
  # calculate relative targets
  targets <- p$targets$output()
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, seq_len(raster::nlayers(sim_features)))
  expect_equivalent(unlist(targets$zone), rep(1, raster::nlayers(sim_features)))
  expect_equal(targets$value, c(seq(0.1, 0.6, length.out = 5) *
                                p$feature_abundances_in_total_units()))
  expect_equal(targets$sense, rep(">=", raster::nlayers(sim_features)))
})

test_that("add_relative_targets (character, single zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = runif(10))
  species <- data.frame(id = seq_len(5), name = letters[1:5],
                        target = runif(5))
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5))
  rij$amount <- runif(nrow(rij))
  # create problem
  p <- problem(pu, species, rij, "cost") %>%
       add_relative_targets("target")
  # calculate relative targets
  targets <- p$targets$output()
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, 1:5)
  expect_equivalent(unlist(targets$zone), rep(1, 5))
  expect_equal(targets$value, species$target *
                              c(p$feature_abundances_in_total_units()))
  expect_equal(targets$sense, rep(">=", raster::nlayers(sim_features)))
})

test_that("add_relative_targets (invalid input, single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # tests
  ## wrong number of features
  expect_error(add_relative_targets(p, c(0.1, 0.2)))
  expect_error(add_relative_targets(p, matrix(0.1, ncol = 1, nrow = 3)))
  ## wong number of zones
  expect_error(add_relative_targets(p, matrix(0.1, ncol = 2, nrow = 5)))
  expect_error(add_relative_targets(p, c("cost", "cost")))
  ## NA values
  expect_error(add_relative_targets(p, c(0.1, NA)))
  expect_error(add_relative_targets(p, c("cost", NA_character_)))
  ## targets that are outside of exceed zero and one
  expect_error(add_relative_targets(p, -0.2))
  expect_error(add_relative_targets(p, -1))
  expect_error(add_relative_targets(p, 1.2))
})

test_that("add_relative_targets (matrix, multiple zones)", {
  # load data
  data(sim_pu_zones_stack, sim_features_zones)
  m <- matrix(runif(raster::nlayers(sim_features) *
                      raster::nlayers(sim_pu_zones_stack)),
              ncol = raster::nlayers(sim_pu_zones_stack),
              nrow = raster::nlayers(sim_features))
  # create problem
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_relative_targets(m)
  # calculate relative targets
  targets <- p$targets$output()
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, rep(seq_len(raster::nlayers(sim_features)),
                                    raster::nlayers(sim_pu_zones_stack)))
  expect_equivalent(unlist(targets$zone),
                    rep(seq_len(raster::nlayers(sim_pu_zones_stack)),
                        each = raster::nlayers(sim_features)))
  expect_equal(targets$value, c(m) *
                              c(p$feature_abundances_in_total_units()))
  expect_equal(targets$sense, rep(">=", raster::nlayers(sim_features) *
                                        raster::nlayers(sim_pu_zones_stack)))
})

test_that("add_relative_targets (character, multiple zones)", {
  # simulate data
  pu <- data.frame(id = seq_len(10),
                   cost_1 = runif(10), cost_2 = runif(10))
  species <- data.frame(id = seq_len(5), name = letters[1:5],
                        target_1 = runif(5), target_2 = runif(5))
  zone <- data.frame(id = seq_len(2), name = LETTERS[1:2])
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5), zone = seq_len(2))
  rij$amount <- runif(nrow(rij))
  # create problem
  p <- problem(pu, species, rij, c("cost_1", "cost_2"), zone) %>%
       add_relative_targets(c("target_1", "target_2"))
  # calculate relative targets
  targets <- p$targets$output()
  # run tests
  expect_is(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "zone", "sense", "value")))
  expect_is(targets$feature, "integer")
  expect_is(targets$zone, "list")
  expect_is(targets$value, "numeric")
  expect_is(targets$sense, "character")
  expect_equal(targets$feature, rep(1:5, 2))
  expect_equivalent(unlist(targets$zone), rep(seq_len(2), each = 5))
  expect_equal(targets$value, c(species$target_1, species$target_2) *
                              c(p$feature_abundances_in_total_units()))
  expect_equal(targets$sense, rep(">=", 10))
})

test_that("add_relative_targets (invalid input, multiple zones)", {
  # simulate data
  data(sim_pu_zones_stack, sim_features_zones)
  pu <- data.frame(id = seq_len(10),
                   cost_1 = runif(10), cost_2 = runif(10))
  species <- data.frame(id = seq_len(5), name = letters[1:5],
                        target_1 = runif(5), target_2 = c(runif(4), NA))
  zone <- data.frame(id = seq_len(2), name = LETTERS[1:2])
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5), zone = seq_len(2))
  rij$amount <- runif(nrow(rij))
  # numeric inputs
  p <- problem(sim_pu_zones_stack, sim_features_zones)
  expect_error(add_relative_targets(p, 5))
  expect_error(
    add_relative_targets(p,
                         rep(5, raster::nlayers(sim_pu_zones_stack))))
  # matrix input
  p <- problem(sim_pu_zones_stack, sim_features_zones)
  expect_error(add_relative_targets(p, matrix(1:5, ncol = 1)))
  expect_error(add_relative_targets(p, matrix(1:5, nrow = 1)))
  expect_error(
    add_relative_targets(p,
      matrix(c(seq_len(number_of_zones(sim_features_zones) *
                       number_of_features(sim_features_zones))[-1], NA),
             ncol = number_of_zones(sim_features_zones),
             nrow = number_of_features(sim_features_zones))))
  # character inputs
  p <- problem(pu, species, rij, c("cost_1", "cost_2"), zone)
  expect_error(add_relative_targets(p, "target_1"))
  expect_error(add_relative_targets(p, c("target_1", "name")))
  expect_error(add_relative_targets(p, c("target_1", "target_2")))
})
