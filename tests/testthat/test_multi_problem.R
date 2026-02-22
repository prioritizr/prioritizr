test_that("single zone", {
  stop("TODO")
})

test_that("multiple zone", {
  stop("TODO")
})

test_that("warnings", {
  # test that throws warning if problems have non-default portfolios
  stop("TODO")
  # test that throws warning if problems have non-default solvers
  stop("TODO")
})

test_that("invalid inputs", {
  # load data
  sim_pu_raster <- get_pu_raster()
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # run tests
  ## single problem
  expect_tidy_error(
    multi_problem(
      problem(sim_zones_pu_raster[[1]], sim_features) %>%
      add_min_set_objective() %>%
      add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
      add_binary_decisions()
    ),
    "at least two"
  )
  ## mismatched planning units
  expect_tidy_error(
    {
      c1 <- sim_pu_raster
      c2 <- sim_pu_raster[1:10] <- NA
      multi_problem(
        obj1 =
          problem(c1, sim_features) %>%
          add_min_set_objective() %>%
          add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
          add_binary_decisions(),
        obj2 =
          problem(c2, sim_features) %>%
          add_min_set_objective() %>%
          add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
          add_binary_decisions()
      )
    },
    "planning units"
  )
  expect_tidy_error(
    {
      multi_problem(
        obj1 =
          problem(sim_pu_raster, sim_features) %>%
          add_min_set_objective() %>%
          add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
          add_binary_decisions(),
        obj2 =
          problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
          add_min_set_objective() %>%
          add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
          add_binary_decisions()
      )
    },
    "planning units"
  )
  ## mismatched number of zones
  expect_tidy_error(
    multi_problem(
      obj1 =
        problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
        add_binary_decisions(),
      obj2 =
        problem(sim_zones_pu_raster, sim_zones_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(
          matrix(
            1,
            ncol = number_of_zones(sim_zones_pu_raster),
            nrow = number_of_features(sim_zones_features)
          )
        ) %>%
        add_binary_decisions()
    ),
    "same number of zones"
  )
  ## mismatched zone names
  stop("TODO")
  ## mismatched decision types
  stop("TODO")
})
