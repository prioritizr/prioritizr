test_that("solver information (single solution)", {
  skip_on_cran()
  skip_if_no_solvers_installed()
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  weights <- runif(2)
  # create multi-object problem
  p <-
    multi_problem(
      obj1 = problem(sim_zones_pu_raster[[1]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
        add_binary_decisions(),
      obj2 = problem(sim_zones_pu_raster[[2]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
        add_binary_decisions()
    ) %>%
    add_weighted_sum_approach(weights = weights, verbose = FALSE) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # run tests
  expect_inherits(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 1)
  expect_named(s, names(sim_zones_pu_raster[[1]]))
  expect_inherits(attr(s, "objective"), "matrix")
  expect_inherits(c(attr(s, "objective")), "numeric")
  expect_equal(ncol(attr(s, "objective")), 2)
  expect_equal(nrow(attr(s, "objective")), 1)
  expect_equal(colnames(attr(s, "objective")), c("obj1", "obj2"))
  expect_equal(rownames(attr(s, "objective")), "solution_1")
  expect_inherits(attr(s, "runtime"), "numeric")
  expect_length(attr(s, "runtime"), 1)
  expect_named(attr(s, "runtime"), "solution_1")
  expect_inherits(attr(s, "status"), "character")
  expect_length(attr(s, "status"), 1)
  expect_named(attr(s, "status"), "solution_1")
  expect_inherits(attr(s, "gap"), "numeric")
  expect_length(attr(s, "gap"), 1)
  expect_named(attr(s, "gap"), "solution_1")
})

test_that("solver information (multiple solutions)", {
  skip_on_cran()
  skip_if_no_solvers_installed()
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  weights <- matrix(runif(10), ncol = 2)
  # create multi-object problem
  p <-
    multi_problem(
      obj1 = problem(sim_zones_pu_raster[[1]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
        add_binary_decisions(),
      obj2 = problem(sim_zones_pu_raster[[2]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
        add_binary_decisions()
    ) %>%
    add_weighted_sum_approach(weights = weights, verbose = FALSE) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # run tests
  expect_inherits(s, "list")
  expect_length(s, 5)
  expect_named(s, paste0("solution_", seq_len(5)))
  expect_true(all_elements_inherit(s, "SpatRaster"))
  expect_inherits(attr(s, "objective"), "matrix")
  expect_inherits(c(attr(s, "objective")), "numeric")
  expect_equal(ncol(attr(s, "objective")), 2)
  expect_equal(nrow(attr(s, "objective")), 5)
  expect_equal(colnames(attr(s, "objective")), c("obj1", "obj2"))
  expect_equal(rownames(attr(s, "objective")), names(s))
  expect_inherits(attr(s, "runtime"), "numeric")
  expect_length(attr(s, "runtime"), 5)
  expect_named(attr(s, "runtime"), names(s))
  expect_inherits(attr(s, "status"), "character")
  expect_length(attr(s, "status"), 5)
  expect_named(attr(s, "status"), names(s))
  expect_inherits(attr(s, "gap"), "numeric")
  expect_length(attr(s, "gap"), 5)
  expect_named(attr(s, "gap"), names(s))
})

test_that("solve (single solution)", {
  skip_on_cran()
  skip_if_no_solvers_installed()
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  weights <- runif(2)
  # create multi-object problem
  p1 <-
    multi_problem(
      problem(sim_zones_pu_raster[[1]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
        add_binary_decisions(),
      problem(sim_zones_pu_raster[[2]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
        add_binary_decisions()
    ) %>%
    add_weighted_sum_approach(weights = weights, verbose = FALSE) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create equivalent single objective problem
  p2 <-
    problem(
      stats::setNames(sum(c(
        sim_zones_pu_raster[[1]] * weights[[1]],
        sim_zones_pu_raster[[2]] * weights[[2]]
      )), "zone_1"),
      sim_features
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      pmax(
        seq_along(terra::nlyr(sim_features)),
        rev(seq_along(terra::nlyr(sim_features)))
      )
    ) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # generate solutions
  s1 <- solve(p1)
  s2 <- solve(p2)
  # run tests
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("solve (multiple solutions)", {
  skip_on_cran()
  skip_if_no_solvers_installed()
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  weights <- matrix(runif(10), ncol = 2)
  # create multi-object problem
  p1 <-
    multi_problem(
      problem(sim_zones_pu_raster[[1]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
        add_binary_decisions(),
      problem(sim_zones_pu_raster[[2]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
        add_binary_decisions()
    ) %>%
    add_weighted_sum_approach(weights = weights, verbose = FALSE) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create equivalent single objective problem
  p2 <- lapply(seq_len(5), function(i) {
    problem(
      stats::setNames(sum(c(
        sim_zones_pu_raster[[1]] * weights[i, 1],
        sim_zones_pu_raster[[2]] * weights[i, 2]
      )), "zone_1"),
      sim_features
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      pmax(
        seq_along(terra::nlyr(sim_features)),
        rev(seq_along(terra::nlyr(sim_features)))
      )
    ) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  })
  # generate solutions
  s1 <- solve(p1)
  s2 <- lapply(p2, solve)
  # run tests
  expect_equal(terra::values(s1$solution_1), terra::values(s2[[1]]))
  expect_equal(terra::values(s1$solution_2), terra::values(s2[[2]]))
  expect_equal(terra::values(s1$solution_3), terra::values(s2[[3]]))
  expect_equal(terra::values(s1$solution_4), terra::values(s2[[4]]))
  expect_equal(terra::values(s1$solution_5), terra::values(s2[[5]]))
})

test_that("invalid inputs", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  weights <- matrix(runif(10), ncol = 2)
  # create multi-object problem
  p <-
    multi_problem(
      problem(sim_zones_pu_raster[[1]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
        add_binary_decisions(),
      problem(sim_zones_pu_raster[[2]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
        add_binary_decisions()
    )
  # run tests
  expect_tidy_error(
    add_weighted_sum_approach(p, "a"),
     "numeric"
  )
  expect_tidy_error(
    add_weighted_sum_approach(p, c(1, NA_real_)),
    "missing"
  )
  expect_tidy_error(
    add_weighted_sum_approach(p, 1),
    "length"
  )
  expect_tidy_error(
    add_weighted_sum_approach(p, seq_len(3)),
    "length"
  )
  expect_tidy_error(
    add_weighted_sum_approach(p, matrix(seq_len(3), nrow = 3)),
    "ncol"
  )
  expect_tidy_error(
    add_weighted_sum_approach(p, matrix(seq_len(9), nrow = 3)),
    "ncol"
  )
})
