test_that("format (single solution)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  names(sim_zones_pu_raster) <- rep("zone_1", 3)
  sim_features <- get_sim_features()
  weights <- c(0.1, 0.5)
  # create multi-object problem
  p <-
    multi_problem(
      obj1 =
        problem(sim_zones_pu_raster[[1]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
        add_binary_decisions(),
      obj2 =
        problem(sim_zones_pu_raster[[2]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
        add_binary_decisions()
    ) %>%
    add_wtd_sum_approach(weights = weights, verbose = FALSE) %>%
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

test_that("format (multiple solutions)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  names(sim_zones_pu_raster) <- rep("zone_1", 3)
  sim_features <- get_sim_features()
  weights <- matrix(seq_len(10), ncol = 2)
  # create multi-object problem
  p <-
    multi_problem(
      obj1 =
        problem(sim_zones_pu_raster[[1]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
        add_binary_decisions(),
      obj2 =
        problem(sim_zones_pu_raster[[2]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
        add_binary_decisions()
    ) %>%
    add_wtd_sum_approach(weights = weights, verbose = FALSE) %>%
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

test_that("correct solution (single solution)", {
  skip_on_cran()
  skip_if_no_solvers_installed()
  # import data
  pu <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1)))
  ft1 <- terra::rast(matrix(c(5, 0.5, 0, 0, 0, 0)))
  ft2 <- terra::rast(matrix(c(0, 0, 0, 0, 3, 2)))
  # create multi-object problem
  mp <-
    prioritizr::multi_problem(
      obj1 =
        prioritizr::problem(pu, ft1) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions(),
      obj2 =
        prioritizr::problem(pu, ft2) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions()
    ) %>%
    add_wtd_sum_approach(weights = c(1, 1), verbose = FALSE) %>%
    prioritizr::add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(mp, run_checks = FALSE)
  # run tests
  expect_s4_class(s, "SpatRaster")
  expect_equal(
    c(terra::values(s)),
    c(1, 0, 0, 0, 1, 0)
  )
  expect_equal(
    attr(s, "objective")[1, ],
    c(obj1 = 5, obj2 = 3)
  )
})

test_that("correct solution (multiple solutions)", {
  skip_on_cran()
  skip_if_no_solvers_installed()
  # import data
  pu <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1)))
  ft1 <- terra::rast(matrix(c(5, 0.5, 0, 0, 0, 0)))
  ft2 <- terra::rast(matrix(c(0, 0, 0, 0, 3, 2)))
  # define weights
  weights <- matrix(0, nrow = 4, ncol = 2)
  weights[1, ] <- c(1, 0)
  weights[2, ] <- c(0, 1)
  weights[3, ] <- c(1, 1)
  weights[4, ] <- c(1, 5)
  # create multi-object problem
  mp <-
    prioritizr::multi_problem(
      obj1 =
        prioritizr::problem(pu, ft1) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions(),
      obj2 =
        prioritizr::problem(pu, ft2) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions()
    ) %>%
    add_wtd_sum_approach(weights = weights, verbose = FALSE) %>%
    prioritizr::add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(mp, run_checks = FALSE)
  # run tests
  expect_type(s, "list")
  expect_equal(length(s), 4)
  expect_equal(
    c(terra::values(s[[1]])),
    c(1, 1, 0, 0, 0, 0)
  )
  expect_equal(
    c(terra::values(s[[2]])),
    c(0, 0, 0, 0, 1, 1)
  )
  expect_equal(
    c(terra::values(s[[3]])),
    c(1, 0, 0, 0, 1, 0)
  )
  expect_equal(
    c(terra::values(s[[4]])),
    c(0, 0, 0, 0, 1, 1)
  )
  expect_equal(
    attr(s, "objective")[1, ],
    c(obj1 = 5.5, obj2 = 0)
  )
  expect_equal(
    attr(s, "objective")[2, ],
    c(obj1 = 0, obj2 = 5)
  )
  expect_equal(
    attr(s, "objective")[3, ],
    c(obj1 = 5, obj2 = 3)
  )
  expect_equal(
    attr(s, "objective")[4, ],
    c(obj1 = 0, obj2 = 5)
  )
})

test_that("invalid inputs", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  names(sim_zones_pu_raster) <- rep("zone_1", 3)
  sim_features <- get_sim_features()
  weights <- matrix(seq_len(10), ncol = 2)
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
  ## weights
  expect_tidy_error(
    add_wtd_sum_approach(p, "a"),
     "numeric"
  )
  expect_tidy_error(
    add_wtd_sum_approach(p, c(1, NA_real_)),
    "missing"
  )
  expect_tidy_error(
    add_wtd_sum_approach(p, 1),
    "value for each problem"
  )
  expect_tidy_error(
    add_wtd_sum_approach(p, seq_len(3)),
    "value for each problem"
  )
  expect_tidy_error(
    add_wtd_sum_approach(p, matrix(seq_len(3), nrow = 3)),
    "column for each problem"
  )
  expect_tidy_error(
    add_wtd_sum_approach(p, matrix(seq_len(9), nrow = 3)),
    "column for each problem"
  )
  ## verbose
  expect_tidy_error(
    add_wtd_sum_approach(p, weights, NA),
     "missing"
  )
})
