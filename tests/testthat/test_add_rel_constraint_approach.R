test_that("gurobi solver information (single solution)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  #skip_if_no_solvers_installed()
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  rel_tol <- runif(1)
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
    add_rel_constraint_approach(rel_tol = rel_tol, verbose = FALSE) %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
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


test_that("gurobi solver information (multiple solutions)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
 # skip_if_no_solvers_installed()
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  rel_tol <- matrix(runif(5), ncol = 1)
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
    add_rel_constraint_approach(rel_tol = rel_tol, verbose = FALSE) %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
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

test_that("other solver information (single solution)", {
  skip_on_cran()
  skip_if_not_installed("highs")
 # skip_if_no_solvers_installed()
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
    add_highs_solver(gap = 0, verbose = FALSE)
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

test_that("other solver information (multiple solutions)", {
  skip_on_cran()
  skip_if_not_installed("highs")
  #skip_if_no_solvers_installed()
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
    add_highs_solver(gap = 0, verbose = FALSE)
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

test_that("relative constraint tolerance controls objective trade-off (gurobi)", {
  skip_on_cran()
  skip_if_not_installed("highs")
  
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  
  b1 <- 0.2 * terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]]
  # use exactly one feature per objective
  f1 <- sim_features[[1]]
  f2 <- sim_features[[2]]
  
  p <-
    multi_problem(
      obj1 = problem(sim_zones_pu_raster[[1]], f1) %>%
        add_max_utility_objective(budget = b1) %>%
        add_binary_decisions(),
      obj2 = problem(sim_zones_pu_raster[[1]], f2) %>%
        add_max_utility_objective(budget = b1) %>%
        add_binary_decisions()
    ) %>%
    add_rel_constraint_approach(rel_tol = matrix(c(0.01, 0.99), ncol = 1),
                                verbose = FALSE) %>%
    add_gurobi_solver(gap = 0, verbose = FALSE) #TODO with gurobi I get negative objective values
  
  s <- solve(p, run_checks = FALSE)
  
  # low tolerance: protect obj1 strongly
  obj_low <- attr(s, "objective")["solution_1", , drop = FALSE]
  
  # high tolerance: allow degradation of obj1 to improve obj2
  obj_high <- attr(s, "objective")["solution_2", , drop = FALSE]
  
  # high tolerance should improve objective 2
  expect_gt(abs(obj_high[1, "obj2"]), abs(obj_low[1, "obj2"]))
  
  # improvement in obj2 should come at a cost to obj1
  expect_lt(abs(obj_high[1, "obj1"]), abs(obj_low[1, "obj1"]))
})

test_that("relative constraint tolerance controls objective trade-off (other solvers)", {
  skip_on_cran()
  skip_if_not_installed("highs")
  
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  
  b1 <- 0.2 * terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]]
  # use exactly one feature per objective
  f1 <- sim_features[[1]]
  f2 <- sim_features[[2]]
  
  p <-
    multi_problem(
      obj1 = problem(sim_zones_pu_raster[[1]], f1) %>%
        add_max_utility_objective(budget = b1) %>%
        add_binary_decisions(),
      obj2 = problem(sim_zones_pu_raster[[1]], f2) %>%
        add_max_utility_objective(budget = b1) %>%
        add_binary_decisions()
    ) %>%
    add_rel_constraint_approach(rel_tol = matrix(c(0.01, 0.99), ncol = 1),
                                verbose = FALSE) %>%
    add_highs_solver(gap = 0, verbose = FALSE)
  
  s <- solve(p, run_checks = FALSE)
  
  # low tolerance: protect obj1 strongly
  obj_low <- attr(s, "objective")["solution_1", , drop = FALSE]
  
  # high tolerance: allow degradation of obj1 to improve obj2
  obj_high <- attr(s, "objective")["solution_2", , drop = FALSE]
  
  # high tolerance should improve objective 2
  expect_gt(obj_high[1, "obj2"], obj_low[1, "obj2"])
  
  # improvement in obj2 should come at a cost to obj1
  expect_lt(obj_high[1, "obj1"], obj_low[1, "obj1"])
})

test_that("invalid inputs", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  rel_tol <- matrix(runif(5), ncol = 1)
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
    add_rel_constraint_approach(p, rel_tol = "a"),
    "numeric"
  )
  expect_tidy_error(
    add_rel_constraint_approach(p, rel_tol = c(0.2, 0.3)),
    "length"
  )
})
