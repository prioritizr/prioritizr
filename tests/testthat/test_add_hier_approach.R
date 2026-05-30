test_that("format (gurobi solver, single solution)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  names(sim_zones_pu_raster) <- rep("zone_1", 3)
  sim_features <- get_sim_features()
  rel_tol <- c(0.1)
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
    add_hier_approach(rel_tol = rel_tol, verbose = FALSE) %>%
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

test_that("format (gurobi solver, multiple solutions)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  names(sim_zones_pu_raster) <- rep("zone_1", 3)
  sim_features <- get_sim_features()
  rel_tol <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.45), ncol = 1)
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
    add_hier_approach(rel_tol = rel_tol, verbose = FALSE) %>%
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

test_that("format (generic solver, single solution)", {
  skip_on_cran()
  skip_if_not_installed("highs")
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  names(sim_zones_pu_raster) <- rep("zone_1", 3)
  sim_features <- get_sim_features()
  rel_tol <- c(0.1)
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
    add_hier_approach(rel_tol = rel_tol, verbose = FALSE) %>%
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

test_that("format (generic solver, multiple solutions)", {
  skip_on_cran()
  skip_if_not_installed("highs")
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  names(sim_zones_pu_raster) <- rep("zone_1", 3)
  sim_features <- get_sim_features()
  rel_tol <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.45), ncol = 1)
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
    add_hier_approach(rel_tol = rel_tol, verbose = FALSE) %>%
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

test_that("expected trade-off behavior (gurobi solver)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  names(sim_zones_pu_raster) <- rep("zone_1", 3)
  sim_features <- get_sim_features()
  # prepare data
  b <- 0.2 * terra::global(sim_zones_pu_raster[[1]], "sum", na.rm = TRUE)[[1]]
  f1 <- sim_features[[1]]
  f2 <- sim_features[[2]]
  rel_tol <- matrix(c(0.01, 0.99, 0), byrow = TRUE, ncol = 1)
  priority <- matrix(c(2, 1, 2, 1, 1, 2), byrow = TRUE, ncol = 2)
  # build problem
  p <-
    multi_problem(
      obj1 =
        problem(sim_zones_pu_raster[[1]], f1) %>%
        add_max_wtd_sum_objective(budget = b) %>%
        add_binary_decisions(),
      obj2 =
        problem(sim_zones_pu_raster[[1]], f2) %>%
        add_max_wtd_sum_objective(budget = b) %>%
        add_binary_decisions()
    ) %>%
    add_hier_approach(
      rel_tol = rel_tol, priority = priority, verbose = FALSE
    ) %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, run_checks = FALSE)
  # extract solution statistics
  ## solution 1 has low tolerance for obj1,
  sol1_obj <- attr(s, "objective")["solution_1", , drop = FALSE]
  ## solution 2 has high tolerance for obj1
  sol2_obj <- attr(s, "objective")["solution_2", , drop = FALSE]
  ## solution 3 has low tolerance for obj2
  sol3_obj <- attr(s, "objective")["solution_2", , drop = FALSE]
  # run tests
  ## sol1 should have better obj1 because it has lower tolerance for obj1
  expect_gt(abs(sol1_obj[1, "obj1"]), abs(sol2_obj[1, "obj1"]))
  ## sol2 should have better obj2 because it has higher tolerance for obj1
  expect_gt(abs(sol2_obj[1, "obj2"]), abs(sol1_obj[1, "obj2"]))
  ## sol3 should have best obj2 because it obj2 has higher priority and zero
  ## tolerance for obj2
  expect_gt(abs(sol3_obj[1, "obj2"]), abs(sol1_obj[1, "obj2"]))
  expect_gte(abs(sol3_obj[1, "obj2"]), abs(sol2_obj[1, "obj2"]))
})

test_that("expected trade-off behavior (generic solver, verbose = TRUE)", {
  skip_on_cran()
  skip_if_not_installed("highs")
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  names(sim_zones_pu_raster) <- rep("zone_1", 3)
  sim_features <- get_sim_features()
  # prepare data
  b <- 0.2 * terra::global(sim_zones_pu_raster[[1]], "sum", na.rm = TRUE)[[1]]
  f1 <- sim_features[[1]]
  f2 <- sim_features[[2]]
  rel_tol <- matrix(c(0.01, 0.99, 0), byrow = TRUE, ncol = 1)
  priority <- matrix(c(2, 1, 2, 1, 1, 2), byrow = TRUE, ncol = 2)
  # build problem
  p <-
    multi_problem(
      obj1 =
        problem(sim_zones_pu_raster[[1]], f1) %>%
        add_max_wtd_sum_objective(budget = b) %>%
        add_binary_decisions(),
      obj2 =
        problem(sim_zones_pu_raster[[1]], f2) %>%
        add_max_wtd_sum_objective(budget = b) %>%
        add_binary_decisions()
    ) %>%
    add_hier_approach(
      rel_tol = rel_tol, priority = priority, verbose = TRUE
    ) %>%
    add_highs_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, run_checks = FALSE)
  # extract solution statistics
  ## solution 1 has low tolerance for obj1,
  sol1_obj <- attr(s, "objective")["solution_1", , drop = FALSE]
  ## solution 2 has high tolerance for obj1
  sol2_obj <- attr(s, "objective")["solution_2", , drop = FALSE]
  ## solution 3 has low tolerance for obj2
  sol3_obj <- attr(s, "objective")["solution_2", , drop = FALSE]
  # run tests
  ## sol1 should have better obj1 because it has lower tolerance for obj1
  expect_gt(abs(sol1_obj[1, "obj1"]), abs(sol2_obj[1, "obj1"]))
  ## sol2 should have better obj2 because it has higher tolerance for obj1
  expect_gt(abs(sol2_obj[1, "obj2"]), abs(sol1_obj[1, "obj2"]))
  ## sol3 should have best obj2 because it obj2 has higher priority and zero
  ## tolerance for obj2
  expect_gt(abs(sol3_obj[1, "obj2"]), abs(sol1_obj[1, "obj2"]))
  expect_gte(abs(sol3_obj[1, "obj2"]), abs(sol2_obj[1, "obj2"]))
})

test_that("infeasibility (highs)", {
  skip_on_cran()
  skip_if_not_installed("highs")
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create multi-objective problem with infeasible constraints
  p <-
    multi_problem(
      obj1 =
        problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.99) %>%
        add_linear_constraints(0, "<=", sim_pu_raster) %>%
        add_binary_decisions(),
      obj2 =
        problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.99) %>%
        add_linear_constraints(0, "<=", sim_pu_raster) %>%
        add_binary_decisions()
    ) %>%
    add_hier_approach(rel = 0.5, verbose = FALSE) %>%
    add_highs_solver(verbose = FALSE)
  # test
  expect_tidy_error(solve(p), "solution")
})

test_that("infeasibility (gurobi)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create multi-objective problem with infeasible constraints
  p <-
    multi_problem(
      obj1 =
        problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.99) %>%
        add_linear_constraints(0, "<=", sim_pu_raster) %>%
        add_binary_decisions(),
      obj2 =
        problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.99) %>%
        add_linear_constraints(0, "<=", sim_pu_raster) %>%
        add_binary_decisions()
    ) %>%
    add_hier_approach(rel = 0.5, verbose = FALSE) %>%
    add_gurobi_solver(verbose = FALSE)
  # test
  expect_tidy_error(solve(p), "solution")
})

test_that("invalid inputs", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  names(sim_zones_pu_raster) <- rep("zone_1", 3)
  sim_features <- get_sim_features()
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
  ## rel_tol
  expect_tidy_error(
    add_hier_approach(p, rel_tol = "a"),
    "numeric"
  )
  expect_tidy_error(
    add_hier_approach(p, rel_tol = c(NA_real_)),
    "missing"
  )
  expect_tidy_error(
    add_hier_approach(p, rel_tol = c(0.2, 0.3)),
    "value for each constraint"
  )
  expect_tidy_error(
    add_hier_approach(
      p, rel_tol = matrix(c(0.2, 0.3, 0.9), nrow = 1)
    ),
    "column for each constraint"
  )
  ## priority
  expect_tidy_error(
    add_hier_approach(
      p, rel_tol = c(0.1), priority = "a"
    ),
    "numeric"
  )
  expect_tidy_error(
    add_hier_approach(
      p, rel_tol = c(0.1), priority = c(0.1, NA)
    ),
    "missing"
  )
  expect_tidy_error(
    add_hier_approach(
      p, rel_tol = c(0.1), priority = c(0.2, 0.3, 0.9)
    ),
    "value for each problem"
  )
  expect_tidy_error(
    add_hier_approach(
      p, rel_tol = matrix(c(0.1), nrow = 1),
      priority = matrix(c(0.2, 0.3, 0.9), nrow = 1)
    ),
    "column for each problem"
  )
  ## cross checks between priority and rel_tol
  expect_tidy_error(
    add_hier_approach(
      p, rel_tol = matrix(c(0.1), nrow = 1),
      priority = matrix(c(0.2, 0.3), ncol = 2, nrow = 2)
    ),
    "same number of rows"
  )
  expect_tidy_error(
    add_hier_approach(
      p, rel_tol = c(0.1),
      priority = matrix(c(0.2, 0.3), nrow = 1)
    ),
    "both have the same class"
  )
  ## verbose
  expect_tidy_error(
    add_hier_approach(
      p, rel_tol = c(0.1), verbose = NA_character_
    ),
    "logical"
  )
})
