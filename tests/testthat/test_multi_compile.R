test_that("dual min set problems (single zone)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  # build problems
  p1 <-
    problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_zones_pu_raster[[2]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
    add_binary_decisions()
  # build multi-objective problem
  p <- multi_problem(obj1 = p1, obj2 = p2)
  # compile problem
  o1 <- multi_compile(p)
  # compile problem with helper function that has correct result
  o2 <- helper_ws_multi_compile(list(p1, p2))
  # tests
  ## structure
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  ## obj
  expect_equal(nrow(o1$obj), 2)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  ## modelsense
  expect_equal(o1$modelsense, c("min", "min"))
  ## optimization problem components
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("dual min shortfall problems (single zone)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  # set budgets
  b1 <- 0.2 * terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]]
  b2 <- 0.3 * terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
  # build problems
  p1 <-
    problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_min_shortfall_objective(budget = b1) %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_zones_pu_raster[[2]], sim_features) %>%
    add_min_shortfall_objective(budget = b2) %>%
    add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
    add_binary_decisions()
  # build multi-objective problem
  p <- multi_problem(obj1 = p1, obj2 = p2)
  # compile problem
  o1 <- multi_compile(p)
  # compile problem with helper function that has correct result
  o2 <- helper_ws_multi_compile(list(p1, p2))
  # run tests
  ## structure
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  ## obj
  expect_equal(nrow(o1$obj), 2)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  ## modelsense
  expect_equal(o1$modelsense, c("min", "min"))
  ## optimization problem components
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("min shortfall and min set problems (single zone)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  # set budget
  b <- 0.2 * terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
  # build problems
  p1 <-
    problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_zones_pu_raster[[2]], sim_features) %>%
    add_min_shortfall_objective(budget = b) %>%
    add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
    add_binary_decisions()
  # build multi-object problem
  p <- multi_problem(obj1 = p1, obj2 = p2)
  # compile problem
  o1 <- multi_compile(p)
  # compile problem with helper function that has correct result
  o2 <- helper_ws_multi_compile(list(p1, p2))
  # run tests
  ## structure
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  ## obj
  expect_equal(nrow(o1$obj), 2)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  ## modelsense
  expect_equal(o1$modelsense, c("min", "min"))
  ## optimization problem components
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("three problems (single zone)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  # set budget
  b <- 0.2 * terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
  # build problems
  p1 <-
    problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_zones_pu_raster[[2]], sim_features) %>%
    add_min_shortfall_objective(budget = b) %>%
    add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
    add_binary_decisions()
  p3 <- problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(rep(1, terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  # build problem
  p <- multi_problem(obj1 = p1, obj2 = p2, obj3 = p3)
  # compile problem
  o1 <- multi_compile(p)
  # compile problem with helper function that has correct result
  o2 <- helper_ws_multi_compile(list(p1, p2, p3))
  # run tests
  ## structure
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  ## obj
  expect_equal(nrow(o1$obj), 3)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  ## modelsense
  expect_equal(o1$modelsense, c("min", "min", "min"))
  ## optimization problem components
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("dual min set problems (multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  # set targets
  targets <- matrix(
    seq_len(terra::nlyr(sim_features)),
    nrow = terra::nlyr(sim_features), ncol = 2
  )
  # build problems
  p1 <-
    problem(
      c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
      zones(z1 = sim_features, z2 = sim_features)
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(
        seq_len(terra::nlyr(sim_features)),
        nrow = terra::nlyr(sim_features), ncol = 2
      )
    ) %>%
    add_binary_decisions()
  p2 <-
    problem(
      c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
      zones(z1 = sim_features, z2 = sim_features)
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(
        rev(seq_len(terra::nlyr(sim_features))),
        nrow = terra::nlyr(sim_features), ncol = 2
      )
    ) %>%
    add_binary_decisions()
  # build multi-objective problem
  p <- multi_problem(obj1 = p1, obj2 = p2)
  # compile problem
  o1 <- multi_compile(p)
  # compile problem with helper function that has correct result
  o2 <- helper_ws_multi_compile(list(p1, p2))
  # run tests
  ## structure
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  ## obj
  expect_equal(nrow(o1$obj), 2)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  ## modelsense
  expect_equal(o1$modelsense, c("min", "min"))
  ## optimization problem components
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("dual min shortfall problems (multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  # set budgets
  budgets <- 0.25 * c(
    terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]],
    terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
  )
  # build problems
  p1 <-
    problem(
      c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
      zones(z1 = sim_features, z2 = sim_features)
    ) %>%
    add_min_shortfall_objective(budget = budgets) %>%
    add_absolute_targets(
      matrix(
        seq_len(terra::nlyr(sim_features)),
        nrow = terra::nlyr(sim_features), ncol = 2
      )
    ) %>%
    add_binary_decisions()
  p2 <-
    problem(
      c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
      zones(z1 = sim_features, z2 = sim_features)
    ) %>%
    add_min_shortfall_objective(budget = budgets) %>%
    add_absolute_targets(
      matrix(
        rev(seq_len(terra::nlyr(sim_features))),
        nrow = terra::nlyr(sim_features), ncol = 2
      )
    ) %>%
    add_binary_decisions()
  # build problem
  p <- multi_problem(obj1 = p1, obj2 = p2)
  # compile problem
  o1 <- multi_compile(p)
  # compile problem with helper function that has correct result
  o2 <- helper_ws_multi_compile(list(p1, p2))
  # run tests
  ## structure
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  ## obj
  expect_equal(nrow(o1$obj), 2)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  ## modelsense
  expect_equal(o1$modelsense, c("min", "min"))
  ## optimization problem components
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("min shortfall and min set problems (multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  ## set budgets
  budgets <- 0.2 * c(
    terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]],
    terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
  )
  # build problems
  p1 <-
    problem(
      c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
      zones(z1 = sim_features, z2 = sim_features)
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(
        seq_len(terra::nlyr(sim_features)),
        nrow = terra::nlyr(sim_features), ncol = 2
      )
    ) %>%
    add_binary_decisions()
  p2 <-
    problem(
      c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
      zones(z1 = sim_features, z2 = sim_features)
    ) %>%
    add_min_shortfall_objective(budget = budgets) %>%
    add_absolute_targets(
      matrix(
        rev(seq_len(terra::nlyr(sim_features))),
        nrow = terra::nlyr(sim_features), ncol = 2
      )
    ) %>%
    add_binary_decisions()
  # build problems
  p <- multi_problem(obj1 = p1, obj2 = p2)
  # compile problem
  o1 <- multi_compile(p)
  # compile problem with helper function that has correct result
  o2 <- helper_ws_multi_compile(list(p1, p2))
  # run tests
  ## structure
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  ## obj
  expect_equal(nrow(o1$obj), 2)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  ## modelsense
  expect_equal(o1$modelsense, c("min", "min"))
  ## optimization problem components
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("three problems (multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  # set budgets
  budgets <- 0.2 * c(
    terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]],
    terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
  )
  # build problems
  p1 <-
    problem(
      c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
      zones(z1 = sim_features, z2 = sim_features)
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(
        seq_len(terra::nlyr(sim_features)),
        nrow = terra::nlyr(sim_features), ncol = 2
      )
    ) %>%
    add_binary_decisions()
  p2 <-
    problem(
      c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
      zones(z1 = sim_features, z2 = sim_features)
    ) %>%
    add_min_shortfall_objective(budget = budgets) %>%
    add_absolute_targets(
      matrix(
        rev(seq_len(terra::nlyr(sim_features))),
        nrow = terra::nlyr(sim_features), ncol = 2
      )
    ) %>%
    add_binary_decisions()
  p3 <-
    problem(
      c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
      zones(z1 = sim_features, z2 = sim_features)
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(1, nrow = terra::nlyr(sim_features), ncol = 2)
    ) %>%
    add_binary_decisions()
  # build multi-objective problem
  p <- multi_problem(obj1 = p1, obj2 = p2, obj3 = p3)
  # compile problem
  o1 <- multi_compile(p)
  # compile problem with helper function that has correct result
  o2 <- helper_ws_multi_compile(list(p1, p2, p3))
  # run tests
  ## structure
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  ## obj
  expect_equal(nrow(o1$obj), 3)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  ## modelsense
  expect_equal(o1$modelsense, c("min", "min", "min"))
  ## optimization problem components
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("invalid inputs", {
  # run tests
  ## problem missing objective
  p1 <-
    problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  
  p2 <-
    problem(sim_zones_pu_raster[[2]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  
  mp <- multi_problem(obj1 = p1, obj2 = p2)
  
  expect_tidy_error(
    multi_compile(mp),
    "objective"
  )
  ## min set problem missing targets
  p1 <-
    problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_min_set_objective() %>%
    add_binary_decisions()
  
  p2 <-
    problem(sim_zones_pu_raster[[2]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  
  mp <- multi_problem(obj1 = p1, obj2 = p2)
  
  expect_tidy_error(
    multi_compile(mp),
    "targets"
  )
})
