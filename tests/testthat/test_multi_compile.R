test_that("dual min set problems (single zone)", {
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  
  p1 <- problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  
  p2 <- problem(sim_zones_pu_raster[[2]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
    add_binary_decisions()
  
  p <- multi_problem(obj1 = p1, obj2 = p2)
  
  o1 <- multi_compile(p)
  o2 <- helper_ws_multi_compile(list(p1, p2))
  
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
  ## optimization problem  
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("dual min shortfall problems (single zone)", {
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  
  b1 <- 0.2 * terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]]
  b2 <- 0.3 * terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
  
  p1 <- problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_min_shortfall_objective(budget = b1) %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  
  p2 <- problem(sim_zones_pu_raster[[2]], sim_features) %>%
    add_min_shortfall_objective(budget = b2) %>%
    add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
    add_binary_decisions()
  
  p <- multi_problem(obj1 = p1, obj2 = p2)
  
  o1 <- multi_compile(p)
  o2 <- helper_ws_multi_compile(list(p1, p2))
  
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
  ## optimization problem 
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("min shortfall and min set problems (single zone)", {
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  
  p1 <- problem(sim_zones_pu_raster[[1]], sim_features) %>% # can also name objectives
    add_min_set_objective() %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  
  p2 <- problem(sim_zones_pu_raster[[2]], sim_features) %>%
    add_min_shortfall_objective(
      budget = 0.2 * terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
    ) %>%
    add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
    add_binary_decisions()
  
  # create multi-object problem
  p <-
    multi_problem(
      obj1 = p1,
      obj2 = p2
    )
  
  o1 <- multi_compile(p)
  o2 <- helper_ws_multi_compile(list(p1, p2))
  
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
  ## optimization problem
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("three problems (single zone)", {
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  
  p1 <- problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  
  p2 <- problem(sim_zones_pu_raster[[2]], sim_features) %>%
    add_min_shortfall_objective(
      budget = 0.2 * terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
    ) %>%
    add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
    add_binary_decisions()
  
  p3 <- problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(rep(1, terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  
  p <- multi_problem(obj1 = p1, obj2 = p2, obj3 = p3)
  
  o1 <- multi_compile(p)
  o2 <- helper_ws_multi_compile(list(p1, p2, p3))
  
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  
  expect_equal(nrow(o1$obj), 3)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  
  expect_equal(o1$modelsense, c("min", "min", "min"))
  
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("dual min set problems (multiple zones)", {
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  
  targets <- matrix(
    seq_len(terra::nlyr(sim_features)),
    nrow = terra::nlyr(sim_features),
    ncol = 2
  )
  
  p1 <- problem(
    c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
    zones(
      z1 = sim_features,
      z2 = sim_features
    )
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(seq_len(terra::nlyr(sim_features)), nrow = terra::nlyr(sim_features), ncol = 2)
    ) %>%
    add_binary_decisions()
  
  p2 <- problem(
    c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
    zones(
      z1 = sim_features,
      z2 = sim_features
    )
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(rev(seq_len(terra::nlyr(sim_features))), nrow = terra::nlyr(sim_features), ncol = 2)
    ) %>%
    add_binary_decisions()
  
  p <- multi_problem(obj1 = p1, obj2 = p2)
  
  o1 <- multi_compile(p)
  o2 <- helper_ws_multi_compile(list(p1, p2))
  
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  
  expect_equal(nrow(o1$obj), 2)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  
  expect_equal(o1$modelsense, c("min", "min"))
  
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
  
})

test_that("dual min shortfall problems (multiple zones)", {
  
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  
  budgets <- 0.25 * c(
    terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]],
    terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
  )
  
  p1 <- problem(
    c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
    zones(
      z1 = sim_features,
      z2 = sim_features
    )
  ) %>%
    add_min_shortfall_objective(budget = budgets) %>%
    add_absolute_targets(
      matrix(seq_len(terra::nlyr(sim_features)), nrow = terra::nlyr(sim_features), ncol = 2)
    ) %>%
    add_binary_decisions()
  
  p2 <- problem(
    c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
    zones(
      z1 = sim_features,
      z2 = sim_features
    )
  ) %>%
    add_min_shortfall_objective(budget = budgets) %>%
    add_absolute_targets(
      matrix(rev(seq_len(terra::nlyr(sim_features))), nrow = terra::nlyr(sim_features), ncol = 2)
    ) %>%
    add_binary_decisions()
  
  p <- multi_problem(obj1 = p1, obj2 = p2)
  
  o1 <- multi_compile(p)
  o2 <- helper_ws_multi_compile(list(p1, p2))
  
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  
  expect_equal(nrow(o1$obj), 2)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  
  expect_equal(o1$modelsense, c("min", "min"))
  
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("min shortfall and min set problems (multiple zones)", {
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  
  budgets <- 0.2 * c(
    terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]],
    terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
  )
  
  p1 <- problem(
    c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
    zones(
      z1 = sim_features,
      z2 = sim_features
    )
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(seq_len(terra::nlyr(sim_features)), nrow = terra::nlyr(sim_features), ncol = 2)
    ) %>%
    add_binary_decisions()
  
  p2 <- problem(
    c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
    zones(
      z1 = sim_features,
      z2 = sim_features
    )
  ) %>%
    add_min_shortfall_objective(budget = budgets) %>%
    add_absolute_targets(
      matrix(rev(seq_len(terra::nlyr(sim_features))), nrow = terra::nlyr(sim_features), ncol = 2)
    ) %>%
    add_binary_decisions()
  
  p <- multi_problem(obj1 = p1, obj2 = p2)
  
  o1 <- multi_compile(p)
  o2 <- helper_ws_multi_compile(list(p1, p2))
  
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  
  expect_equal(nrow(o1$obj), 2)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  
  expect_equal(o1$modelsense, c("min", "min"))
  
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
  
})

test_that("three problems (multiple zones)", {
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  
  
  budgets <- 0.2 * c(
    terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]],
    terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
  )
  
  p1 <- problem(
    c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
    zones(z1 = sim_features, z2 = sim_features)
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(seq_len(terra::nlyr(sim_features)), nrow = terra::nlyr(sim_features), ncol = 2)
    ) %>%
    add_binary_decisions()
  
  p2 <- problem(
    c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
    zones(z1 = sim_features, z2 = sim_features)
  ) %>%
    add_min_shortfall_objective(budget = budgets) %>%
    add_absolute_targets(
      matrix(rev(seq_len(terra::nlyr(sim_features))), nrow = terra::nlyr(sim_features), ncol = 2)
    ) %>%
    add_binary_decisions()
  
  p3 <- problem(
    c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
    zones(z1 = sim_features, z2 = sim_features)
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(1, nrow = terra::nlyr(sim_features), ncol = 2)
    ) %>%
    add_binary_decisions()
  
  p <- multi_problem(obj1 = p1, obj2 = p2, obj3 = p3)
  
  o1 <- multi_compile(p)
  o2 <- helper_ws_multi_compile(list(p1, p2, p3))
  
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  
  expect_equal(nrow(o1$obj), 3)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  
  expect_equal(o1$modelsense, c("min", "min", "min"))
  
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("invalid inputs", {
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  
  p <- problem(sim_zones_pu_raster[[1]], sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  
  expect_error(
    multi_problem(obj1 = p),
    "at least two"
  )
  
  ## mismatched number of planning units
  sim_zones_pu_raster2 <- get_sim_zones_pu_raster()
  
  cost <- sim_zones_pu_raster2[[1]]
  
  ### create a mask that removes some cells
  mask <- cost
  terra::values(mask) <- 1
  terra::values(mask)[seq_len(floor(length(terra::values(mask)) / 2))] <- NA
  
  cropped_cost <- terra::mask(cost, mask)
  
  p2 <- problem(cropped_cost, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
    add_binary_decisions()
  
  expect_error(
    multi_problem(obj1 = p, obj2 = p2)
  )
  
  ## mismatched number of zones
  sim_zones_pu_raster_multi <- get_sim_zones_pu_raster()
  
  p_multi_zone <- problem(
    c(sim_zones_pu_raster_multi[[1]], sim_zones_pu_raster_multi[[2]]),
    zones(
      z1 = sim_features,
      z2 = sim_features
    )
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(seq_len(terra::nlyr(sim_features)), nrow = terra::nlyr(sim_features), ncol = 2)
    ) %>%
    add_binary_decisions()
  
  expect_error(
    multi_problem(obj1 = p, obj2 = p_multi_zone)
  )
  
  ## incompatible objectives (no objective added) # TODO, currently fails because doesn't throw an error
  # p_no_obj <- problem(sim_zones_pu_raster[[1]], sim_features) %>%
  #   add_binary_decisions()
  # 
  # expect_error(
  #   multi_problem(obj1 = p, obj2 = p_no_obj),
  #   "objective"
  # )
})
