test_that("compile (max cover, compressed formulation, single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate budget
  b <- floor(terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]]) * 0.25
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = b) %>%
    add_feature_weights(seq(10, 14))
  o <- compile(p)
  # calculate variables for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- terra::nlyr(sim_features)
  scaled_costs <- p$planning_unit_costs()
  scaled_costs <- scaled_costs * (-0.01 / sum(scaled_costs, na.rm = TRUE))
  # tests
  expect_equal(o$modelsense(), "max")
  expect_equal(o$obj(), c(scaled_costs, seq(10, 14)))
  expect_equal(o$sense(), c(rep(">=", n_f), "<="))
  expect_equal(o$rhs(), c(rep(0, n_f), b))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("present", n_f)))
  expect_equal(o$row_ids(), c(rep("spp_present", n_f), "budget"))
  expect_true(
    all(o$A()[seq_len(n_f), seq_len(n_pu)] == p$data$rij_matrix[[1]])
  )
  expect_equal(
    o$A()[n_f + 1, ],
    c(p$planning_unit_costs(), rep(0, n_f))
  )
  expect_true(
    all(
      o$A()[seq_len(n_f), n_pu + seq_len(n_f)] ==
      triplet_sparse_matrix(
        i = seq_len(n_f), j = seq_len(n_f), x = rep(-1, n_f)
      )
    )
  )
  expect_equal(o$lb(), rep(0, n_f + n_pu))
  expect_equal(o$ub(), rep(1, n_f + n_pu))
})

test_that("compile (min shortfall, compressed formulation, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate budget
  b <- floor(terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.2)
  # calculate targets data
  targ <- floor(terra::global(sim_features, "sum", na.rm = TRUE)[[1]] * 0.25)
  targ[2] <- 0
  # set weights
  w <- round(runif(terra::nlyr(sim_features)), 3)
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(budget = b) %>%
    add_feature_weights(w) %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  o <- compile(p)
  # calculations for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_features <- terra::nlyr(sim_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(rep(0, n_pu), replace(1 / targ, 2, 0) * w))
  expect_equal(o$sense(), c(rep(">=", n_features), "<="))
  expect_equal(o$rhs(), c(targ, b))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("spp_met", n_features)))
  expect_equal(o$row_ids(), c(rep("spp_target", n_features), "budget"))
  expect_true(
    all(o$A()[seq_len(n_features), seq_len(n_pu)] == p$data$rij_matrix[[1]])
  )
  expect_equal(
    o$A()[n_features + 1, ],
    c(p$planning_unit_costs(), rep(0, n_features))
  )
  expect_true(
    all(
      o$A()[seq_len(n_features), n_pu + seq_len(n_features)] ==
      triplet_sparse_matrix(
        i = seq_len(n_features), j = seq_len(n_features), x = 1
      )
    )
  )
  expect_true(all(o$lb() == 0))
  expect_equal(o$ub(), c(rep(1, n_pu), rep(Inf, n_features)))
})

test_that("solve (max utility, compressed formulation, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- 4.23
  cost <- terra::rast(matrix(c(1, 2, NA, 4), nrow = 1))
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), nrow = 1)),
    terra::rast(matrix(c(10, 10, 10, 10), nrow = 1)),
    terra::rast(matrix(c(0, 0, 0, 2), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_max_utility_objective(budget = budget) %>%
    add_feature_weights(c(1, 1, 100)) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # tests
  expect_equal(c(terra::values(s1)), c(0, 0, NA, 1))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("compile (expanded formulation, single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate budget
  b <- floor(terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]]) * 0.25
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = b) %>%
    add_feature_weights(seq(10, 14))
  o <- compile(p, compressed_formulation = FALSE)
  # calculate variables for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- terra::nlyr(sim_features)
  rij <- rij_matrix(sim_pu_raster, sim_features)
  scaled_costs <- p$planning_unit_costs()
  scaled_costs <- scaled_costs * (-0.01 / sum(scaled_costs, na.rm = TRUE))
  # tests
  expect_equal(o$modelsense(), "max")
  expect_equal(o$obj(), c(scaled_costs, rep(0, n_pu * n_f), seq(10, 14)))
  expect_equal(o$sense(), c(rep("<=", n_pu * n_f), rep( ">=", n_f), "<="))
  expect_equal(o$rhs(), c(rep(0, n_f * n_pu), rep(0, n_f), b))
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu), rep("pu_ijz", n_pu * n_f), rep("present", n_f))
  )
  expect_equal(
    o$row_ids(),
    c(rep("pu_ijz", n_f * n_pu), rep("spp_present", n_f), "budget")
  )
  expect_equal(o$lb(), rep(0, n_pu + (n_f * n_pu) + n_f))
  expect_equal(o$ub(), rep(1, n_pu + (n_f * n_pu) + n_f))
  # check that problem matrix is correct
  row <- 0
  for (i in seq_len(n_f)) {
    for (j in seq_len(n_pu)) {
      row <- row + 1
      curr_row <- rep(0, n_pu + (n_pu * n_f) + n_f)
      curr_row[j] <- -1
      curr_row[n_pu + ( (i - 1) * n_pu) + j] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
  }
  for (i in seq_len(n_f)) {
    curr_row <- rep(0, n_pu + (n_pu * n_f) + n_f)
    curr_row[(i * n_pu) + seq_len(n_pu)] <- rij[i, ]
    curr_row[n_pu + (n_f * n_pu) + i] <- -1
    expect_equal(o$A()[(n_f * n_pu) + i, ], curr_row)
  }
  expect_equal(
    o$A()[(n_pu * n_f) + n_f + 1, ],
    c(p$planning_unit_costs(), rep(0, n_f * n_pu), rep(0, n_f))
  )
})

test_that("solve (expanded formulation, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- 4.23
  cost <- terra::rast(matrix(c(1, 2, NA, 4), nrow = 1))
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), nrow = 1)),
    terra::rast(matrix(c(10, 10, 10, 10), nrow = 1)),
    terra::rast(matrix(c(0, 0, 0, 2), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_max_utility_objective(budget = budget) %>%
    add_feature_weights(c(1, 1, 100)) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # tests
  expect_equal(c(terra::values(s)), c(0, 0, NA, 1))
})

test_that("invalid inputs (single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # check that invalid arguments result in errors
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_feature_weights(seq_len(4))
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_feature_weights(c(-1, 1, 2, 3, 4))
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_feature_weights(c(1, NA, 2, 3, 4))
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_feature_weights(c(1, Inf, 2, 3, 4))
  })
})

test_that("compile (compressed formulation, multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate budget
  b <- min(
    floor(terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]]) * 0.25
  )
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = b) %>%
    add_feature_weights(matrix(10 + seq_len(15), ncol = 3))
  o <- compile(p)
  # calculate variables for tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  scaled_costs <- p$planning_unit_costs()
  scaled_costs <- scaled_costs * (-0.01 / sum(scaled_costs, na.rm = TRUE))
  # tests
  expect_equal(o$modelsense(), "max")
  expect_equal(o$obj(), c(scaled_costs, seq(11, 25)))
  expect_equal(
    o$sense(),
    c(rep(">=", n_f * n_z), "<=", rep("<=", n_pu))
  )
  expect_equal(o$rhs(), c(rep(0, n_f * n_z), b, rep(1, n_pu)))
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu * n_z), rep("present", n_f * n_z))
  )
  expect_equal(
    o$row_ids(),
    c(rep("spp_present", n_f * n_z), "budget", rep("pu_zone", n_pu))
  )
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + (n_f * n_z)))
  expect_equal(o$ub(), rep(1, (n_pu * n_z) + (n_f * n_z)))
  # check that problem matrix is correct
  m <- matrix(
    0, nrow = (n_f * n_z) + 1 + n_pu,
    ncol = (n_pu * n_z) + (n_z * n_f)
  )
  counter <- 0
  for (z in seq_len(n_z)) {
    for (f in seq_len(n_f)) {
      counter <- counter + 1
      m[counter, ((z - 1) * n_pu) + seq_len(n_pu)] <-
        p$data$rij_matrix[[z]][f, ]
      m[counter, (n_z * n_pu) + ((z - 1) * n_f) + f] <- -1
    }
  }
  counter <- counter + 1
  m[counter, seq_len(n_pu * n_z)] <- c(p$planning_unit_costs())
  for (i in seq_len(n_pu)) {
    m[i + counter, i] <- 1
    m[i + counter, n_pu + i] <- 1
    m[i + counter, (2 * n_pu) + i] <- 1
  }
  m <- as_Matrix(m, "Matrix")
  expect_true(all(o$A() == m))
})

test_that("solve (compressed formulation, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- 20
  cost <- c(
    terra::rast(matrix(c(5,  6,  7,  8,  NA, NA), nrow = 1)),
    terra::rast(matrix(c(11, 12, 13, 14, NA, 15), nrow = 1))
  )
  features <- c(
    # zone 1
    terra::rast(matrix(c(1,  1,  1, 1, 1, 1), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1, 1, 1, 1), nrow = 1)),
    terra::rast(matrix(c(2,  1,  1, 1, 1, 1), nrow = 1)),
    # zone 2
    terra::rast(matrix(c(1,  1,  1, 1, 1, 1), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1, 1, 1, 1), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1, 1, 1, 3), nrow = 1))
  )
  names(features) <- rep(c("layer.1", "layer.2", "layer.3"), 2)
  targs <- tibble::tibble(
    feature = c("layer.1", "layer.2", "layer.3"),
    zone = list("1", "1", c("1", "2")),
    sense = rep(">=", 3),
    type = "absolute",
    target = c(1, 1, 5),
    weight = c(1, 1, 100)
  )
  # create problem
  p <-
    problem(
      cost,
      zones(features[[1:3]], features[[4:6]], feature_names = targs$feature)
    ) %>%
    add_max_features_objective(budget = budget) %>%
    add_manual_targets(targs[, -6]) %>%
    add_feature_weights(matrix(targs$weight, ncol = 1)) %>%
    add_default_solver(gap = 0, verbose = TRUE)
  # solve problem
  s <- solve(p)
  # tests
  expect_equal(c(terra::values(s[[1]])), c(1, 0, 0, 0, NA, NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 0, 0, 0, NA, 1))
})

test_that("compile (expanded formulation, multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate budget
  b <- min(
    floor(terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)) * 0.25
  )
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = b) %>%
    add_feature_weights(matrix(10 + seq_len(15), ncol = 3))
  o <- compile(p, FALSE)
  # calculate variables for tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  scaled_costs <- p$planning_unit_costs()
  scaled_costs <- scaled_costs * (-0.01 / sum(scaled_costs, na.rm = TRUE))
  # tests
  expect_equal(o$modelsense(), "max")
  expect_equal(o$obj(), c(scaled_costs, rep(0, n_pu * n_f * n_z), seq(11, 25)))
  expect_equal(
    o$sense(),
    c(rep("<=", n_f * n_z * n_pu), rep(">=", n_f * n_z), "<=", rep("<=", n_pu))
  )
  expect_equal(
    o$rhs(),
    c(rep(0, n_pu * n_z * n_f), rep(0, n_f * n_z), b, rep(1, n_pu))
  )
  expect_equal(
    o$col_ids(),
    c(
      rep("pu", n_pu * n_z), rep("pu_ijz", n_pu * n_z * n_f),
      rep("present", n_f * n_z)
    )
  )
  expect_equal(
    o$row_ids(),
    c(
      rep("pu_ijz", n_pu * n_z * n_f), rep("spp_present", n_f * n_z),
      "budget", rep("pu_zone", n_pu)
    )
  )
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + (n_pu * n_z * n_f) + (n_f * n_z)))
  expect_equal(o$ub(), rep(1, (n_pu * n_z) + (n_pu * n_z * n_f) + (n_f * n_z)))
  # check that problem matrix is correct
  m <- matrix(
    0, nrow = (n_pu * n_z * n_f) + (n_f * n_z) + 1 + n_pu,
    ncol = (n_pu * n_z) + (n_pu * n_z * n_f) + (n_z * n_f)
  )
  counter <- 0
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_f)) {
      for (j in seq_len(n_pu)) {
        counter <- counter + 1
        idx1 <- ((z - 1) * n_pu) + j
        idx2 <- (n_pu * n_z) + ((z - 1) * n_pu * n_f) + ((i - 1) * n_pu) + j
        m[counter, idx1] <- -1
        m[counter, idx2] <- 1
      }
    }
  }
  for (z in seq_len(n_z)) {
    for (f in seq_len(n_f)) {
      counter <- counter + 1
      for (pu in seq_len(n_pu)) {
        col <- (n_pu * n_z) + ((z - 1) * n_f * n_pu) + ((f - 1) * n_pu) + pu
        m[counter, col] <- p$data$rij_matrix[[z]][f, pu]
      }
      col <- (n_pu * n_z) + (n_pu * n_f * n_z) + ((z - 1) * n_f) + f
      m[counter, col] <- -1
    }
  }
  counter <- counter + 1
  m[counter, seq_len(n_pu * n_z)] <- c(p$planning_unit_costs())
  for (i in seq_len(n_pu)) {
    m[i + counter, i] <- 1
    m[i + counter, n_pu + i] <- 1
    m[i + counter, (2 * n_pu) + i] <- 1
  }
  m <- as_Matrix(m, "Matrix")
  expect_true(all(o$A() == m))
})

test_that("solve (expanded formulation, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- 20
  cost <- c(
    terra::rast(matrix(c(5,  6,  7,  8,  NA, NA), nrow = 1)),
    terra::rast(matrix(c(11, 12, 13, 14, NA, 15), nrow = 1))
  )
  features <- c(
    # zone 1
    terra::rast(matrix(c(1,  1,  1, 1, 1, 1), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1, 1, 1, 1), nrow = 1)),
    terra::rast(matrix(c(2,  1,  1, 1, 1, 1), nrow = 1)),
    # zone 2
    terra::rast(matrix(c(1,  1,  1, 1, 1, 1), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1, 1, 1, 1), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1, 1, 1, 3), nrow = 1))
  )
  targs <- tibble::tibble(
    feature = c("layer.1", "layer.2", "layer.3"),
    zone = list("1", "1", c("1", "2")),
    sense = rep(">=", 3),
    type = "absolute",
    target = c(1, 1, 5),
    weight = c(1, 1, 100)
  )
  # create problem
  p <-
    problem(
      cost,
      zones(features[[1:3]], features[[4:6]], feature_names = targs$feature)
    ) %>%
    add_max_features_objective(budget = budget) %>%
    add_manual_targets(targs[, -6]) %>%
    add_feature_weights(matrix(targs$weight, ncol = 1)) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_equal(c(terra::values(s[[1]])), c(1, 0, 0, 0, NA, NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 0, 0, 0, NA, 1))
})

test_that("invalid inputs (multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # tests
  expect_tidy_error({
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = c(1, -5, 1))
  })
  expect_tidy_error({
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = c(1, NA, 1))
  })
  expect_tidy_error({
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = c(NA, NA, NA))
  })
  expect_tidy_error({
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = c(1, Inf, 9))
  })
  expect_tidy_error({
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = c(1, Inf, 9))
  })
})

test_that("throw warning with min set objective", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <- p1 %>% add_feature_weights(runif(terra::nlyr(sim_features)))
  # compile problems
  o1 <- compile(p1)
  expect_warning({o2 <- compile(p2)}, "ignored")
  # tests
  expect_equal(o1$modelsense(), o2$modelsense())
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$col_ids(), o2$col_ids())
  expect_equal(o1$row_ids(), o2$row_ids())
  expect_equal(o1$A(), o2$A())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
})

test_that("throw warning with min largest shortfall objective", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_largest_shortfall_objective(100) %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <- p1 %>% add_feature_weights(runif(terra::nlyr(sim_features)))
  # compile problems
  o1 <- compile(p1)
  expect_warning({o2 <- compile(p2)}, "ignored")
  # tests
  expect_equal(o1$modelsense(), o2$modelsense())
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$col_ids(), o2$col_ids())
  expect_equal(o1$row_ids(), o2$row_ids())
  expect_equal(o1$A(), o2$A())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
})
