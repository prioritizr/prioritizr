test_that("compile (compressed formulation, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate budget
  b <- floor(
    terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.25
  )
  # calculate targets data
  targ <- floor(
    terra::global(sim_features, "sum", na.rm = TRUE)[[1]] * 0.25
  )
  targ[2] <- 0
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_largest_shortfall_objective(budget = b) %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  o <- compile(p)
  # calculations for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- terra::nlyr(sim_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(rep(0, n_pu), 1))
  expect_equal(o$sense(), c(rep(">=", n_f), "<="))
  expect_equal(o$rhs(), c(targ, b))
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu), "max_shortfall")
  )
  expect_equal(
    o$row_ids(),
    c(rep("spp_target", n_f), "budget")
  )
  expect_true(
    all(o$A()[seq_len(n_f), seq_len(n_pu)] == p$data$rij_matrix[[1]])
  )
  expect_equal(
    o$A()[n_f + 1, ],
    c(p$planning_unit_costs(), 0)
  )
  expect_equal(o$A()[seq_len(n_f), n_pu + 1], targ)
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))
})

test_that("solution (compressed formulation, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- 4.23
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_largest_shortfall_objective(budget = budget) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_absolute_targets(c(2, 21)) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # tests
  expect_equal(c(terra::values(s1)), c(0, 1, 1, NA))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("compile (expanded formulation, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate budget
  b <- floor(terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.2)
  # calculate targets data
  targ <- floor(terra::global(sim_features, "sum", na.rm = TRUE)[[1]] * 0.25)
  targ[2] <- 0
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_largest_shortfall_objective(budget = b) %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  o <- compile(p, compressed_formulation = FALSE)
  # calculations for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- terra::nlyr(sim_features)
  rij <- rij_matrix(sim_pu_raster, sim_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(
    o$obj(),
    c(rep(0, n_pu), rep(0, n_pu * n_f), 1)
  )
  expect_equal(
    o$sense(),
    c(rep("<=", n_pu * n_f), rep(">=", n_f),  "<=")
  )
  expect_equal(
    o$rhs(),
    c(rep(0, n_pu * n_f), targ, b)
  )
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu), rep("pu_ijz", n_pu * n_f), "max_shortfall")
  )
  expect_equal(
    o$row_ids(),
    c(rep("pu_ijz", n_pu * n_f), rep("spp_target", n_f), "budget")
  )
  expect_equal(o$lb(), rep(0, n_pu + (n_pu * n_f) + 1))
  expect_equal(o$ub(), rep(1, n_pu + (n_pu * n_f) + 1))
  # test that model matrix has been constructed correctly
  row <- 0
  for (i in seq_len(n_f)) {
    for (j in seq_len(n_pu)) {
      row <- row + 1
      curr_row <- rep(0, n_pu + (n_pu * n_f) + 1)
      curr_row[j] <- -1
      curr_row[n_pu + ( (i - 1) * n_pu) + j] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
  }
  for (i in seq_len(n_f)) {
    curr_row <- rep(0, n_pu + (n_pu * n_f) + 1)
    curr_row[(i * n_pu) + seq_len(n_pu)] <- rij[i, ]
    curr_row[n_pu + (n_f * n_pu) + 1] <- targ[i]
    expect_equal(o$A()[(n_f * n_pu) + i, ], curr_row)
  }
  expect_equal(
    o$A()[(n_pu * n_f) + n_f + 1, ],
    c(p$planning_unit_costs(), rep(0, n_f * n_pu), 0)
  )
})

test_that("solution (expanded formulation, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- 4.23
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_largest_shortfall_objective(budget = budget) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_absolute_targets(c(2, 21)) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # tests
  expect_equal(c(terra::values(s)), c(0, 1, 1, NA))
})

test_that("invalid inputs (single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # tests
  expect_tidy_error(
    problem(sim_pu_raster, sim_features) %>%
      add_min_largest_shortfall_objective(budget = -5)
  )
  expect_tidy_error(
    problem(sim_pu_raster, sim_features) %>%
      add_min_largest_shortfall_objective(budget = NA)
  )
  expect_tidy_error(
    problem(sim_pu_raster, sim_features) %>%
      add_min_largest_shortfall_objective(budget = Inf)
  )
  expect_tidy_error(
    problem(sim_pu_raster, sim_pu_raster) %>%
    add_min_largest_shortfall_objective(budget = 5) %>%
    compile()
  )
})

test_that("compile (compressed formulation, multiple zones, scalar budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate budget
  b <- min(floor(
    terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.25
  ))
  # calculate targets data
  targs <- tibble::tibble(
    feature = feature_names(sim_zones_features)[1:3],
    zone = list("zone_1", "zone_2", c("zone_1", "zone_3")),
    sense = c(">=", "<=", ">="),
    target = c(5, 300, 10),
    type = c("absolute", "absolute", "absolute")
  )
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_largest_shortfall_objective(budget = b) %>%
    add_manual_targets(targs) %>%
    add_binary_decisions()
  o <- compile(p)
  # calculations for tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  n_t <- nrow(targs)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(
    o$obj(),
    c(rep(0, length(p$planning_unit_costs())), 1)
  )
  expect_equal(
    o$sense(),
    c(targs$sense, rep("<=", length(b)), rep("<=", n_pu))
  )
  expect_equal(
    o$rhs(),
    c(targs$target, b, rep(1, n_pu))
  )
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu * n_z), "max_shortfall")
  )
  expect_equal(
    o$row_ids(),
    c(rep("spp_target", n_t), rep("budget", length(b)), rep("pu_zone", n_pu))
  )
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + 1))
  expect_equal(o$ub(), rep(1, (n_pu * n_z) + 1))
  # test that problem matrix is correct
  m <- matrix(
    0, nrow = n_t + length(b) + n_pu,
    ncol = (n_pu * n_z) + 1
  )
  counter <- 0
  for (i in seq_len(n_t)) {
    zs <- match(targs$zone[[i]], zone_names(sim_zones_features))
    f <- match(targs$feature[i], feature_names(sim_zones_features))
    counter <- counter + 1
    for (z in zs)
      m[counter, ((z - 1) * n_pu) + seq_len(n_pu)] <-
        p$data$rij_matrix[[z]][f, ]
    m[counter, (n_z * n_pu) + 1] <- targs$target[i]
  }
  counter <- counter + 1
  m[counter, seq_len(n_pu * n_z)] <- c(p$planning_unit_costs())
  for (i in seq_len(n_pu)) {
    m[i + counter, i] <- 1
    m[i + counter, n_pu + i] <- 1
    m[i + counter, (2 * n_pu) + i] <- 1
  }
  m <- as(m, "Matrix")
  expect_true(all(o$A() == m))
})

test_that("solve (compressed formulation, multiple zones, scalar budget)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make and solve problem
  budget <- 7
  locked_out <- matrix(FALSE, ncol = 2, nrow = 5)
  locked_out[1, 1] <- TRUE
  targs <- tibble::tibble(
    feature = c("f1", "f2", "f2"),
    zone = list("zone_1", "zone_2", c("zone_1", "zone_2")),
    sense = c(">=", ">=", "<="),
    target = c(5, 1, 300),
    type = c("absolute", "absolute", "absolute"))
  cost <- c(
    terra::rast(matrix(c(1, 2, 4, NA, NA), nrow = 1)),
    terra::rast(matrix(c(0.5, 1, 1, 1, NA), nrow = 1))
  )
  features <- c(
    terra::rast(matrix(c(5,   2,  3,  0,  4), nrow = 1)),
    terra::rast(matrix(c(5,   5,  5,  5,  5), nrow = 1)),
    terra::rast(matrix(c(5,   0,  1,  10, 4), nrow = 1)),
    terra::rast(matrix(c(500, 5,  5,  5,  5), nrow = 1))
  )
  # create problem
  p <-
    problem(
      cost,
      zones(
        features[[1:2]], features[[3:4]],
        zone_names = c("zone_1", "zone_2"), feature_names = c("f1", "f2")
      )
    ) %>%
    add_min_largest_shortfall_objective(budget = budget) %>%
    add_manual_targets(targs) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_equal(c(terra::values(s[[1]])), c(0, 1, 1, NA,  NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 0, 0, 1, NA))
})

test_that("compile (compressed formulation, multiple zones, vector budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate budget
  b <- floor(
    terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.25
  )
  # calculate targets data
  targs <- tibble::tibble(
    feature = feature_names(sim_zones_features)[1:3],
    zone = list("zone_1", "zone_2", c("zone_1", "zone_3")),
    sense = c(">=", "<=", ">="),
    target = c(5, 300, 10),
    type = c("absolute", "absolute", "absolute")
  )
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_largest_shortfall_objective(budget = b) %>%
    add_manual_targets(targs) %>%
    add_binary_decisions()
  o <- compile(p)
  # calculations for tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  n_t <- nrow(targs)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(
    o$obj(),
    c(rep(0, length(p$planning_unit_costs())), 1)
  )
  expect_equal(
    o$sense(),
    c(targs$sense, rep("<=", length(b)), rep("<=", n_pu))
  )
  expect_equal(
    o$rhs(),
    c(targs$target, b, rep(1, n_pu))
  )
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu * n_z), "max_shortfall")
  )
  expect_equal(
    o$row_ids(),
    c(rep("spp_target", n_t), rep("budget", length(b)), rep("pu_zone", n_pu))
  )
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + 1))
  expect_equal(o$ub(), rep(1, (n_pu * n_z) + 1))
  # test that problem matrix is correct
  m <- matrix(
    0, nrow = n_t + length(b) + n_pu,
    ncol = (n_pu * n_z) + 1
  )
  counter <- 0
  for (i in seq_len(n_t)) {
    zs <- match(targs$zone[[i]], zone_names(sim_zones_features))
    f <- match(targs$feature[i], feature_names(sim_zones_features))
    counter <- counter + 1
    for (z in zs)
      m[counter, ((z - 1) * n_pu) + seq_len(n_pu)] <-
        p$data$rij_matrix[[z]][f, ]
    m[counter, (n_z * n_pu) + 1] <- targs$target[i]
  }
  counter <- counter + 1
  m[counter, seq_len(n_pu)] <- p$planning_unit_costs()[, 1]
  counter <- counter + 1
  m[counter, n_pu + seq_len(n_pu)] <- p$planning_unit_costs()[, 2]
  counter <- counter + 1
  m[counter, (2 * n_pu) + seq_len(n_pu)] <- p$planning_unit_costs()[, 3]
  for (i in seq_len(n_pu)) {
    m[i + counter, i] <- 1
    m[i + counter, n_pu + i] <- 1
    m[i + counter, (2 * n_pu) + i] <- 1
  }
  m <- as(m, "Matrix")
  expect_true(all(o$A() == m))
})

test_that("solve (compressed formulation, multiple zones, vector budget)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- c(6, 1)
  locked_out <- matrix(FALSE, ncol = 2, nrow = 5)
  locked_out[1, 1] <- TRUE
  targs <- tibble::tibble(
    feature = c("f1", "f2", "f2"),
    zone = list("zone_1", "zone_2", c("zone_1", "zone_2")),
    sense = c(">=", ">=", "<="),
    target = c(5, 1, 300),
    type = c("absolute", "absolute", "absolute")
  )
  cost <- c(
    terra::rast(matrix(c(1, 2, 4, NA, NA), nrow = 1)),
    terra::rast(matrix(c(0.5, 1, 1, 1, NA), nrow = 1))
  )
  features <- c(
    terra::rast(matrix(c(5,   2,  3,  0,  4), nrow = 1)),
    terra::rast(matrix(c(5,   5,  5,  5,  5), nrow = 1)),
    terra::rast(matrix(c(5,   0,  1,  10, 4), nrow = 1)),
    terra::rast(matrix(c(500, 5,  5,  5,  5), nrow = 1))
  )
  # create problem
  p <-
    problem(
      cost,
      zones(
        features[[1:2]], features[[3:4]],
        zone_names = c("zone_1", "zone_2"), feature_names = c("f1", "f2")
      )
    ) %>%
    add_min_largest_shortfall_objective(budget = budget) %>%
    add_manual_targets(targs) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_equal(c(terra::values(s[[1]])), c(0, 1, 1, NA,  NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 0, 0, 1, NA))
})

test_that("compile (expanded formulation, multiple zones, vector budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate budget
  b <- floor(
    terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.25
  )
  # calculate targets data
  targs <- tibble::tibble(
    feature = feature_names(sim_zones_features)[1:3],
    zone = list("zone_1", "zone_2", c("zone_1", "zone_3")),
    sense = c(">=", "<=", ">="),
    target = c(5, 300, 10),
    type = c("absolute", "absolute", "absolute")
  )
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_largest_shortfall_objective(budget = b) %>%
    add_manual_targets(targs) %>%
    add_binary_decisions()
  o <- compile(p, compressed_formulation = FALSE)
  # calculations for tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  n_t <- nrow(targs)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(
    o$obj(),
    c(rep(0, n_pu * n_z), rep(0, n_pu * n_f * n_z), 1)
  )
  expect_equal(
    o$sense(),
    c(
      rep("<=", n_f * n_z * n_pu), targs$sense,
      rep("<=", length(b)), rep("<=", n_pu)
    )
  )
  expect_equal(
    o$rhs(),
    c(rep(0, n_pu * n_z * n_f), targs$target, b, rep(1, n_pu))
  )
  expect_equal(
    o$col_ids(),
    c(
      rep("pu", n_pu * n_z), rep("pu_ijz", n_pu * n_z * n_f),
      "max_shortfall"
    )
  )
  expect_equal(
    o$row_ids(),
    c(rep("pu_ijz", n_pu * n_z * n_f), rep("spp_target", n_t),
      rep("budget", length(b)), rep("pu_zone", n_pu)
    )
  )
  expect_equal(
    o$lb(),
    rep(0, (n_pu * n_z) + (n_pu * n_z * n_f) + 1)
  )
  expect_equal(
    o$ub(),
    rep(1, (n_pu * n_z) + (n_pu * n_z * n_f) + 1)
  )
  # test that problem matrix is correct
  m <- matrix(
    0, nrow = (n_pu * n_z * n_f) + n_t + length(b) + n_pu,
    ncol = (n_pu * n_z) + (n_pu * n_z * n_f) + 1
  )
  counter <- 0
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_f)) {
      for (j in seq_len(n_pu)) {
        counter <- counter + 1
        m[counter, ((z - 1) * n_pu) + j] <- -1
        idx <- (n_pu * n_z) + ((z - 1) * n_pu * n_f) + ((i - 1) * n_pu) + j
        m[counter, idx] <- 1
      }
    }
  }
  for (i in seq_len(nrow(targs))) {
    zs <- match(targs$zone[[i]], zone_names(sim_zones_features))
    f <- match(targs$feature[i], feature_names(sim_zones_features))
    counter <- counter + 1
    for (z in zs) {
      for (pu in seq_len(n_pu)) {
        col <- (n_pu * n_z) + ((z - 1) * n_f * n_pu) + ((f - 1) * n_pu) + pu
        m[counter, col] <- p$data$rij_matrix[[z]][f, pu]
      }
      col <- (n_pu * n_z) + (n_pu * n_f * n_z) + 1
      m[counter, col] <- targs$target[i]
    }
  }
  counter <- counter + 1
  m[counter, seq_len(n_pu)] <- p$planning_unit_costs()[, 1]
  counter <- counter + 1
  m[counter, n_pu + seq_len(n_pu)] <- p$planning_unit_costs()[, 2]
  counter <- counter + 1
  m[counter, (2 *n_pu) + seq_len(n_pu)] <- p$planning_unit_costs()[, 3]
  for (i in seq_len(n_pu)) {
    m[i + counter, i] <- 1
    m[i + counter, n_pu + i] <- 1
    m[i + counter, (2 * n_pu) + i] <- 1
  }
  m <- as(m, "Matrix")
  expect_true(all(o$A() == m))
})

test_that("solve (expanded formulation, multiple zones, vector budget)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- c(6, 1)
  locked_out <- matrix(FALSE, ncol = 2, nrow = 5)
  locked_out[1, 1] <- TRUE
  targs <- tibble::tibble(
    feature = c("f1", "f2", "f2"),
    zone = list("zone_1", "zone_2", c("zone_1", "zone_2")),
    sense = c(">=", ">=", "<="),
    target = c(5, 1, 300),
    type = c("absolute", "absolute", "absolute")
  )
  cost <- c(
    terra::rast(matrix(c(1, 2, 4, NA, NA), nrow = 1)),
    terra::rast(matrix(c(0.5, 1, 1, 1, NA), nrow = 1))
  )
  features <- c(
    terra::rast(matrix(c(5,   2,  3,  0,  4), nrow = 1)),
    terra::rast(matrix(c(5,   5,  5,  5,  5), nrow = 1)),
    terra::rast(matrix(c(5,   0,  1,  10, 4), nrow = 1)),
    terra::rast(matrix(c(500, 5,  5,  5,  5), nrow = 1))
  )
  # create problem
  p <-
    problem(
      cost,
      zones(
        features[[1:2]], features[[3:4]],
        zone_names = c("zone_1", "zone_2"), feature_names = c("f1", "f2")
      )
    ) %>%
    add_min_largest_shortfall_objective(budget = budget) %>%
    add_manual_targets(targs) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # tests
  expect_equal(c(terra::values(s[[1]])), c(0, 1, 1, NA,  NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 0, 0, 1, NA))
})

test_that("invalid inputs (multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # tests
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_largest_shortfall_objective(budget = c(1, -5, 1))
  )
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_largest_shortfall_objective(budget = c(1, NA, 1))
  )
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_largest_shortfall_objective(budget = c(NA, NA, NA))
  )
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_largest_shortfall_objective(budget = c(1, Inf, 9))
  )
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_largest_shortfall_objective(budget = c(1, Inf, 9))
  )
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_largest_shortfall_objective(budget = c(5, 5, 5)) %>%
    compile()
  )
})
