context("add_max_cover_objective")

test_that("compile (compressed formulation, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate budget
  b <- floor(terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]]) * 0.25
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = b)
  o <- compile(p)
  # calculations for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- terra::nlyr(sim_features)
  scaled_costs <- c(p$planning_unit_costs())
  scaled_costs <- scaled_costs * (-0.01 / sum(scaled_costs, na.rm = TRUE))
  # tests
  expect_equal(o$modelsense(), "max")
  expect_equal(o$obj(), c(scaled_costs, rep(1, n_f)))
  expect_equal(o$sense(), c(rep(">=", n_f), "<="))
  expect_equal(o$rhs(), c(rep(0, n_f), b))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("present", n_f)))
  expect_equal(o$row_ids(), c(rep("spp_present", n_f), "budget"))
  expect_equal(o$lb(), rep(0, n_f + n_pu))
  expect_equal(o$ub(), rep(1, n_f + n_pu))
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
})

test_that("solve (compressed formulation, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- 3
  cost <- terra::rast(matrix(c(1, 2, 4, NA), nrow = 1))
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(1, 1, 1, 0), nrow = 1)),
    terra::rast(matrix(c(1, 0, 1, 10), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_max_cover_objective(budget = budget) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # tests
  expect_equal(c(terra::values(s1)), c(0, 1, 0, NA))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("compile (expanded formulation, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate budget
  b <- floor(terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]]) * 0.25
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = b)
  o <- compile(p, compressed_formulation = FALSE)
  # calculations for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- terra::nlyr(sim_features)
  rij <- rij_matrix(sim_pu_raster, sim_features)
  scaled_costs <- c(p$planning_unit_costs())
  scaled_costs <- scaled_costs * (-0.01 / sum(scaled_costs, na.rm = TRUE))
  # tests
  expect_equal(o$modelsense(), "max")
  expect_equal(o$obj(), c(scaled_costs, rep(0, n_pu * n_f), rep(1, n_f)))
  expect_equal(o$sense(), c(rep("<=", n_pu * n_f), rep( ">=", n_f), "<="))
  expect_equal(o$rhs(), c(rep(0, n_f * n_pu), rep(0, n_f), b))
  expect_equal(o$lb(), rep(0, n_pu + (n_f * n_pu) + n_f))
  expect_equal(o$ub(), rep(1, n_pu + (n_f * n_pu) + n_f))
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu), rep("pu_ijz", n_pu * n_f), rep("present", n_f))
  )
  expect_equal(
    o$row_ids(),
    c(rep("pu_ijz", n_f * n_pu), rep("spp_present", n_f), "budget")
  )
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
  # create  data
  budget <- 3
  cost <- terra::rast(matrix(c(1, 2, 4, NA), nrow = 1))
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(1, 1, 1, 0), nrow = 1)),
    terra::rast(matrix(c(1, 0, 1, 10), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_max_cover_objective(budget = budget) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # tests
  expect_equal(c(terra::values(s)), c(0, 1, 0, NA))
})

test_that("invalid inputs (single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # tests
  expect_warning(
    problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = 5000) %>%
    add_relative_targets(0.1) %>%
    compile(),
    "ignored"
  )
  expect_tidy_error(
    problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = -5)
  )
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = NA_real_)
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = Inf)
  })
})

test_that("compile (compressed formulation, multiple zones, scalar budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate budget
  b <- min(floor(
    terra::global(sim_zones_pu_raster[[1]], "sum", na.rm = TRUE)[[1]] * 0.25
  ))
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = b)
  o <- compile(p)
  # calculations for tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  scaled_costs <- c(p$planning_unit_costs())
  scaled_costs <- scaled_costs * (-0.01 / sum(scaled_costs, na.rm = TRUE))
  # tests
  expect_equal(o$modelsense(), "max")
  expect_equal(o$obj(), c(scaled_costs, rep(1, n_f * n_z)))
  expect_equal(
    o$sense(),
    c(rep(">=", n_f * n_z), "<=", rep("<=", n_pu))
  )
  expect_equal(
    o$rhs(),
    c(rep(0, n_f * n_z), b, rep(1, n_pu))
  )
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
    0, nrow = (n_f * n_z) + 1 + n_pu, ncol = (n_pu * n_z) + (n_z * n_f)
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
  m <- as(m, "Matrix")
  expect_true(all(o$A() == m))
})

test_that("solve (compressed formulation, multiple zones, scalar budget)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- 3
  locked_out <- matrix(FALSE, ncol = 2, nrow = 5)
  locked_out[1, 2] <- TRUE
  cost <- c(
    terra::rast(matrix(c(10, 10, 10, 10, NA), nrow = 1)),
    terra::rast(matrix(c(1,  2,  4,  NA, NA), nrow = 1))
  )
  features <- c(
    terra::rast(matrix(c(1,  1,  1,  1,   1), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1,  1,   1), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1,  0,   1), nrow = 1)),
    terra::rast(matrix(c(1,  0,  1,  10,  1), nrow = 1))
  )
 # create problem
  p <-
    problem(cost, zones(features[[1:2]], features[[3:4]])) %>%
    add_max_cover_objective(budget = budget) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_equal(c(terra::values(s[[1]])), c(0, 0, 0, 0,  NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 1, 0, NA, NA))
})

test_that("compile (expanded formulation, multiple zones, scalar budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate budget
  b <- min(floor(
    terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.25
  ))
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = b)
  o <- compile(p, FALSE)
  # calculations for tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  scaled_costs <- c(p$planning_unit_costs())
  scaled_costs <- scaled_costs * (-0.01 / sum(scaled_costs, na.rm = TRUE))
  # tests
  expect_equal(o$modelsense(), "max")
  expect_equal(
    o$obj(),
    c(scaled_costs, rep(0, n_pu * n_f * n_z), rep(1, n_f * n_z))
  )
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
      rep("pu", n_pu * n_z),
      rep("pu_ijz", n_pu * n_z * n_f),
      rep("present", n_f * n_z)
    )
  )
  expect_equal(
    o$row_ids(),
    c(
      rep("pu_ijz", n_pu * n_z * n_f),
      rep("spp_present", n_f * n_z),
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
        m[counter, ((z - 1) * n_pu) + j] <- -1
        idx <- (n_pu * n_z) + ((z - 1) * n_pu * n_f) + ((i - 1) * n_pu) + j
        m[counter, idx] <- 1
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
  m <- as(m, "Matrix")
  expect_true(all(o$A() == m))
})

test_that("solve (expanded formulation, multiple zones, scalar budget)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- 3
  locked_out <- matrix(FALSE, ncol = 2, nrow = 5)
  locked_out[1, 2] <- TRUE
  cost <- c(
    terra::rast(matrix(c(10, 10, 10, 10, NA), nrow = 1)),
    terra::rast(matrix(c(1,  2,  4,  NA, NA), nrow = 1))
  )
  features <- c(
    terra::rast(matrix(c(1,  1,  1,  1,   1), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1,  1,   1), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1,  0,   1), nrow = 1)),
    terra::rast(matrix(c(1,  0,  1,  10,  1), nrow = 1))
  )
  # create problem
  p <-
    problem(cost, zones(features[[1:2]], features[[3:4]])) %>%
    add_max_cover_objective(budget = budget) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # tests
  expect_equal(c(terra::values(s[[1]])), c(0, 0, 0, 0,  NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 1, 0, NA, NA))
})

test_that("compile (compressed formulation, multiple zones, vector budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate budget
  b <- unname(floor(
    terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]]) * 0.25
  )
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = b)
  o <- compile(p)
  # calculations for tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  scaled_costs <- c(p$planning_unit_costs())
  scaled_costs <- scaled_costs * (-0.01 / sum(scaled_costs, na.rm = TRUE))
  # tests
  expect_equal(o$modelsense(), "max")
  expect_equal(o$obj(), c(scaled_costs, rep(1, n_f * n_z)))
  expect_equal(
    o$sense(),
    c(rep(">=", n_f * n_z), rep("<=", 3), rep("<=", n_pu))
  )
  expect_equal(o$rhs(), c(rep(0, n_f * n_z), b, rep(1, n_pu)))
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu * n_z), rep("present", n_f * n_z))
  )
  expect_equal(
    o$row_ids(),
    c(rep("spp_present", n_f * n_z), rep("budget", 3), rep("pu_zone", n_pu))
  )
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + (n_f * n_z)))
  expect_equal(o$ub(), rep(1, (n_pu * n_z) + (n_f * n_z)))
  # check that problem matrix is correct
  m <- matrix(
    0, nrow = (n_f * n_z) + n_z + n_pu, ncol = (n_pu * n_z) + (n_z * n_f)
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
  budget <- c(10, 3)
  locked_out <- matrix(FALSE, ncol = 2, nrow = 5)
  locked_out[1, 2] <- TRUE
  cost <- c(
    terra::rast(matrix(c(10, 10, 10, 10, NA), nrow = 1)),
    terra::rast(matrix(c(1,  2,  4,  NA, NA), nrow = 1))
  )
  features <- c(
    terra::rast(matrix(c(1,  1,  0,  0,   0), nrow = 1)),
    terra::rast(matrix(c(1,  0,  0,  0,   0), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1,  0,   1), nrow = 1)),
    terra::rast(matrix(c(1,  0,  1,  10,  1), nrow = 1))
  )
 # create problem
  p <-
    problem(cost, zones(features[[1:2]], features[[3:4]])) %>%
    add_max_cover_objective(budget = budget) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_equal(c(terra::values(s[[1]])), c(1, 0, 0, 0,  NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 1, 0, NA, NA))
})

test_that("compile (expanded formulation, multiple zones, vector budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate budget
  b <- unname(
    floor(terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]]) * 0.25
  )
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = b)
  o <- compile(p, FALSE)
  # calculations for tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  scaled_costs <- c(p$planning_unit_costs())
  scaled_costs <- scaled_costs * (-0.01 / sum(scaled_costs, na.rm = TRUE))
  # tests
  expect_equal(o$modelsense(), "max")
  expect_equal(
    o$obj(),
    c(scaled_costs, rep(0, n_pu * n_f * n_z), rep(1, n_f * n_z))
  )
  expect_equal(
    o$sense(),
    c(
      rep("<=", n_f * n_z * n_pu),
      rep(">=", n_f * n_z), rep("<=", n_z),
      rep("<=", n_pu)
    )
  )
  expect_equal(
    o$rhs(),
    c(rep(0, n_pu * n_z * n_f), rep(0, n_f * n_z), b, rep(1, n_pu))
  )
  expect_equal(
    o$col_ids(),
    c(
      rep("pu", n_pu * n_z),
      rep("pu_ijz", n_pu * n_z * n_f),
      rep("present", n_f * n_z)
    )
  )
  expect_equal(
    o$row_ids(),
    c(
      rep("pu_ijz", n_pu * n_z * n_f),
      rep("spp_present", n_f * n_z),
      rep("budget", n_z), rep("pu_zone", n_pu)
    )
  )
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + (n_pu * n_z * n_f) + (n_f * n_z)))
  expect_equal(o$ub(), rep(1, (n_pu * n_z) + (n_pu * n_z * n_f) + (n_f * n_z)))
  # check that problem matrix is correct
  m <- matrix(
    0, nrow = (n_pu * n_z * n_f) + (n_f * n_z) + n_z + n_pu,
    ncol = (n_pu * n_z) + (n_pu * n_z * n_f) + (n_z * n_f)
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

test_that("solve (expanded formulation, multiple zones, vector budget)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  budget <- c(10, 3)
  locked_out <- matrix(FALSE, ncol = 2, nrow = 5)
  locked_out[1, 2] <- TRUE
  cost <- c(
    terra::rast(matrix(c(10, 10, 10, 10, NA), nrow = 1)),
    terra::rast(matrix(c(1,  2,  4,  NA, NA), nrow = 1))
  )
  features <- c(
    terra::rast(matrix(c(1,  1,  0,  0,   0), nrow = 1)),
    terra::rast(matrix(c(1,  0,  0,  0,   0), nrow = 1)),
    terra::rast(matrix(c(1,  1,  1,  0,   1), nrow = 1)),
    terra::rast(matrix(c(1,  0,  1,  10,  1), nrow = 1))
  )
 # create problem
  p <-
    problem(cost, zones(features[[1:2]], features[[3:4]])) %>%
    add_max_cover_objective(budget = budget) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # tests
  expect_equal(c(terra::values(s[[1]])), c(1, 0, 0, 0,  NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 1, 0, NA, NA))
})

test_that("invalid inputs (multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # tests
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = c(1, -5, 1))
  )
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = c(1, NA, 1))
  )
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = c(NA, NA, NA))
  )
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = c(1, Inf, 9))
  )
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_max_cover_objective(budget = c(1, Inf, 9))
  )
})
