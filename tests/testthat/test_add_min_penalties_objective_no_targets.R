test_that("compile (compressed formulation, single zone, scalar budget)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate data
  budget <- terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.8
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_penalties_objective(budget) %>%
    add_binary_decisions()
  expect_warning(
    {o <- compile(p)},
    "any penalties"
  )
  # calculations for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  # run tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), rep(0, n_pu))
  expect_equal(o$vtype(), rep("B", n_pu))
  expect_equal(o$lb(), rep(0, n_pu))
  expect_equal(o$ub(), rep(1, n_pu))
  expect_equal(o$rhs(), budget)
  expect_equal(o$sense(), "<=")
  expect_equal(o$col_ids(), rep("pu", n_pu))
  expect_equal(o$row_ids(), "budget_mp")
  expect_true(
    all(o$A() == c(sim_pu_raster[[1]][!is.na(sim_pu_raster)]))
  )
})

test_that("compile (compressed formulation, single zone, no budget)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_penalties_objective() %>%
    add_binary_decisions()
  expect_warning(
    {o <- compile(p)},
    "any penalties"
  )
  # calculations for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  # run tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), rep(0, n_pu))
  expect_equal(o$vtype(), rep("B", n_pu))
  expect_equal(o$lb(), rep(0, n_pu))
  expect_equal(o$ub(), rep(1, n_pu))
  expect_equal(o$rhs(), 1)
  expect_equal(o$sense(), "<=")
  expect_equal(o$col_ids(), rep("pu", n_pu))
  expect_equal(o$row_ids(), "dum_mp")
  expect_true(
    all(o$A() == c(0.1, rep(0, n_pu - 1)))
  )
})

test_that("solve (compressed formulation, single zone)", {
  skip_on_cran()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate data
  budget <- terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.8
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_linear_penalties(1, sim_pu_raster) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  p <- lapply(
    list(NULL, budget),
    add_min_penalties_objective, x = p
  )
  # solve problem
  s <- terra::rast(lapply(p, solve))
  # run tests
  expect_inherits(s, "SpatRaster")
  expect_equal(max(terra::global(s, "sum", na.rm = TRUE)[[1]]), 0)
})

test_that("compile (compressed formulation, multiple zones, scalar budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # specify budgets
  budget <- sum(
    terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.8
  )
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_penalties_objective(budget = budget) %>%
    add_binary_decisions()
  expect_warning(
    {o <- compile(p)},
    "any penalties"
  )
  # calculations for tests
  n_pu <- length(sim_zones_pu_raster[[1]][!is.na(sim_zones_pu_raster[[1]])])
  n_f <- number_of_features(sim_zones_features)
  n_z <- number_of_zones(sim_zones_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), rep(0, n_pu * n_z))
  expect_equal(
    o$sense(),
    c(rep("<=", n_pu), "<=")
  )
  expect_equal(o$rhs(), c(budget, rep(1, n_pu)))
  expect_equal(
    o$row_ids(),
    c("budget_mp", rep("pu_zone", n_pu))
  )
  expect_equal(o$col_ids(), rep("pu", n_pu * n_z))
  expect_equal(o$lb(), rep(0, n_pu * n_z))
  expect_equal(o$ub(), rep(1, n_pu * n_z))
  # test model matrix
  m <- matrix(0, nrow = n_pu + 1, ncol = n_pu * n_z)
  ## budget constraint
  m[1, ] <- c(p$planning_unit_costs())
  ## zone constraints
  for (i in seq_len(n_pu)) {
    m[1 + i, c(i, n_pu + i, n_pu + n_pu + i) ] <- 1
  }
  ## convert to sparseMatrix
  m <- as_Matrix(m, "dgCMatrix")
  ## tests
  expect_true(all(m == o$A()))
})

test_that("compile (compressed formulation, multiple zones, vector budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # specify budgets
  budget <- terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.8
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_penalties_objective(budget = budget) %>%
    add_binary_decisions()
  expect_warning(
    {o <- compile(p)},
    "any penalties"
  )
  # calculations for tests
  n_pu <- length(sim_zones_pu_raster[[1]][!is.na(sim_zones_pu_raster[[1]])])
  n_z <- number_of_zones(sim_zones_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), rep(0, n_pu * n_z))
  expect_equal(
    o$sense(),
    c(rep("<=", n_pu), rep("<=", n_z))
  )
  expect_equal(o$rhs(), c(budget, rep(1, n_pu)))
  expect_equal(
    o$row_ids(),
    c(rep("budget_mp", n_z), rep("pu_zone", n_pu))
  )
  expect_equal(o$col_ids(), rep("pu", n_pu * n_z))
  expect_equal(o$lb(), rep(0, n_pu * n_z))
  expect_equal(o$ub(), rep(1, n_pu * n_z))
  # test model matrix
  m <- matrix(0, nrow = n_pu + n_z, ncol = n_pu * n_z)
  ## budget constraint
  for (i in seq_len(n_z)) {
    m[i, ((i - 1) * n_pu) + seq_len(n_pu)] <- p$planning_unit_costs()[, i]
  }
  ## zone constraints
  for (i in seq_len(n_pu)) {
    m[n_z + i, c(i, n_pu + i, n_pu + n_pu + i) ] <- 1
  }
  ## convert to sparseMatrix
  m <- as_Matrix(m, "dgCMatrix")
  ## tests
  expect_true(all(m == o$A()))
})

test_that("compile (compressed formulation, multiple zones, no budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_penalties_objective() %>%
    add_binary_decisions()
  expect_warning(
    {o <- compile(p)},
    "any penalties"
  )
  # calculations for tests
  n_pu <- length(sim_zones_pu_raster[[1]][!is.na(sim_zones_pu_raster[[1]])])
  n_z <- number_of_zones(sim_zones_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), rep(0, n_pu * n_z))
  expect_equal(o$sense(), rep("<=", n_pu))
  expect_equal(o$rhs(), rep(1, n_pu))
  expect_equal(o$row_ids(), rep("pu_zone", n_pu))
  expect_equal(o$col_ids(), rep("pu", n_pu * n_z))
  expect_equal(o$lb(), rep(0, n_pu * n_z))
  expect_equal(o$ub(), rep(1, n_pu * n_z))
  # test model matrix
  m <- matrix(0, nrow = n_pu, ncol = n_pu * n_z)
  ## zone constraints
  for (i in seq_len(n_pu)) {
    m[i, c(i, n_pu + i, n_pu + n_pu + i) ] <- 1
  }
  ## convert to sparseMatrix
  m <- as_Matrix(m, "dgCMatrix")
  ## tests
  expect_true(all(m == o$A()))
})

test_that("solve (compressed formulation, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # specify budgets
  b1 <- terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.8
  b2 <- sum(b1)
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_linear_penalties(
      rep(1, terra::nlyr(sim_zones_pu_raster)),
      sim_zones_pu_raster
    ) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  p <- lapply(
    list(NULL, b1, b2),
    add_min_penalties_objective, x = p
  )
  # solve problem
  s <- terra::rast(lapply(p, solve))
  # run tests
  expect_inherits(s, "SpatRaster")
  expect_equal(max(terra::global(s, "sum", na.rm = TRUE)[[1]]), 0)
})

test_that("compile (expanded formulation, single zone, scalar budget)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate targets data
  budget <- terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.8
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_penalties_objective(budget) %>%
    add_binary_decisions()
  expect_warning(
    {o <- compile(p, FALSE)},
    "any penalties"
  )
  # calculations for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- terra::nlyr(sim_features)
  n_dv <- n_pu + (n_pu * n_f)
  # run tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), rep(0, n_dv))
  expect_equal(o$vtype(), rep("B", n_dv))
  expect_equal(o$lb(), rep(0, n_dv))
  expect_equal(o$ub(), rep(1, n_dv))
  expect_equal(o$rhs(), c(rep(0, n_pu * n_f), budget))
  expect_equal(o$sense(), rep("<=", (n_pu * n_f) + 1))
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu), rep("pu_ijz", n_pu * n_f))
  )
  expect_equal(
    o$row_ids(),
    c(rep("pu_ijz", n_pu * n_f), "budget_mp")
  )
  # test model matrix
  m <- matrix(
    0, nrow = n_pu * n_f + 1, ncol = n_pu + (n_pu * n_f)
  )
  ## allocation constraints
  r <- 0
  for (z in seq_len(1)) {
    for (i in seq_len(n_f)) {
      for (j in seq_len(n_pu)) {
        r <- r + 1
        m[r, ((z - 1) * n_pu) + j] <- -1
        idx <- (n_pu * 1) + ((z - 1) * n_pu * n_f) +
               ((i - 1) * n_pu) + j
        m[r, idx] <- 1
      }
    }
  }
  ## budget constraints
  r <- r + 1
  col <- seq_len(n_pu)
  m[r, col] <- c(p$planning_unit_costs())
  ## convert to sparseMatrix
  m <- as_Matrix(m, "dgCMatrix")
  ## tests
  expect_true(all(m == o$A()))
})

test_that("compile (expanded formulation, single zone, no budget)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_penalties_objective() %>%
    add_binary_decisions()
  expect_warning(
    {o <- compile(p, FALSE)},
    "any penalties"
  )
  # calculations for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- terra::nlyr(sim_features)
  n_dv <- n_pu + (n_pu * n_f)
  # run tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), rep(0, n_dv))
  expect_equal(o$vtype(), rep("B", n_dv))
  expect_equal(o$lb(), rep(0, n_dv))
  expect_equal(o$ub(), rep(1, n_dv))
  expect_equal(o$rhs(), c(rep(0, n_pu * n_f)))
  expect_equal(o$sense(), rep("<=", (n_pu * n_f)))
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu), rep("pu_ijz", n_pu * n_f))
  )
  expect_equal(
    o$row_ids(),
    c(rep("pu_ijz", n_pu * n_f))
  )
  # test model matrix
  m <- matrix(
    0, nrow = n_pu * n_f, ncol = n_pu + (n_pu * n_f)
  )
  ## allocation constraints
  r <- 0
  for (z in seq_len(1)) {
    for (i in seq_len(n_f)) {
      for (j in seq_len(n_pu)) {
        r <- r + 1
        m[r, ((z - 1) * n_pu) + j] <- -1
        idx <- (n_pu * 1) + ((z - 1) * n_pu * n_f) +
               ((i - 1) * n_pu) + j
        m[r, idx] <- 1
      }
    }
  }
  ## convert to sparseMatrix
  m <- as_Matrix(m, "dgCMatrix")
  ## tests
  expect_true(all(m == o$A()))
})

test_that("compile (expanded formulation, multiple zones, scalar budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # specify budgets
  budget <- sum(
    terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.8
  )
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_penalties_objective(budget = budget) %>%
    add_binary_decisions()
  expect_warning(
    {o <- compile(p, FALSE)},
    "any penalties"
  )
  # calculations for tests
  n_pu <- length(sim_zones_pu_raster[[1]][!is.na(sim_zones_pu_raster[[1]])])
  n_z <- number_of_zones(sim_zones_features)
  n_f <- number_of_features(sim_zones_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(
    o$obj(),
    c(rep(0, n_pu * n_z), rep(0, n_pu * n_z * n_f))
  )
  expect_equal(
    o$sense(),
    c(
      rep("<=", n_pu * n_z * n_f),
      "<=",
      rep("<=", n_pu)
    )
  )
  expect_equal(
    o$rhs(),
    c(
      rep(0, n_pu * n_z * n_f),
      budget,
      rep(1, n_pu)
    )
  )
  expect_equal(
    o$row_ids(),
    c(
      rep("pu_ijz", n_pu * n_z * n_f),
      "budget_mp",
      rep("pu_zone", n_pu)
    )
  )
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu * n_z), rep("pu_ijz", n_pu * n_z * n_f))
  )
  expect_equal(o$lb(), rep(0, n_pu * n_z + n_pu * n_z * n_f))
  expect_equal(o$ub(), rep(1, n_pu * n_z + n_pu * n_z * n_f))
  # test model matrix
  m <- matrix(
    0, nrow = n_pu * n_f * n_z + 1 + n_pu,
    ncol = (n_pu * n_z) + (n_pu * n_z * n_f)
  )
  ## allocation constraints
  r <- 0
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_f)) {
      for (j in seq_len(n_pu)) {
        r <- r + 1
        m[r, ((z - 1) * n_pu) + j] <- -1
        idx <- (n_pu * n_z) + ((z - 1) * n_pu * n_f) +
               ((i - 1) * n_pu) + j
        m[r, idx] <- 1
      }
    }
  }
  ## budget constraints
  r <- r + 1
  col <- seq_len(n_pu * n_z)
  m[r, col] <- c(p$planning_unit_costs())
  ## zone constraints
  for (i in seq_len(n_pu))
    m[r + i, c(i, n_pu + i, n_pu + n_pu + i)] <- 1
  ## convert to sparseMatrix
  m <- as_Matrix(m, "dgCMatrix")
  ## tests
  expect_true(all(m == o$A()))
})

test_that("compile (expanded formulation, multiple zones, vector budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # specify budgets
  budget <- terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.8
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_penalties_objective(budget = budget) %>%
    add_binary_decisions()
  expect_warning(
    {o <- compile(p, FALSE)},
    "any penalties"
  )
  # calculations for tests
  n_pu <- length(sim_zones_pu_raster[[1]][!is.na(sim_zones_pu_raster[[1]])])
  n_z <- number_of_zones(sim_zones_features)
  n_f <- number_of_features(sim_zones_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(
    o$obj(),
    c(rep(0, n_pu * n_z), rep(0, n_pu * n_z * n_f))
  )
  expect_equal(
    o$sense(),
    c(
      rep("<=", n_pu * n_z * n_f),
      rep("<=", n_z),
      rep("<=", n_pu)
    )
  )
  expect_equal(
    o$rhs(),
    c(
      rep(0, n_pu * n_z * n_f),
      budget,
      rep(1, n_pu)
    )
  )
  expect_equal(
    o$row_ids(),
    c(
      rep("pu_ijz", n_pu * n_z * n_f),
      rep("budget_mp", n_z),
      rep("pu_zone", n_pu)
    )
  )
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu * n_z), rep("pu_ijz", n_pu * n_z * n_f))
  )
  expect_equal(o$lb(), rep(0, n_pu * n_z + n_pu * n_z * n_f))
  expect_equal(o$ub(), rep(1, n_pu * n_z + n_pu * n_z * n_f))
  # test model matrix
  m <- matrix(
    0, nrow = n_pu * n_f * n_z + n_z + n_pu,
    ncol = (n_pu * n_z) + (n_pu * n_z * n_f)
  )
  ## allocation constraints
  r <- 0
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_f)) {
      for (j in seq_len(n_pu)) {
        r <- r + 1
        m[r, ((z - 1) * n_pu) + j] <- -1
        idx <- (n_pu * n_z) + ((z - 1) * n_pu * n_f) +
               ((i - 1) * n_pu) + j
        m[r, idx] <- 1
      }
    }
  }
  ## budget constraints
  for (i in seq_len(n_z)) {
    r <- r + 1
    col <- ((i - 1) * n_pu) + seq_len(n_pu)
    m[r, col] <- p$planning_unit_costs()[, i]
  }
  ## zone constraints
  for (i in seq_len(n_pu))
    m[r + i, c(i, n_pu + i, n_pu + n_pu + i)] <- 1
  ## convert to sparseMatrix
  m <- as_Matrix(m, "dgCMatrix")
  ## tests
  expect_true(all(m == o$A()))
})

test_that("compile (expanded formulation, multiple zones, no budget)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_penalties_objective() %>%
    add_binary_decisions()
  expect_warning(
    {o <- compile(p, FALSE)},
    "any penalties"
  )
  # calculations for tests
  n_pu <- length(sim_zones_pu_raster[[1]][!is.na(sim_zones_pu_raster[[1]])])
  n_z <- number_of_zones(sim_zones_features)
  n_f <- number_of_features(sim_zones_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(
    o$obj(),
    c(rep(0, n_pu * n_z), rep(0, n_pu * n_z * n_f))
  )
  expect_equal(
    o$sense(),
    c(
      rep("<=", n_pu * n_z * n_f),
      rep("<=", n_pu)
    )
  )
  expect_equal(
    o$rhs(),
    c(
      rep(0, n_pu * n_z * n_f),
      rep(1, n_pu)
    )
  )
  expect_equal(
    o$row_ids(),
    c(
      rep("pu_ijz", n_pu * n_z * n_f),
      rep("pu_zone", n_pu)
    )
  )
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu * n_z), rep("pu_ijz", n_pu * n_z * n_f))
  )
  expect_equal(o$lb(), rep(0, n_pu * n_z + n_pu * n_z * n_f))
  expect_equal(o$ub(), rep(1, n_pu * n_z + n_pu * n_z * n_f))
  # test model matrix
  m <- matrix(
    0, nrow = n_pu * n_f * n_z + n_pu,
    ncol = (n_pu * n_z) + (n_pu * n_z * n_f)
  )
  ## allocation constraints
  r <- 0
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_f)) {
      for (j in seq_len(n_pu)) {
        r <- r + 1
        m[r, ((z - 1) * n_pu) + j] <- -1
        idx <- (n_pu * n_z) + ((z - 1) * n_pu * n_f) +
               ((i - 1) * n_pu) + j
        m[r, idx] <- 1
      }
    }
  }
  ## zone constraints
  for (i in seq_len(n_pu))
    m[r + i, c(i, n_pu + i, n_pu + n_pu + i)] <- 1
  ## convert to sparseMatrix
  m <- as_Matrix(m, "dgCMatrix")
  ## tests
  expect_true(all(m == o$A()))
})

test_that("solve (expanded formulation, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # specify budgets
  b1 <- terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.8
  b2 <- sum(b1)
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_linear_penalties(
      rep(1, terra::nlyr(sim_zones_pu_raster)),
      sim_zones_pu_raster
    ) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  p <- lapply(
    list(NULL, b1, b2),
    add_min_penalties_objective, x = p
  )
  # solve problem
  s <- terra::rast(lapply(p, solve, compressed_formulation = FALSE))
  # run tests
  expect_inherits(s, "SpatRaster")
  expect_equal(max(terra::global(s, "sum", na.rm = TRUE)[[1]]), 0)
})


test_that("invalid inputs (single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # run tests
  expect_tidy_error(
    problem(sim_pu_raster, sim_features) %>%
      add_min_penalties_objective(budget = "a") %>%
      add_absolute_targets(1) %>%
      compile(),
    "budget"
  )
  expect_tidy_error(
    problem(sim_pu_raster, sim_features) %>%
      add_min_penalties_objective(budget = c(1, 2)) %>%
      add_absolute_targets(1) %>%
      compile(),
    "budget"
  )
  expect_warning(
    problem(sim_pu_raster, sim_features) %>%
      add_min_penalties_objective() %>%
      compile(),
    "not have any penalties"
  )
})

test_that("invalid inputs (multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # run tests
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_penalties_objective(budget = c(1, 2, 3, 4)) %>%
    add_binary_decisions() %>%
    compile(),
    "budget"
  )
})
