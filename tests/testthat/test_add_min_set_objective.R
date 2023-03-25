test_that("compile (compressed formulation, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate targets data
  targ <- floor(terra::global(sim_features, "sum", na.rm = TRUE)[[1]] * 0.25)
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  o <- compile(p)
  # calculations for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(sim_pu_raster[[1]][!is.na(sim_pu_raster)]))
  expect_equal(o$sense(), rep(">=", terra::nlyr(sim_features)))
  expect_equal(o$rhs(), targ)
  expect_equal(o$row_ids(), rep("spp_target", terra::nlyr(sim_features)))
  expect_equal(o$col_ids(), rep("pu", n_pu))
  expect_true(all(o$A() == p$data$rij_matrix[[1]]))
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))
})

test_that("solve (compressed formulation, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
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
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # tests
  expect_equal(c(terra::values(s1)), c(0, 1, 1, NA))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("solve (compressed formulation, single zone, negative values)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, 0, 0, NA), nrow = 1))
  locked_in <- 2
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2,  1,  1,  100,  -200, 0), nrow = 1)),
    terra::rast(matrix(c(10, 10, 10, -200, 100, 10), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  # create problem
  expect_warning(
    p <-
      problem(cost, features) %>%
      add_min_set_objective() %>%
      add_absolute_targets(c(2, 10)) %>%
      add_locked_in_constraints(locked_in) %>%
      add_locked_out_constraints(locked_out) %>%
      add_default_solver(gap = 0, verbose = FALSE),
    "negative"
  )
  # solve problem
  s <- solve(p)
  # tests
  expect_equal(c(terra::values(s)), c(0, 1, 1, 0, 0, NA))
})

test_that("compile (expanded formulation, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate targets data
  targ <- floor(terra::global(sim_features, "sum", na.rm = TRUE)[[1]] * 0.25)
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  o <- compile(p, FALSE)
  # calculations for tests
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- terra::nlyr(sim_features)
  rij <- rij_matrix(sim_pu_raster, sim_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(
    o$obj(),
    c(
      sim_pu_raster[[1]][!is.na(sim_pu_raster)],
      rep(0, n_pu * n_f)
    )
  )
  expect_equal(
    o$sense(),
    c(rep("<=", n_pu * n_f), rep(">=", terra::nlyr(sim_features)))
  )
  expect_equal(o$rhs(), c(rep(0, n_pu * n_f), targ))
  expect_equal(
    o$row_ids(),
    c(rep("pu_ijz", n_pu * n_f), rep("spp_target", terra::nlyr(sim_features)))
  )
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("pu_ijz", n_pu * n_f)))
  expect_equal(o$lb(), rep(0, n_pu + (n_pu * n_f)))
  expect_equal(o$ub(), rep(1, n_pu + (n_pu * n_f)))
  # test that model matrix is correct
  row <- 0
  for (i in seq_len(n_f)) {
    for (j in seq_len(n_pu)) {
      row <- row + 1
      curr_row <- rep(0, n_pu + (n_pu * n_f))
      curr_row[j] <- -1
      curr_row[n_pu + ( (i - 1) * n_pu) + j] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
  }
  for (i in seq_len(n_f)) {
    curr_row <- rep(0, n_pu + (n_pu * n_f))
    curr_row[(i * n_pu) + seq_len(n_pu)] <- rij[i, ]
    expect_equal(o$A()[(n_f * n_pu) + i, ], curr_row)
  }
})

test_that("solve (expanded formulation, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
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
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # tests
  expect_equal(c(terra::values(s)), c(0, 1, 1, NA))
})

test_that("invalid inputs (single zone)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # tests
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    compile()
  )
})

test_that("compile (compressed formulation, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_manual_targets(
      tibble::tibble(
        feature = c("feature_1", "feature_2", "feature_3"),
        zone = list("zone_1", "zone_2", c("zone_1", "zone_3")),
        sense = c("<=", ">=", "="),
        type = rep("absolute", 3),
        target = c(4, 5, 6)
      )
    ) %>%
    add_binary_decisions()
  o <- compile(p)
  # calculations for tests
  n_pu <- length(sim_zones_pu_raster[[1]][!is.na(sim_zones_pu_raster[[1]])])
  n_zone <- number_of_zones(sim_zones_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(p$planning_unit_costs()))
  expect_equal(o$sense(), c("<=", ">=", "=", rep("<=", n_pu)))
  expect_equal(o$rhs(), c(4, 5, 6, rep(1, n_pu)))
  expect_equal(o$row_ids(), c(rep("spp_target", 3), rep("pu_zone", n_pu)))
  expect_equal(o$col_ids(), rep("pu", n_pu * n_zone))
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))
  # test model matrix
  m <- matrix(0, nrow = 3 + n_pu, ncol = n_pu * n_zone)
  ## targets
  m[1, seq_len(n_pu)] <- p$data$rij_matrix[[1]][1, ]
  m[2, n_pu + seq_len(n_pu)] <- p$data$rij_matrix[[2]][2, ]
  m[3, seq_len(n_pu)] <- p$data$rij_matrix[[1]][3, ]
  m[3, (n_pu * 2) + seq_len(n_pu)] <- p$data$rij_matrix[[3]][3, ]
  ## zone constraints
  for (i in seq_len(n_pu))
    m[3 + i, c(i, n_pu + i, n_pu + n_pu + i) ] <- 1
  ## convert to sparseMatrix
  m <- as_Matrix(m, "dgCMatrix")
  ## tests
  expect_true(all(m == o$A()))
})

test_that("solve (compressed formulation, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- c(
    terra::rast(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7)),
    terra::rast(matrix(c(10, 10, 10, 10,  4,   1, NA), ncol = 7))
  )
  spp <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7))
  )
  # create problem
  p <-
    problem(costs, zones(spp[[1:2]], spp[[3:4]])) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- p %>% solve()
  # tests
  expect_inherits(s, "SpatRaster")
  expect_equal(c(terra::values(s[[1]])), c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 0, 0,  0, 1, 0, NA))
})

test_that("compile (expanded formulation, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_manual_targets(
      tibble::tibble(
        feature = c("feature_1", "feature_2", "feature_3"),
        zone = list("zone_1", "zone_2", c("zone_1", "zone_3")),
        sense = c("<=", ">=", "="),
        type = rep("absolute", 3),
        target = c(4, 5, 6)
      )
    ) %>%
    add_binary_decisions()
  o <- compile(p, FALSE)
  # calculations for tests
  n_pu <- length(sim_zones_pu_raster[[1]][!is.na(sim_zones_pu_raster[[1]])])
  n_zone <- number_of_zones(sim_zones_features)
  n_feature <- number_of_features(sim_zones_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(
    o$obj(),
    c(c(p$planning_unit_costs()), rep(0, n_pu * n_zone * n_feature))
  )
  expect_equal(
    o$sense(),
    c(rep("<=", n_pu * n_zone * n_feature), "<=", ">=", "=", rep("<=", n_pu))
  )
  expect_equal(
    o$rhs(),
    c(rep(0, n_pu * n_zone * n_feature), 4, 5, 6, rep(1, n_pu))
  )
  expect_equal(
    o$row_ids(),
    c(
      rep("pu_ijz", n_pu * n_zone * n_feature),
      rep("spp_target", 3),
      rep("pu_zone", n_pu)
    )
  )
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu * n_zone), rep("pu_ijz", n_pu * n_zone * n_feature))
  )
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))
  # test model matrix
  m <- matrix(
    0, nrow = n_pu * n_feature * n_zone + 3 + n_pu,
    ncol = (n_pu * n_zone) + (n_pu * n_zone * n_feature)
  )
  ## allocation constraints
  r <- 0
  for (z in seq_len(n_zone)) {
    for (i in seq_len(n_feature)) {
      for (j in seq_len(n_pu)) {
        r <- r + 1
        m[r, ((z - 1) * n_pu) + j] <- -1
        idx <- (n_pu * n_zone) + ((z - 1) * n_pu * n_feature) +
               ((i - 1) * n_pu) + j
        m[r, idx] <- 1
      }
    }
  }
  ## targets
  r <- r + 1
  m[r, (n_pu * n_zone) + seq_len(n_pu)] <- p$data$rij_matrix[[1]][1, ]
  r <- r + 1
  col <- (n_pu * n_zone) + (1 * n_pu * n_feature) + (1 * n_pu) + seq_len(n_pu)
  m[r, col] <- p$data$rij_matrix[[2]][2, ]
  r <- r + 1
  col <- (n_pu * n_zone) + (0 * n_pu * n_feature) + (2 * n_pu) + seq_len(n_pu)
  m[r, col] <- p$data$rij_matrix[[1]][3, ]
  col <- (n_pu * n_zone) + (2 * n_pu * n_feature) + (2 * n_pu) + seq_len(n_pu)
  m[r, col] <- p$data$rij_matrix[[3]][3, ]
  ## zone constraints
  for (i in seq_len(n_pu))
    m[r + i, c(i, n_pu + i, n_pu + n_pu + i)] <- 1
  ## convert to sparseMatrix
  m <- as_Matrix(m, "dgCMatrix")
  ## tests
  expect_true(all(m == o$A()))
})

test_that("solve (expanded formulation, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- c(
    terra::rast(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7)),
    terra::rast(matrix(c(10, 10, 10, 10,  4,   1, NA), ncol = 7))
  )
  spp <- c(
    terra::rast(matrix(c(1,  2,  0,  0,   0,  0,  0), ncol = 7)),
    terra::rast(matrix(c(NA, 0,  1,  1,   0,  0,  0), ncol = 7)),
    terra::rast(matrix(c(1,  0,  0,  0,   1,  0,  0), ncol = 7)),
    terra::rast(matrix(c(0,  0,  0,  0,   0,  10, 0), ncol = 7))
  )
  # create problem
  p <-
    problem(costs, zones(spp[[1:2]], spp[[3:4]])) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # tests
  expect_inherits(s, "SpatRaster")
  expect_equal(c(terra::values(s[[1]])), c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 0, 0,  0, 1, 0, NA))
})

test_that("invalid inputs (multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # tests
  expect_tidy_error(
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    compile()
  )
})
