test_that("compile (compressed formulation, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_mandatory_allocation_constraints() %>%
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
  n_pu <- p$number_of_planning_units()
  n_zone <- number_of_zones(sim_zones_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(p$planning_unit_costs()))
  expect_equal(o$sense(), c("<=", ">=", "=", rep("=", n_pu)))
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

test_that("compile (expanded formulation, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_mandatory_allocation_constraints() %>%
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
  n_pu <- p$number_of_planning_units()
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
    c(rep("<=", n_pu * n_zone * n_feature), "<=", ">=", "=", rep("=", n_pu))
  )
  expect_equal(
    o$rhs(),
    c(rep(0, n_pu * n_zone * n_feature), 4, 5, 6, rep(1, n_pu))
  )
  expect_equal(
    o$row_ids(),
    c(
      rep("pu_ijz", n_pu * n_zone * n_feature), rep("spp_target", 3),
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
        m[r, (n_pu * n_zone) +
             ((z - 1) * n_pu * n_feature) +
             ((i - 1) * n_pu) +
             j] <- 1
      }
    }
  }
  ## targets
  r <- r + 1
  m[r, (n_pu * n_zone) + seq_len(n_pu)] <- p$data$rij_matrix[[1]][1, ]
  r <- r + 1
  j <- (n_pu * n_zone) + (1 * n_pu * n_feature) + (1 * n_pu) + seq_len(n_pu)
  m[r, j] <- p$data$rij_matrix[[2]][2, ]
  r <- r + 1
  j <- (n_pu * n_zone) + (0 * n_pu * n_feature) + (2 * n_pu) + seq_len(n_pu)
  m[r, j] <- p$data$rij_matrix[[1]][3, ]
  j <- (n_pu * n_zone) + (2 * n_pu * n_feature) + (2 * n_pu) + seq_len(n_pu)
  m[r, j] <- p$data$rij_matrix[[3]][3, ]
  ## zone constraints
  for (i in seq_len(n_pu))
    m[r + i, c(i, n_pu + i, n_pu + n_pu + i)] <- 1
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
  # create and solve problem
  s <-
    problem(costs, zones(spp[[1:2]], spp[[3:4]])) %>%
    add_min_set_objective() %>%
    add_mandatory_allocation_constraints() %>%
    add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve()
  # tests
  expect_inherits(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 2)
  expect_equal(c(terra::values(s[[1]])), c(1, 1, NA, 1, 0, 0, NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 0, 1,  0, 1, 1, NA))
})

test_that("solve (expanded formulation, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
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
  # create and solve problem
  s <-
    problem(costs, zones(spp[[1:2]], spp[[3:4]])) %>%
    add_min_set_objective() %>%
    add_mandatory_allocation_constraints() %>%
    add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve(compressed_formulation = FALSE)
  # tests
  expect_inherits(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 2)
  expect_equal(c(terra::values(s[[1]])), c(1, 1, NA, 1, 0, 0, NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 0, 1,  0, 1, 1, NA))
})

test_that("throws error for problem with single zone", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # tests
  expect_tidy_error(
    problem(sim_pu_raster, sim_features) %>%
    add_mandatory_allocation_constraints()
  )
})
