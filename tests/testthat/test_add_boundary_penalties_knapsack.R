test_that("minimum set objective (compile, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(3, 0.5, "knapsack")
  o <- compile(p)
  # print
  suppressMessages(print(p))
  suppressMessages(summary(p))
  # calculations for tests
  ## number of planning units
  n_pu <- p$number_of_planning_units()
  ## number of features
  n_f <- p$number_of_features()
  ## prepare boundary calculations
  b_data <- boundary_matrix(p$data$cost)
  b_data <- b_data[p$planning_unit_indices(), p$planning_unit_indices()]
  b_exterior <- c(
    Matrix::diag(b_data) - (Matrix::rowSums(b_data) - Matrix::diag(b_data))
  )
  b_total <- Matrix::diag(b_data)
  b_interior <- b_total - b_exterior
  ## calculate scaled costs with total boundaries
  b_sc_interior <- 3 * b_interior
  b_sc_exterior <- 3 * 0.5 * b_exterior
  ## prepare scaled shared lengths
  Matrix::diag(b_data) <- 0
  b_data <- Matrix::drop0(b_data)
  b_data <- as_Matrix(b_data * 3, "dgTMatrix")
  ## objectives for boundary decision variables
  b_obj <- o$obj()[n_pu + seq_len(n_pu * 2)]
  ## lower bound for boundary decision variables
  b_lb <- o$lb()[n_pu + seq_len(n_pu * 2)]
  ## upper bound for boundary decision variables
  b_ub <- o$ub()[n_pu + seq_len(n_pu * 2)]
  ## vtype bound for boundary decision variables
  b_vtype <- o$vtype()[n_pu + seq_len(n_pu * 2)]
  ## pu costs including exterior boundary
  pu_costs <- o$obj()[seq_len(n_pu)]
  ## matrix labels
  b_col_labels <- o$col_ids()[n_pu + seq_len(n_pu * 2)]
  b_row_labels <- o$row_ids()[n_f + seq_len(n_pu * 2)]
  ## sense for boundary decision constraints
  b_sense <- o$sense()[n_f + seq_len(n_pu * 2)]
  ## rhs for boundary decision constraints
  b_rhs <- o$rhs()[n_f + seq_len(n_pu * 2)]
  # tests
  expect_equal(b_col_labels, c(rep("b1", n_pu), rep("b2", n_pu)))
  expect_equal(pu_costs, p$planning_unit_costs()[, 1] + b_sc_exterior)
  expect_equal(b_obj, c(rep(0, n_pu), b_sc_interior))
  expect_equal(b_lb, rep(0, n_pu * 2))
  expect_equal(b_ub, rep(1, n_pu * 2))
  expect_equal(b_vtype, rep("C", n_pu * 2))
  expect_equal(b_row_labels, rep(c("b1", "b2"), each = n_pu))
  expect_equal(b_sense, c(rep(">=", n_pu), rep("<=", n_pu)))
  expect_equal(b_rhs, c(rep(1, n_pu), b_sc_interior))
  b_A <- o$A()[seq(n_f + 1, n_f + n_pu + n_pu), , drop = FALSE]
  correct_b_A <- Matrix::sparseMatrix(i = 1, j = 1, x = 0, dims = dim(b_A))
  correct_b_A[matrix(
    c(rep(seq_len(n_pu), 2), seq_len(n_pu), n_pu + seq_len(n_pu)),
    ncol = 2
  )] <- 1
  correct_b_A[
    matrix(
      c(n_pu + seq_len(n_pu), seq_len(n_pu)),
      ncol = 2
  )] <- b_sc_interior
  correct_b_A[
    matrix(
      c(n_pu + seq_len(n_pu), n_pu + n_pu + seq_len(n_pu)),
      ncol = 2
  )] <- -b_sc_interior
  correct_b_A[matrix(
    c(n_pu + b_data@i + 1, n_pu + b_data@j + 1),
    ncol = 2
  )] <- b_data@x
  expect_equal(b_A, Matrix::drop0(correct_b_A))
})

test_that("minimum set objective (obj fun, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problems
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(10000, 1, "knapsack") %>%
    add_default_solver(gap = 0, verbose = FALSE)
  s <- solve(p)
  # calculations for tests
  obj_value <- unname(attr(s, "objective"))
  total_perim <- terra::perim(
    terra::as.polygons(terra::clamp(s, lower = 0.5, values = FALSE))
  )
  # tests
  expect_equal(
    terra::global(sim_pu_raster * s, "sum", na.rm = TRUE)[[1]] +
      (10000 * total_perim),
    obj_value,
    tolerance = 1e-6
  )
})

test_that("minimum set objective (compile, multiple zones)", {
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create zones data
  penalty <- 5
  p_zones <- matrix(0, ncol = 3, nrow = 3)
  diag(p_zones) <- c(0.7, 0.8, 0.9)
  p_zones[upper.tri(p_zones)] <- c(0.1, 0.2, 0.3)
  p_zones[lower.tri(p_zones)] <- p_zones[upper.tri(p_zones)]
  p_edge_factor <- seq(0.1, 0.1 * 3, 0.1)
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty, p_edge_factor, "knapsack", p_zones)
  o <- compile(p)
  # create variables for tests
  ## number of planning units
  n_pu <- p$number_of_planning_units()
  ## number of features
  n_f <- p$number_of_features()
  ## number of zones
  n_z <- p$number_of_zones()
  ## generate boundary data
  pu_indices <- p$planning_unit_indices()
  b_matrix <- boundary_matrix(p$data$cost)[pu_indices, pu_indices]
  b_exterior <- c(
    Matrix::diag(b_matrix) -
      (Matrix::rowSums(b_matrix) - Matrix::diag(b_matrix))
  )
  b_total <- Matrix::diag(b_matrix)
  ## create matrix with the scaled boundary components
  indices <- as.matrix(expand.grid(i = seq_len(n_z), j = seq_len(n_z)))
  # create list of matrices
  scb_list <- list()
  b_sc_exterior <- rep(0, n_pu * n_z)
  b_sc_interior <- rep(0, n_pu * n_z)
  for (i in seq_len(nrow(indices))) {
    ## extract data
    curr_m <- b_matrix
    ## prepare scaled shared lengths
    Matrix::diag(curr_m) <- 0
    curr_m <- Matrix::drop0(curr_m)
    curr_m <- as_Matrix(
      curr_m * penalty * p_zones[indices[i, 1], indices[i, 2]],
      "dgTMatrix"
    )
    ## if z1 == z2, then store interior and exterior costs
    if (indices[i, 1] == indices[i, 2]) {
      ## store interior and exterior edge values
      idx <- ((indices[i, 1] - 1) * n_pu) + seq_len(n_pu)
      b_sc_exterior[idx] <-
        b_sc_exterior[idx] +
        penalty * p_zones[indices[i, 1], indices[i, 2]] *
        (b_exterior * p_edge_factor[indices[i, 1]])
      b_sc_interior[idx] <-
        b_sc_interior[idx] +
        penalty * p_zones[indices[i, 1], indices[i, 2]] *
        (b_total - b_exterior)
    }
    ## remove diagonal values
    Matrix::diag(curr_m) <- 0
    ## store result
    scb_list[[i]] <- curr_m
  }
  names(scb_list) <- paste0("z", indices[, 1], "_z", indices[, 2])
  # calculate number of variables for penalties
  n_z_p <- n_pu * n_z * 2
  ## calculate objective function
  correct_obj <- c(
    c(p$planning_unit_costs()) + b_sc_exterior,
    rep(0, n_pu * n_z),
    b_sc_interior
  )
  # tests
  expect_equal(o$obj(), correct_obj)
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + n_z_p))
  expect_equal(o$ub(), rep(1, (n_pu * n_z) + n_z_p))
  expect_equal(o$vtype(), c(rep("B", n_pu * n_z), rep("C", n_z_p)))
  expect_equal(
    o$row_ids(),
    c(
      rep("spp_target", n_f * n_z),
      rep("pu_zone", n_pu),
      rep(c("b1", "b2"), each = n_pu * n_z)
    )
  )
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu * n_z), c(rep("b1", n_pu * n_z), rep("b2", n_pu * n_z)))
  )
  expect_equal(
    o$sense(),
    c(
      rep(">=", n_f * n_z), rep("<=", n_pu),
      rep(c(">=", "<="), each = n_pu * n_z)
    )
  )
  expect_equal(
    o$rhs(),
    c(rep(0.1, n_f * n_z), rep(1, n_pu), rep(1, n_z * n_pu), b_sc_interior)
  )
  ## check model matrix is defined correctly
  oA <- o$A()
  ## targets
  oA_targets <- oA[seq_len(n_z * n_f), , drop = FALSE]
  correct_oA_targets <- Matrix::sparseMatrix(
    i = 1, j = 1, x = 0, dims = dim(oA_targets)
  )
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_f)) {
      correct_oA_targets[
        ((z - 1) * n_f) + i,
        ((z - 1) * n_pu) + seq_len(n_pu)
      ] <- p$data$rij_matrix[[z]][i, ]
    }
  }
  expect_equal(oA_targets, Matrix::drop0(correct_oA_targets))
  ## zone constraints
  oA_zones <- oA[seq((n_z * n_f) + 1, (n_z * n_f) + n_pu), , drop = FALSE]
  correct_oA_zones <- Matrix::sparseMatrix(
    i = 1, j = 1, x = 0, dims = dim(oA_zones)
  )
  for (j in seq_len(n_pu)) {
    for (z in seq_len(n_z)) {
      correct_oA_zones[j, ((z - 1) * n_pu) + j] <- 1
    }
  }
  expect_equal(oA_zones, Matrix::drop0(correct_oA_zones))
  ## penalty variable constraints
  oA_b <- oA[
    seq((n_z * n_f) + n_pu + 1, (n_z * n_f) + n_pu + n_z_p), ,
    drop = FALSE
  ]
  correct_oA_b <- Matrix::sparseMatrix(
    i = 1, j = 1, x = 0, dims = dim(oA_b)
  )
  correct_oA_b[matrix(
    c(
      rep(seq_len(n_pu * n_z), 2), seq_len(n_pu * z),
      n_z * n_pu + seq_len(n_pu * n_z)
    ),
    ncol = 2
  )] <- 1
  correct_oA_b[matrix(
    c((n_z * n_pu) + seq_len(n_pu * n_z), seq_len(n_pu * n_z)),
    ncol = 2
  )] <- b_sc_interior
  correct_oA_b[matrix(
    c(
      (n_z * n_pu) + seq_len(n_z * n_pu),
      (n_z * n_pu) + (n_z * n_pu) + seq_len(n_z * n_pu)
    ),
    ncol = 2
  )] <- -b_sc_interior
  for (i in seq_len(nrow(indices))) {
    idx1 <- (n_pu * n_z) + ((indices[i, 1] - 1) * n_pu) + scb_list[[i]]@i + 1
    idx2 <- (n_pu * n_z) + ((indices[i, 2] - 1) * n_pu) + scb_list[[i]]@j + 1
    correct_oA_b[matrix(c(idx1, idx2), ncol = 2)] <- scb_list[[i]]@x
  }
  expect_equal(oA_b, Matrix::drop0(correct_oA_b))
})

test_that("minimum set objective (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate zones data
  m <- matrix(
    c(
      1, 0, 0,
      0, 1, 0,
      0, 0, 1
    ),
    byrow = TRUE, ncol = 3
  )
  # create baseline problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.025, ncol = 3, nrow = 5)) %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE, gap = 0.2, time_limit = 10)
  # create and solve problems
  s <-
    p %>%
    add_boundary_penalties(300, rep(1, 3), "knapsack", m) %>%
    solve()
  # calculations for tests
  obj_value <- unname(attr(s, "objective"))
  total_perim <- terra::perim(
    terra::as.polygons(
      max(terra::clamp(s, lower = 0.5, values = FALSE), na.rm = TRUE)
    )
  )
  # tests
  expect_equal(
    sum(terra::global(sim_zones_pu_raster * s, "sum", na.rm = TRUE)[[1]]) +
      (300 * total_perim),
    obj_value,
    tolerance = 1e-6
  )
  expect_inherits(s, "SpatRaster")
  expect_true(all_binary(s[[1]]))
  expect_true(all_binary(s[[2]]))
  expect_true(all_binary(s[[3]]))
})

test_that("invalid inputs (single zones)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate zones data
  z <- diag(terra::nlyr(sim_pu_raster))
  m <- boundary_matrix(sim_pu_raster)
  # prepare problem
  p <- problem(sim_pu_raster, sim_features)
  # tests
  expect_error(
    add_boundary_penalties(p, -1, rep(1, ncol(z)), "knapsack", z, m),
    "positive"
  )
  expect_error(
    add_boundary_penalties(p, 1, rep(1, ncol(z)), "knapsack", -z, m),
    "positive"
  )
  expect_error(
    add_boundary_penalties(p, 1, rep(1, ncol(z)), "knapsack", z, -m),
    "positive"
  )
})

test_that("invalid inputs (multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate zones data
  z <- diag(terra::nlyr(sim_zones_pu_raster))
  m <- boundary_matrix(sim_zones_pu_raster)
  # prepare problem
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  # tests
  expect_error(
    add_boundary_penalties(p, -1, rep(1, ncol(z)), "knapsack", z, m),
    "positive"
  )
  expect_error(
    add_boundary_penalties(p, 1, rep(1, ncol(z)), "knapsack", -z, m),
    "positive"
  )
  expect_error(
    add_boundary_penalties(p, 1, rep(1, ncol(z)), "knapsack", z, -m),
    "positive"
  )
})
