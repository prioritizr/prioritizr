test_that("minimum set objective (compile, single zone)", {
  # make and compile problems
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create connectivity matrix data
  cmatrix <- matrix(
    0, nrow = terra::ncell(sim_pu_raster), ncol = terra::ncell(sim_pu_raster)
  )
  cmatrix[] <- runif(length(cmatrix))
  cmatrix[cmatrix[] < 0.9] <- 0
  cmatrix[terra::cells(is.na(sim_pu_raster), 0)[[1]]] <- 0
  cmatrix <- Matrix::drop0(as_Matrix(cmatrix, "dgCMatrix"))
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_asym_connectivity_penalties(0.5, data = cmatrix)
  o <- compile(p)
  # run tests
  ## create variables for debugging
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  pu_indices <- p$planning_unit_indices()
  c_data <- cmatrix[pu_indices, pu_indices]
  c_data <- c_data * -0.5
  # connectivity weights for each planning unit
  c_weights <- Matrix::diag(c_data)
  # i,j,x matrix for planning unit boundaries
  Matrix::diag(c_data) <- 0
  c_data <- Matrix::drop0(c_data)
  c_data <- as_Matrix(c_data, "dgTMatrix")
  # objectives for connectivity decision variables
  c_obj <- o$obj()[n_pu + seq_len(length(c_data@i))]
  # lower bound for connectivity decision variables
  c_lb <- o$lb()[n_pu + seq_len(length(c_data@i))]
  # upper bound for connectivity decision variables
  c_ub <- o$ub()[n_pu + seq_len(length(c_data@i))]
  # vtype bound for connectivity decision variables
  c_vtype <- o$vtype()[n_pu + seq_len(length(c_data@i))]
  # pu costs including total connectivity
  pu_costs <- o$obj()[seq_len(n_pu)]
  # matrix labels
  c_col_labels <- o$col_ids()[n_pu + seq_len(length(c_data@i))]
  c_row_labels <- o$row_ids()[n_f + seq_len(length(c_data@i) * 2)]
  # sense for connectivity decision constraints
  c_sense <- o$sense()[n_f + seq_len(length(c_data@i) * 2)]
  # rhs for connectivity decision constraints
  c_rhs <- o$rhs()[n_f + seq_len(length(c_data@i) * 2)]
  ## check that constraints added correctly
  expect_equal(
    pu_costs,
    p$planning_unit_costs()[, 1] + c_weights + (-1 * Matrix::rowSums(c_data))
  )
  expect_equal(c_obj, c_data@x)
  expect_equal(c_lb, rep(0, length(c_data@i)))
  expect_equal(c_ub, rep(1, length(c_data@i)))
  expect_equal(c_vtype, rep("C", length(c_data@i)))
  expect_equal(c_col_labels, rep("ac", length(c_data@i)))
  expect_equal(c_row_labels, rep(c("ac1", "ac2"), length(c_data@i)))
  expect_equal(c_sense, rep(c("<=", "<="), length(c_data@i)))
  expect_equal(c_rhs, rep(c(0, 0), length(c_data@i)))
  c_A <- o$A()[seq(n_f + 1, n_f + (length(c_data@i) * 2)), , drop = FALSE]
  correct_c_A <- Matrix::sparseMatrix(i = 1, j = 1, x = 0, dims = dim(c_A))
  correct_c_A[matrix(
    c(seq(1, nrow(correct_c_A), 2), n_pu + seq_along(c_data@i)),
    ncol = 2
  )] <- 1
  correct_c_A[
    matrix(
      c(seq(1, nrow(correct_c_A), 2), c_data@i + 1),
      ncol = 2
  )] <- -1
  correct_c_A[matrix(
    c(seq(2, nrow(correct_c_A), 2), n_pu + seq_along(c_data@i)),
    ncol = 2
  )] <- 1
  correct_c_A[matrix(
    c(seq(2, nrow(correct_c_A), 2), c_data@j + 1),
    ncol = 2
  )] <- -1
  expect_equal(c_A, Matrix::drop0(correct_c_A))
})

test_that("maximum features objective (compile, single zone)", {
  # make and compile problems
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create connectivity matrix data
  cmatrix <- matrix(
    0, nrow = terra::ncell(sim_pu_raster), ncol = terra::ncell(sim_pu_raster)
  )
  cmatrix[] <- runif(length(cmatrix), -5, 5)
  cmatrix[abs(cmatrix[]) < 4] <- 0
  cmatrix[terra::cells(is.na(sim_pu_raster), 0)[[1]]] <- 0
  cmatrix <- Matrix::drop0(as_Matrix(cmatrix, "dgCMatrix"))
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_max_features_objective(100) %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_asym_connectivity_penalties(0.5, data = cmatrix)
  o <- compile(p)
  # run tests
  ## create variables for debugging
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  pu_indices <- p$planning_unit_indices()
  c_data <- cmatrix[pu_indices, pu_indices]
  c_data <- c_data * -0.5
  # connectivity weights for each planning unit
  c_weights <- Matrix::diag(c_data)
  # i,j,x matrix for planning unit boundaries
  Matrix::diag(c_data) <- 0
  c_data <- Matrix::drop0(c_data)
  c_data <- as_Matrix(c_data, "dgTMatrix")
  # objectives for connectivity decision variables
  c_obj <- o$obj()[n_pu + n_f + seq_len(length(c_data@i))]
  # lower bound for connectivity decision variables
  c_lb <- o$lb()[n_pu + n_f + seq_len(length(c_data@i))]
  # upper bound for connectivity decision variables
  c_ub <- o$ub()[n_pu + n_f + seq_len(length(c_data@i))]
  # vtype bound for connectivity decision variables
  c_vtype <- o$vtype()[n_pu + n_f +  seq_len(length(c_data@i))]
  # pu costs including total connectivity
  pu_costs <- o$obj()[seq_len(n_pu)]
  scaled_costs <- c(p$planning_unit_costs())
  scaled_costs <- scaled_costs * (-0.01 / sum(scaled_costs, na.rm = TRUE))
  # matrix labels
  c_col_labels <- o$col_ids()[n_pu + n_f + seq_len(length(c_data@i))]
  c_row_labels <-
    o$row_ids()[n_f + 1 + seq_len(length(o$row_ids()) - n_f - 1)]
  # sense for connectivity decision constraints
  c_sense <- o$sense()[n_f + 1 + seq_along(c_row_labels)]
  # rhs for connectivity decision constraints
  c_rhs <- o$rhs()[n_f + 1 + seq_along(c_row_labels)]
  ## check that constraints added correctly
  expect_equal(
    pu_costs,
    scaled_costs + (-1 * c_weights) + Matrix::rowSums(c_data)
  )
  expect_equal(c_obj, -c_data@x)
  expect_equal(c_lb, rep(0, length(c_data@i)))
  expect_equal(c_ub, rep(1, length(c_data@i)))
  expect_equal(c_vtype, rep("C", length(c_data@i)))
  expect_equal(c_col_labels, rep("ac", length(c_data@i)))
  expect_equal(
    c_row_labels,
    unlist(lapply(c_data@x, function(x) {
      if (x > 0) return(c("ac1", "ac2", "ac3"))
      c("ac1", "ac2")
    }))
  )
  expect_equal(
    c_rhs,
    unlist(lapply(c_data@x, function(x) {
      if (x > 0) return(c(0, 0, -1))
      c(0, 0)
    }))
  )
  expect_equal(
    c_sense,
    unlist(lapply(c_data@x, function(x) {
      if (x > 0) return(c("<=", "<=", ">="))
      c("<=", "<=")
    }))
  )
  c_A <- o$A()[seq(n_f + 2, n_f + 1 + length(c_rhs)), , drop = FALSE]
  correct_c_A <- Matrix::sparseMatrix(i = 1, j = 1, x = 0, dims = dim(c_A))
  counter <- 0
  for (i in seq_along(c_data@i)) {
    counter <- counter + 1
    correct_c_A[counter, n_pu + n_f + i] <- 1
    correct_c_A[counter, c_data@i[i] + 1] <- -1
    counter <- counter + 1
    correct_c_A[counter, n_pu + n_f + i] <- 1
    correct_c_A[counter, c_data@j[i] + 1] <- -1
    if (c_data@x[i] > 0) {
      counter <- counter + 1
      correct_c_A[counter, n_pu + n_f + i] <- 1
      correct_c_A[counter, c_data@i[i] + 1] <- -1
      correct_c_A[counter, c_data@j[i] + 1] <- -1
    }
  }
  expect_equal(c_A, Matrix::drop0(correct_c_A))
})

test_that("minimum set objective (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # crop data
  ext <- terra::ext(0, 0.4, 0.6, 1)
  pu <- terra::crop(sim_pu_raster, ext)
  feats <- terra::crop(sim_features, ext)
  # create asymmetric connectivity matrix
  # here the first 5 planning units have very high connectivity with
  # their adjacent planning units, and the rest of the connections
  # are adjacent
  cmatrix <- adjacency_matrix(pu)
  cmatrix <- (Matrix::tril(cmatrix) * 5)
  cmatrix[seq(9, nrow(cmatrix)), ] <- 0
  cmatrix[, seq(9, nrow(cmatrix))] <- 0
  cmatrix <- Matrix::drop0(cmatrix)
  # create a locked in matrix
  locked_in <- pu * 0
  locked_in[6] <- 1
  # create and solve problem
  p1 <-
    problem(pu, feats) %>%
    add_min_set_objective() %>%
    add_absolute_targets(0) %>%
    add_asym_connectivity_penalties(1000, data = cmatrix) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0.0, verbose = FALSE)
  s1_1 <- solve(p1)
  s1_2 <- solve(p1)
  p2 <-
    problem(pu, feats) %>%
    add_min_set_objective() %>%
    add_absolute_targets(0) %>%
    add_asym_connectivity_penalties(1000, data = cmatrix) %>%
    add_locked_in_constraints(locked_in) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0.0, verbose = FALSE)
  s2_1 <- solve(p2)
  s2_2 <- solve(p2)
  # tests
  expect_inherits(s1_1, "SpatRaster")
  expect_inherits(s1_2, "SpatRaster")
  expect_true(all(na.omit(unique(terra::values(s1_1))) == 0))
  expect_equal(terra::values(s1_1), terra::values(s1_2))
  expect_inherits(s2_1, "SpatRaster")
  expect_inherits(s2_2, "SpatRaster")
  expect_equal(
    c(terra::values(s2_1)),
    c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
  expect_equal(terra::values(s2_1), terra::values(s2_2))
})

test_that("invalid inputs (single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  c_matrix <- as_Matrix(boundary_matrix(sim_pu_raster), "dgCMatrix")
  c_matrix@x <- c_matrix@x + runif(length(c_matrix@x))
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  expect_tidy_error(
    add_asym_connectivity_penalties(p, NA_real_, data = c_matrix)
  )
  expect_tidy_error(
    add_asym_connectivity_penalties(p, 1, 0, data = c_matrix)
  )
  expect_tidy_error(
    add_asym_connectivity_penalties(p, 5, data = c_matrix[, -1])
  )
  expect_tidy_error(
    add_asym_connectivity_penalties(p, 5, data = c_matrix[-1, ])
  )
  c_matrix2 <- boundary_matrix(sim_pu_raster)
  expect_warning(
    add_asym_connectivity_penalties(p, 5, data = c_matrix2),
    "asymmetric"
  )
})

test_that("minimum set objective (compile, multiple zones)", {
  # load data
  set.seed(500)
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  sim_zones_pu_polygons <- sim_zones_pu_polygons[seq_len(20), ]
  # prepare data for problem
  cm <-
    adjacency_matrix(sim_zones_pu_polygons) *
    matrix(
      runif(nrow(sim_zones_pu_polygons)^2),
      ncol = nrow(sim_zones_pu_polygons),
      nrow = nrow(sim_zones_pu_polygons)
    )
  zm <- matrix(seq_len(9) * 0.1, ncol = 3)
  # make and compile problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
    add_asym_connectivity_penalties(100, zm, cm) %>%
    add_binary_decisions()
  o <- compile(p)
  ## prepare data for tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  # prepare matrix
  c_data <- cm * -100
  c_weights <- rep(Matrix::diag(c_data), n_z) * rep(diag(zm), each = n_pu)
  Matrix::diag(c_data) <- 0
  c_data <- Matrix::drop0(c_data)
  c_data <- as_Matrix(c_data, "dgTMatrix")
  c_penalties <- c()
  for (i in seq_len(n_z)) {
    for (j in seq_len(n_z)) {
      c_penalties <- c(c_penalties, c_data@x * zm[i, j])
    }
  }
  c_totals <- rep(0, n_pu * n_z)
  for (z1 in seq_len(n_z)) {
    for (z2 in seq_len(n_z)) {
      for (i in seq_len(n_pu)) {
        for (j in seq_len(n_pu)) {
          if (!((i == j) && (z1 == z2))) {
            idx <- ((z1 - 1) * (n_pu)) + i
            c_totals[idx] <- c_totals[idx] + (c_data[i, j] * zm[z1, z2])
          }
        }
      }
    }
  }
  # objectives for connectivity decision variables
  c_obj <- o$obj()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  # lower bound for connectivity decision variables
  c_lb <- o$lb()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  # upper bound for connectivity decision variables
  c_ub <- o$ub()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  # vtype bound for connectivity decision variables
  c_vtype <- o$vtype()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  # pu costs including connectivity penalties
  pu_costs <- o$obj()[seq_len(n_pu * n_z)]
  # matrix labels
  c_col_labels <-
    o$col_ids()[(n_pu * n_z) + seq_len(length(c_data@i)  * n_z * n_z)]
  c_row_labels <-
    o$row_ids()[(n_f * n_z) + n_pu + seq_len(length(c_data@i) * 2 * n_z * n_z)]
  # sense for connectivity decision constraints
  c_sense <-
    o$sense()[(n_f * n_z) + n_pu + seq_len(length(c_data@i) * 2 * n_z * n_z)]
  # rhs for connectivity decision constraints
  c_rhs <-
    o$rhs()[(n_f * n_z) + n_pu + seq_len(length(c_data@i) * 2 * n_z * n_z)]
  # run tests
  expect_equal(
    pu_costs,
    c(p$planning_unit_costs()) + c_weights + (-1 * c_totals)
  )
  expect_equal(c_obj, c_penalties)
  expect_equal(c_lb, rep(0, length(c_data@i) * n_z * n_z))
  expect_equal(c_ub, rep(1, length(c_data@i) * n_z * n_z))
  expect_equal(c_vtype, rep("C", length(c_data@i) * n_z * n_z))
  expect_equal(c_col_labels, rep("ac", length(c_data@i) * n_z * n_z))
  expect_equal(c_row_labels, rep(c("ac1", "ac2"), length(c_data@i) * n_z * n_z))
  expect_equal(c_sense, rep(c("<=", "<="), length(c_data@i) * n_z * n_z))
  expect_equal(c_rhs, rep(c(0, 0), length(c_data@i) * n_z * n_z))
  c_A <- o$A()[
    seq(
      (n_f * n_z) + n_pu + 1,
      (n_f * n_z) + n_pu + (length(c_data@i) * 2 * n_z * n_z)
    ), , drop = FALSE
  ]
  correct_c_A <- Matrix::sparseMatrix(i = 1, j = 1, x = 0, dims = dim(c_A))
  counter <- 0
  counter2 <- 0
  for (i in seq_len(n_z)) {
    for (j in seq_len(n_z)) {
      for (k in seq_along(c_data@i)) {
        counter <- counter + 1
        counter2 <- counter2 + 1
        correct_c_A[counter, (n_pu * n_z) + counter2] <- 1
        correct_c_A[counter, ((i - 1) * n_pu) + c_data@i[k] + 1] <- -1
        counter <- counter + 1
        correct_c_A[counter, (n_pu * n_z) + counter2] <- 1
        correct_c_A[counter, ((j - 1) * n_pu) + c_data@j[k] + 1] <- -1
      }
    }
  }
  expect_equal(c_A, Matrix::drop0(correct_c_A))
})

test_that("minimum set objective (compile, array data, multiple zones)", {
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # prepare data for problem
  cm <- as_Matrix(adjacency_matrix(sim_zones_pu_polygons), "dgCMatrix")
  cm@x <- cm@x + runif(length(cm@x))
  zm <- matrix(seq_len(9) * 0.1, ncol = 3)
  ca <- array(0, dim = c(dim(cm), 3, 3))
  for (i in seq_len(3))
    for (j in seq_len(3))
      ca[, , i, j] <- as.matrix(cm * zm[i, j])
  # make and compile problems
  p1 <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
    add_asym_connectivity_penalties(100, zm, cm) %>%
    add_binary_decisions()
  p2 <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
    add_asym_connectivity_penalties(100, NULL, ca) %>%
    add_binary_decisions()
  o1 <- compile(p1)
  o2 <- compile(p2)
  # run tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$modelsense(), o2$modelsense())
  expect_equal(o1$A(), o2$A())
})

test_that("minimum set objective (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  ext <- terra::ext(0, 0.4, 0.6, 1)
  pu <- terra::crop(sim_zones_pu_raster, ext)
  feats <- lapply(sim_zones_features, terra::crop, ext)
  feats <- do.call(
    zones,
    append(
      feats,
      list(
        zone_names = zone_names(sim_zones_features),
        feature_names = feature_names(sim_zones_features)
      )
    )
  )
  # make zones matrices
  zm <- matrix(0, ncol = 3, nrow = 3)
  zm[1, 1] <- 1
  zm[3, 2] <- 1
  # create targets matrix
  targets <- matrix(
    0, nrow = length(feature_names(sim_zones_features)),
    ncol = length(zone_names(sim_zones_features))
  )
  # create asymmetric connectivity matrix
  # here the first 5 planning units have very high connectivity with
  # their adjacent planning units, and the rest of the connections
  # are adjacent
  cmatrix <- adjacency_matrix(pu)
  cmatrix <- (Matrix::tril(cmatrix) * 5)
  cmatrix[seq(9, nrow(cmatrix)), ] <- 0
  cmatrix[, seq(9, nrow(cmatrix))] <- 0
  cmatrix <- Matrix::drop0(cmatrix)
  # create a locked in matrix
  locked_in1 <- (pu[[1]] * 0)[[rep(1, 3)]]
  names(locked_in1) <- zone_names(sim_zones_features)
  locked_in1[[1]][6] <- 1
  locked_in2 <- locked_in1
  locked_in2[[3]][4] <- 1
  # create and solve problem
  p1 <-
    problem(pu, feats) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_asym_connectivity_penalties(1000, zm, data = cmatrix) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0.0, verbose = FALSE)
  s1_1 <- solve(p1)
  s1_2 <- solve(p1)
  p2 <-
    problem(pu, feats) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_asym_connectivity_penalties(1000, zm, data = cmatrix) %>%
    add_locked_in_constraints(locked_in1) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0.0, verbose = FALSE)
  s2_1 <- solve(p2)
  s2_2 <- solve(p2)
  p3 <-
    problem(pu, feats) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_asym_connectivity_penalties(1000, zm, data = cmatrix) %>%
    add_locked_in_constraints(locked_in2) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0.0, verbose = FALSE)
  s3_1 <- solve(p3)
  s3_2 <- solve(p3)
  # tests
  expect_inherits(s1_1, "SpatRaster")
  expect_inherits(s1_2, "SpatRaster")
  expect_true(all(terra::values(sum(s1_1)) < 0.5, na.rm = TRUE))
  expect_equal(terra::values(s1_1), terra::values(s1_2))
  expect_inherits(s2_1, "SpatRaster")
  expect_inherits(s2_2, "SpatRaster")
  expect_equal(
    as.data.frame(terra::values(s2_1)),
    data.frame(
      zone_1 = c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0, NA, NA, 0, 0, 0, 0),
      zone_2 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, NA, 0, 0, 0, 0),
      zone_3 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, NA, 0, 0, 0, 0)
    )
  )
  expect_equal(terra::values(s2_1), terra::values(s2_2))
  expect_inherits(s3_1, "SpatRaster")
  expect_inherits(s3_2, "SpatRaster")
  expect_equal(
    as.data.frame(terra::values(s3_1)),
    data.frame(
      zone_1 = c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0, NA, NA, 0, 0, 0, 0),
      zone_2 = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, NA, NA, 0, 0, 0, 0),
      zone_3 = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, NA, NA, 0, 0, 0, 0)
    )
  )
  expect_equal(terra::values(s3_1), terra::values(s3_2))
})
#
test_that("invalid inputs (multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # make zones matrices
  zm <- matrix(-1, ncol = 3, nrow = 3)
  diag(zm) <- 1
  # make connectivity data
  cm <- as_Matrix(adjacency_matrix(sim_zones_pu_raster), "dgCMatrix")
  cm@x <- cm@x + runif(length(cm@x))
  ca <- array(1, dim = c(dim(cm), 3, 3))
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
    add_binary_decisions()
  # tests
  expect_tidy_error(add_asym_connectivity_penalties(p, NA_real_, zm, cm))
  expect_tidy_error(add_asym_connectivity_penalties(p, Inf, zm ,cm))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, zm[-1, ], cm))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, zm[, -1], cm))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, `[<-`(zm, 1, -2), cm))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, `[<-`(zm, 1, 3), cm))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, `[<-`(zm, 1, NA), cm))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, zm, cm[-1, ]))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, zm, cm[, -1]))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, zm, cm[, -1]))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, zm, ca))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, data = ca))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, NULL, ca[-1, , , ]))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, NULL, ca[, -1, , ]))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, NULL, ca[, , -1, ]))
  expect_tidy_error(add_asym_connectivity_penalties(p, 1, NULL, ca[, , , -1]))
})

test_that("alternative data formats", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create connectivity matrices
  m <- adjacency_matrix(sim_pu_raster)
  m <- as_Matrix(m, "dgTMatrix")
  m@x <- m@x + runif(length(m@x))
  m2 <- data.frame(id1 = m@i + 1, id2 = m@j + 1, boundary = m@x)
  # create problem
  p0 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.45) %>%
    add_binary_decisions()
  p1 <-
    p0 %>%
    add_asym_connectivity_penalties(1000, data = m)
  p2 <-
    p0 %>%
    add_asym_connectivity_penalties(1000, data = as.matrix(m))
  p3 <-
    p0 %>%
    add_asym_connectivity_penalties(1000, data = m2)
  # create objects
  o1 <- as.list(compile(p1))
  o2 <- as.list(compile(p2))
  o3 <- as.list(compile(p3))
  # tests
  expect_equal(o1, o2)
  expect_equal(o1, o3)
})
