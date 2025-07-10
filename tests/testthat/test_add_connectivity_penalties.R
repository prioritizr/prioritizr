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
    add_connectivity_penalties(5, data = boundary_matrix(sim_pu_raster))
  # compile problem
  o <- compile(p)
  # create variables for testing
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  pu_indices <- p$planning_unit_indices()
  c_data <- boundary_matrix(p$data$cost)[pu_indices, pu_indices]
  c_data <- c_data * -5
  # connectivity weights for each planning unit
  c_weights <- Matrix::diag(c_data)
  # i,j,x matrix for planning unit boundaries
  Matrix::diag(c_data) <- 0
  c_data <- Matrix::drop0(c_data)
  c_data <- as_Matrix(Matrix::tril(c_data), "dgTMatrix")
  # objectives for boundary decision variables
  c_obj <- o$obj()[n_pu + seq_len(length(c_data@i))]
  # lower bound for boundary decision variables
  c_lb <- o$lb()[n_pu + seq_len(length(c_data@i))]
  # upper bound for boundary decision variables
  c_ub <- o$ub()[n_pu + seq_len(length(c_data@i))]
  # vtype bound for boundary decision variables
  c_vtype <- o$vtype()[n_pu + seq_len(length(c_data@i))]
  # pu costs including total boundary
  pu_costs <- o$obj()[seq_len(n_pu)]
  # matrix labels
  c_col_labels <- o$col_ids()[n_pu + seq_len(length(c_data@i))]
  c_row_labels <- o$row_ids()[n_f + seq_len(length(c_data@i) * 2)]
  # sense for boundary decision constraints
  c_sense <- o$sense()[n_f + seq_len(length(c_data@i) * 2)]
  # rhs for boundary decision constraints
  c_rhs <- o$rhs()[n_f + seq_len(length(c_data@i) * 2)]
  # tests
  expect_equal(pu_costs, p$planning_unit_costs()[, 1] + c_weights)
  expect_equal(c_obj, c_data@x)
  expect_equal(c_lb, rep(0, length(c_data@i)))
  expect_equal(c_ub, rep(1, length(c_data@i)))
  expect_equal(c_vtype, rep("C", length(c_data@i)))
  expect_equal(c_col_labels, rep("c", length(c_data@i)))
  expect_equal(c_row_labels, rep(c("c1", "c2"), length(c_data@i)))
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

test_that("minimum set objective (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create and solve problem
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.45) %>%
    add_connectivity_penalties(1000, data = adjacency_matrix(sim_pu_raster)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0.01, verbose = FALSE)
  s1_1 <- solve(p1)
  s1_2 <- solve(p1)
  p2 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.45) %>%
    add_connectivity_penalties(
      -1000, data = adjacency_matrix(sim_pu_raster)
    ) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0.01, verbose = FALSE)
  s2_1 <- solve(p2)
  s2_2 <- solve(p2)
  # tests
  expect_inherits(s1_1, "SpatRaster")
  expect_inherits(s1_2, "SpatRaster")
  expect_true(all_binary(terra::values(s1_1)))
  expect_true(is_single_patch_raster(s1_1))
  expect_equal(terra::values(s1_1), terra::values(s1_2))
  expect_inherits(s2_1, "SpatRaster")
  expect_inherits(s2_2, "SpatRaster")
  expect_true(all_binary(s2_1))
  expect_true(is_checkerboard_raster(s2_1))
  expect_equal(terra::values(s2_1), terra::values(s2_2))
})

test_that("invalid inputs (single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  c_matrix <- boundary_matrix(sim_pu_raster)
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # tests
  expect_tidy_error(add_connectivity_penalties(p, NA_real_, data = c_matrix))
  expect_tidy_error(add_connectivity_penalties(p, 1, 0, data = c_matrix))
  expect_tidy_error(add_connectivity_penalties(p, 5, data = c_matrix[, -1]))
  expect_tidy_error(add_connectivity_penalties(p, 5, data = c_matrix[-1, ]))
  ac_data <- matrix(
    runif(terra::ncell(sim_pu_raster) ^ 2),
    ncol = terra::ncell(sim_pu_raster)
  )
  expect_tidy_error(add_connectivity_penalties(p, 5, data = ac_data))
})

test_that("minimum set objective (compile, multiple zones)", {
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  sim_zones_pu_polygons <- sim_zones_pu_polygons[seq_len(20), ]
  # prepare data for problem
  cm <- boundary_matrix(sim_zones_pu_polygons)
  zm <- matrix(seq_len(9) * 0.1, ncol = 3)
  # create and compile problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
    add_connectivity_penalties(100, zm, cm) %>%
    add_binary_decisions()
  o <- compile(p)
  # prepare data for tests
  ## calculate constants
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  ## prepare matrix
  c_data <- cm * -100
  c_data <- as_Matrix(Matrix::tril(c_data), "dgTMatrix")
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
  ## objectives for connectivity decision variables
  c_obj <- o$obj()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  ## lower bound for connectivity decision variables
  c_lb <- o$lb()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  ## upper bound for connectivity decision variables
  c_ub <- o$ub()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  ## vtype bound for connectivity decision variables
  c_vtype <- o$vtype()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  ## pu costs including connectivity penalties
  pu_costs <- o$obj()[seq_len(n_pu * n_z)]
  ## matrix labels
  c_col_labels <-
    o$col_ids()[(n_pu * n_z) + seq_len(length(c_data@i)  * n_z * n_z)]
  c_row_labels <-
    o$row_ids()[(n_f * n_z) + n_pu + seq_len(length(c_data@i) * 2)]
  ## sense for connectivity decision constraints
  c_sense <-
    o$sense()[(n_f * n_z) + n_pu + seq_len(length(c_data@i) * 2)]
  ## rhs for connectivity decision constraints
  c_rhs <-
    o$rhs()[(n_f * n_z) + n_pu + seq_len(length(c_data@i) * 2)]
  # tests
  expect_equal(pu_costs, c(p$planning_unit_costs()) + c_weights)
  expect_equal(c_obj, c_penalties)
  expect_equal(c_lb, rep(0, length(c_data@i) * n_z * n_z))
  expect_equal(c_ub, rep(1, length(c_data@i) * n_z * n_z))
  expect_equal(c_vtype, rep("C", length(c_data@i) * n_z * n_z))
  expect_equal(c_col_labels, rep("c", length(c_data@i) * n_z * n_z))
  expect_equal(c_row_labels, rep(c("c1", "c2"), length(c_data@i)))
  expect_equal(c_sense, rep(c("<=", "<="), length(c_data@i)))
  expect_equal(c_rhs, rep(c(0, 0), length(c_data@i)))
  c_A <- o$A()[
    seq(
      (n_f * n_z) + n_pu + 1,
      (n_f * n_z) + n_pu + (n_z * n_z * (2 * length(c_data@i)))
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
  cm <- boundary_matrix(sim_zones_pu_polygons)
  zm <- matrix(seq_len(9) * 0.1, ncol = 3)
  ca <- array(0, dim = c(dim(cm), 3, 3))
  for (i in seq_len(3))
    for (j in seq_len(3))
      ca[, , i, j] <- as.matrix(cm * zm[i, j])
  # create and compile problems
  p1 <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
    add_connectivity_penalties(100, zm, cm) %>%
    add_binary_decisions()
  p2 <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
    add_connectivity_penalties(100, NULL, ca) %>%
    add_binary_decisions()
  o1 <- compile(p1)
  o2 <- compile(p2)
  # tests
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
  # make zones matrices
  zm <- matrix(-1, ncol = 3, nrow = 3)
  diag(zm) <- 0.1
  # make connectivity data
  cm <- adjacency_matrix(sim_zones_pu_raster)
  # create and solve problem
  expect_warning(
    s <-
      problem(sim_zones_pu_raster * 0, sim_zones_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
      add_connectivity_penalties(1, zm, cm) %>%
      add_binary_decisions() %>%
      add_default_solver(gap = 0.1, verbose = FALSE) %>%
      solve(),
    "zero"
  )
  sc <- category_layer(s)
  # tests
  expect_inherits(s, "SpatRaster")
  expect_true(all_binary(s))
  expect_true(is_single_patch_raster(s[[1]]))
  expect_true(is_single_patch_raster(s[[2]]))
  expect_true(is_single_patch_raster(s[[3]]))
  expect_true(is_distinct_zones_raster(category_layer(s)))
})

test_that("invalid inputs (multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # make zones matrices
  zm <- matrix(-1, ncol = 3, nrow = 3)
  diag(zm) <- 1
  # make connectivity data
  cm <- adjacency_matrix(sim_zones_pu_raster)
  ca <- array(1, dim = c(dim(cm), 3, 3))
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
    add_binary_decisions()
  # tests
  expect_tidy_error(add_connectivity_penalties(p, NA_real_, zm, cm))
  expect_tidy_error(add_connectivity_penalties(p, Inf, zm,cm))
  expect_tidy_error(add_connectivity_penalties(p, 1, zm[-1, ], cm))
  expect_tidy_error(add_connectivity_penalties(p, 1, zm[, -1], cm))
  expect_tidy_error(add_connectivity_penalties(p, 1, `[<-`(zm, 1, -2), cm))
  expect_tidy_error(add_connectivity_penalties(p, 1, `[<-`(zm, 1, 3), cm))
  expect_tidy_error(add_connectivity_penalties(p, 1, `[<-`(zm, 1, NA), cm))
  expect_tidy_error(add_connectivity_penalties(p, 1, zm, cm[-1, ]))
  expect_tidy_error(add_connectivity_penalties(p, 1, zm, cm[, -1]))
  expect_tidy_error(add_connectivity_penalties(p, 1, zm, cm[, -1]))
  expect_tidy_error(add_connectivity_penalties(p, 1, zm, ca))
  expect_tidy_error(add_connectivity_penalties(p, 1, data = ca))
  expect_tidy_error(add_connectivity_penalties(p, 1, NULL, ca[-1, , , ]))
  expect_tidy_error(add_connectivity_penalties(p, 1, NULL, ca[, -1, , ]))
  expect_tidy_error(add_connectivity_penalties(p, 1, NULL, ca[, , -1, ]))
  expect_tidy_error(add_connectivity_penalties(p, 1, NULL, ca[, , , -1]))
})

test_that("alternative data formats", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create connectivity matrices
  m <- adjacency_matrix(sim_pu_raster)
  m2 <- as_Matrix(m, "dgTMatrix")
  m2 <- data.frame(id1 = m2@i + 1, id2 = m2@j + 1, boundary = m2@x)
  # create problem
  p0 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.45) %>%
    add_binary_decisions()
  p1 <- add_connectivity_penalties(p0, 1000, data = m)
  p2 <- add_connectivity_penalties(p0, 1000, data = as.matrix(m))
  p3 <- add_connectivity_penalties(p0, 1000, data = m2)
  # create objects
  o1 <- as.list(compile(p1))
  o2 <- as.list(compile(p2))
  o3 <- as.list(compile(p3))
  # tests
  expect_equal(o1, o2)
  expect_equal(o1, o3)
})
