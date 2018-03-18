context("add_connectivity_penalties")

test_that("minimum set objective (compile, symmetric, single zone)", {
  # make and compile problems
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  p1 <- p %>% add_boundary_penalties(1, 1)
  p2 <- p %>% add_connectivity_penalties(1, boundary_matrix(sim_pu_raster))
  o1 <- compile(p1)
  o2 <- compile(p2)
  # run tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$A(), o2$A())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(gsub("b", "c", o1$col_ids(), fixed = TRUE), o2$col_ids())
  expect_equal(gsub("b", "c", o1$row_ids(), fixed = TRUE), o2$row_ids())
})

test_that("minimum set objective (solve, symmetric, zone)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # load data
  data(sim_pu_raster, sim_features)
  # create and solve problem
  s <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_connectivity_penalties(1, boundary_matrix(sim_pu_raster)) %>%
       add_default_solver(time_limit = 5) %>%
       solve()
  # tests
  expect_is(s, "RasterLayer")
  expect_true(all(na.omit(unique(raster::values(s))) %in% c(0, 1)))
})

test_that("minimum set objective (compile, asymmetric, single zone)", {
  # load data
  data(sim_pu_polygons, sim_features)
  # prepare asymetric connectivity matrix
  c_matrix <- as.matrix(boundary_matrix(sim_pu_polygons))
  c_matrix[lower.tri(c_matrix)] <- c_matrix[lower.tri(c_matrix)] * 0.25
  # make problem
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_connectivity_penalties(1, c_matrix) %>%
       add_binary_decisions()
  # compile problem
  o <- compile(p)
  ## create variables for debugging
  # number of planning units
  n_pu <- p$number_of_planning_units()
  # number of features
  n_f <- p$number_of_features()
  # planning unit indices
  pu_indices <- p$planning_unit_indices()
  # compute total penalties
  total_penalties <- Matrix::rowSums(c_matrix)
  # compute penalties for connection variables
  c_matrix2 <- c_matrix
  upper_ind <- which(upper.tri(c_matrix2), arr.ind = TRUE)
  c_matrix2[upper.tri(c_matrix2)] <- c_matrix2[upper.tri(c_matrix2)] +
                                     c_matrix2[upper_ind[, c(2, 1)]]
  c_matrix2[lower.tri(c_matrix2, diag = TRUE)] <- 0
  c_matrix2 <- methods::as(c_matrix2, "dgCMatrix")
  con_var_penalties <- c_matrix2@x
  n_con_vars <- length(con_var_penalties)
  correct_obj <- unname(c(p$planning_unit_costs()[, 1] + total_penalties,
                          con_var_penalties))
  ## extract data from compiled problem
  # lower bound for boundary decision variables
  c_lb <- o$lb()[n_pu + seq_len(n_con_vars)]
  # upper bound for boundary decision variables
  c_ub <- o$ub()[n_pu + seq_len(n_con_vars)]
  # vtype bound for boundary decision variables
  c_vtype <- o$vtype()[n_pu + seq_len(n_con_vars)]
  # matrix labels
  c_col_labels <- o$col_ids()[n_pu + seq_len(n_con_vars)]
  c_row_labels <- o$row_ids()[n_f + seq_len(n_con_vars * 2)]
  # sense for boundary decision constraints
  c_sense <- o$sense()[n_f + seq_len(n_con_vars * 2)]
  # rhs for boundary decision constraints
  c_rhs <- o$rhs()[n_f + seq_len(n_con_vars * 2)]
  ## tests
  expect_equal(o$obj(), correct_obj)
  expect_equal(c_lb, rep(0, n_con_vars))
  expect_equal(c_ub, rep(1, n_con_vars))
  expect_equal(c_vtype, rep("B", n_con_vars))
  expect_equal(c_sense, rep("<=", n_con_vars * 2))
  expect_equal(c_rhs, rep(0, n_con_vars * 2))
  expect_equal(c_col_labels, rep("ac", n_con_vars))
  expect_equal(c_row_labels, rep(c("ac1", "ac2"), n_con_vars))
  # test constraint matrix
  c_matrix3 <- c_matrix
  Matrix::diag(c_matrix3) <- 0
  c_matrix3 <- as(c_matrix3, "dgTMatrix")
  previous_rows <- c()
  for (i in seq_along(length(c_matrix3@i))) {
    # get current planning unit/zone indices
    curr_i <- c_matrix3@i[i] + 1
    curr_j <- c_matrix3@j[i] + 1
    # find connections with i and j
    rows_i <- which(o$A()[, curr_i] == -1)
    rows_j <- which(o$A()[, curr_j] == -1)
    # assert that there is a connection between them
    con_cols_i <- max.col(o$A()[rows_i, , drop = FALSE] == 1)
    con_cols_j <- max.col(o$A()[rows_j, , drop = FALSE] == 1)
    curr_row <- intersect(con_cols_i, con_cols_j)
    expect_equal(length(curr_row), 1)
    expect_true(!curr_row %in% previous_rows)
    previous_rows <- c(previous_rows, curr_row)
  }
})

test_that("minimum set objective (solve, asymmetric, single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  # prepare asymetric connectivity matrix
  c_matrix <- as.matrix(boundary_matrix(sim_pu_raster))
  c_matrix[lower.tri(c_matrix)] <- c_matrix[lower.tri(c_matrix)] * 0.01
  # make and solve problem
  s <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_connectivity_penalties(100, c_matrix) %>%
       add_binary_decisions() %>%
       solve()
  # check solution
  expect_is(s, "RasterLayer")
  expect_true(all(raster::values(s) %in% c(0, 1, NA)))
})

test_that("invalid inputs (single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  c_matrix <- boundary_matrix(sim_pu_raster)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_error(add_connectivity_penalties(p, c_data, NA_real_))
  expect_error(add_connectivity_penalties(p, NA_real_, 0.5))
  expect_error(add_connectivity_penalties(p, c_data[-1, ], NA_real_))
})

test_that("minimum set objective (compile, symmetric, multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  sim_pu_zones_polygons <- sim_pu_zones_polygons[1:3, ]
  # prepare connectivity matrix that is equivalent to a boundary matrix
  b_penalties <- matrix(0, ncol = 3, nrow = 3)
  diag(b_penalties) <- 10 + seq_len(3)
  b_penalties[upper.tri(b_penalties)] <- seq_len(3)
  b_penalties[lower.tri(b_penalties)] <- b_penalties[upper.tri(b_penalties)]
  b_matrix <- boundary_matrix(sim_pu_zones_polygons)
  c_matrix <- array(0, dim = c(rep(length(sim_pu_zones_polygons), 2),
                               rep(3, 2)))
  for (z1 in seq_len(3)) {
    for (z2 in seq_len(3)) {
      c_matrix[, , z1, z2] <- as.matrix(b_penalties[z1, z2] * b_matrix)
      if (z1 != z2) {
        diag(c_matrix[, , z1, z2]) <- 0
      }
    }
  }
  # make and compile problems
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
       add_binary_decisions()
  p1 <- p %>% add_boundary_penalties(b_penalties, rep(1, 3))
  p2 <- p %>% add_connectivity_penalties(1, c_matrix)
  o1 <- compile(p1)
  o2 <- compile(p2)
  # run tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$A(), o2$A())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(gsub("b", "c", o1$col_ids(), fixed = TRUE), o2$col_ids())
  expect_equal(gsub("b", "c", o1$row_ids(), fixed = TRUE), o2$row_ids())
})

test_that("minimum set objective (solve, symmetric, multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  # prepare connectivity matrix that is equivalent to a boundary matrix
  b_penalties <- matrix(0, ncol = 3, nrow = 3)
  diag(b_penalties) <- 10 + seq_len(3)
  b_penalties[upper.tri(b_penalties)] <- seq_len(3)
  b_penalties[lower.tri(b_penalties)] <- b_penalties[upper.tri(b_penalties)]
  b_matrix <- boundary_matrix(sim_pu_zones_polygons)
  c_matrix <- array(0, dim = c(rep(length(sim_pu_zones_polygons), 2),
                               rep(3, 2)))
  for (z1 in seq_len(3)) {
    for (z2 in seq_len(3)) {
      c_matrix[, , z1, z2] <- as.matrix(b_penalties[z1, z2] * b_matrix)
      if (z1 != z2) {
        diag(c_matrix[, , z1, z2]) <- 0
      }
    }
  }
  # create and solve problem
  s <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
       add_binary_decisions() %>%
       add_connectivity_penalties(1, c_matrix) %>%
       solve()
  # tests
  expect_is(s, "SpatialPolygonsDataFrame")
  expect_true(all(sim_pu_zones_polygons$solution_1_zone_1 %in% c(0, 1)))
  expect_true(all(sim_pu_zones_polygons$solution_1_zone_2 %in% c(0, 1)))
  expect_true(all(sim_pu_zones_polygons$solution_1_zone_3 %in% c(0, 1)))
})

test_that("minimum set objective (compile, asymmetric, multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  sim_pu_zones_polygons <- sim_pu_zones_polygons[1:10, ]
  # prepare asymetric connectivity matrix
  b_matrix <- as.matrix(boundary_matrix(sim_pu_zones_polygons))
  b_matrix[lower.tri(b_matrix)] <- b_matrix[lower.tri(b_matrix)] * 0.25
  z_penalties <- matrix(seq_len(9), ncol = 3, nrow = 3)
  c_matrix <- array(0, dim = c(rep(length(sim_pu_zones_polygons), 2),
                               rep(3, 2)))
  for (z1 in seq_len(3)) {
    for (z2 in seq_len(3)) {
      c_matrix[, , z1, z2] <- as.matrix(z_penalties[z1, z2] * b_matrix)
      if (z1 != z2) {
        diag(c_matrix[, , z1, z2]) <- 0
      }
    }
  }
  # make problem
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
       add_connectivity_penalties(1, c_matrix) %>%
       add_binary_decisions()
  # compile problem
  o <- compile(p)
  ## create variables for debugging
  # number of planning units
  n_pu <- p$number_of_planning_units()
  # number of features
  n_f <- p$number_of_features()
  # number of zones
  n_z <- p$number_of_zones()
  # planning unit indices
  pu_indices <- p$planning_unit_indices()
  # compute total penalties
  total_penalties <- numeric(n_pu * n_z)
  for (z1 in seq_len(n_z)) {
    for (z2 in seq_len(n_z)) {
      pos <- ((z1 - 1) * n_pu) + seq_len(n_pu)
      total_penalties[pos] <- total_penalties[pos] +
                              rowSums(c_matrix[, , z1, z2])
    }
  }
  # compute penalties for connection variables
  c_matrix2 <- c_matrix
  c_matrix2[] <- 0
  pu_var_1 <- c()
  pu_var_2 <- c()
  for (i in seq_len(n_pu)) {
    for (j in seq(i, n_pu)) {
      for (z1 in seq_len(n_z)) {
        for (z2 in seq_len(n_z)) {
          if (i != j) {
            c_matrix2[i, j, z1, z2] <- c_matrix[i, j, z1, z2] +
                                       c_matrix[j, i, z2, z1]
          }
          if (c_matrix2[i, j, z1, z2] > 0) {
            pu_var_1 <- c(pu_var_1, ((z1 - 1) * n_pu) + i)
            pu_var_2 <- c(pu_var_2, ((z2 - 1) * n_pu) + j)
          }
        }
      }
    }
  }
  n_con_vars <- sum(c_matrix2 != 0)
  con_var_penalties <- c_matrix2[c_matrix2 != 0]
  correct_obj <- unname(c(p$planning_unit_costs() + total_penalties,
                          con_var_penalties))
  ## extract data from compiled problem
  # lower bound for boundary decision variables
  c_lb <- o$lb()[(n_pu * n_z) + seq_len(n_con_vars)]
  # upper bound for boundary decision variables
  c_ub <- o$ub()[(n_pu * n_z) + seq_len(n_con_vars)]
  # vtype bound for boundary decision variables
  c_vtype <- o$vtype()[(n_pu * n_z) + seq_len(n_con_vars)]
  # matrix labels
  c_col_labels <- o$col_ids()[(n_pu * n_z) + seq_len(n_con_vars)]
  c_row_labels <- o$row_ids()[(n_f * n_z) + n_pu + seq_len(n_con_vars * 2)]
  # sense for boundary decision constraints
  c_sense <- o$sense()[(n_f * n_z) + n_pu + seq_len(n_con_vars * 2)]
  # rhs for boundary decision constraints
  c_rhs <- o$rhs()[(n_f * n_z) + n_pu + seq_len(n_con_vars * 2)]
  ## tests
  expect_equal(sort(o$obj()), sort(correct_obj))
  expect_equal(c_lb, rep(0, n_con_vars))
  expect_equal(c_ub, rep(1, n_con_vars))
  expect_equal(c_vtype, rep("B", n_con_vars))
  expect_equal(c_sense, rep("<=", n_con_vars * 2))
  expect_equal(c_rhs, rep(0, n_con_vars * 2))
  expect_equal(c_col_labels, rep("ac", n_con_vars))
  expect_equal(c_row_labels, rep(c("ac1", "ac2"), n_con_vars))
  # test constraint matrix
  previous_rows <- c()
  for (i in seq_along(pu_var_1)) {
    # get current planning unit/zone indices
    curr_i <- pu_var_1[i]
    curr_j <- pu_var_2[i]
    # find connections with i and j
    rows_i <- which(o$A()[, curr_i] == -1)
    rows_j <- which(o$A()[, curr_j] == -1)
    # assert that there is a connection between them
    con_cols_i <- max.col(o$A()[rows_i, , drop = FALSE] == 1)
    con_cols_j <- max.col(o$A()[rows_j, , drop = FALSE] == 1)
    curr_row <- intersect(con_cols_i, con_cols_j)
    expect_equal(length(curr_row), 1)
    expect_true(!curr_row %in% previous_rows)
    previous_rows <- c(previous_rows, curr_row)
  }
})

test_that("minimum set objective (solve, asymmetric, multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  sim_pu_zones_polygons <- sim_pu_zones_polygons[1:10, ]
  # prepare asymetric connectivity matrix
  b_matrix <- as.matrix(boundary_matrix(sim_pu_zones_polygons))
  b_matrix[lower.tri(b_matrix)] <- b_matrix[lower.tri(b_matrix)] * 0.25
  z_penalties <- matrix(seq_len(9), ncol = 3, nrow = 3)
  c_matrix <- array(0, dim = c(rep(length(sim_pu_zones_polygons), 2),
                               rep(3, 2)))
  for (z1 in seq_len(3)) {
    for (z2 in seq_len(3)) {
      c_matrix[, , z1, z2] <- as.matrix(z_penalties[z1, z2] * b_matrix)
      if (z1 != z2) {
        diag(c_matrix[, , z1, z2]) <- 0
      }
    }
  }
  # make and solve problem
  s <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
       add_connectivity_penalties(1, c_matrix) %>%
       add_binary_decisions() %>%
       solve()
  # check outputs
  expect_is(s, "SpatialPolygonsDataFrame")
  expect_true("solution_1_zone_1" %in% names(s))
  expect_true("solution_1_zone_2" %in% names(s))
  expect_true("solution_1_zone_3" %in% names(s))
  expect_true(all(s$solution_1_zone_1 %in% c(0, 1, NA)))
  expect_true(all(s$solution_1_zone_2 %in% c(0, 1, NA)))
  expect_true(all(s$solution_1_zone_3 %in% c(0, 1, NA)))
})

test_that("invalid inputs (multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  # prepare asymetric connectivity matrix
  b_matrix <- as.matrix(boundary_matrix(sim_pu_zones_polygons))
  b_matrix[lower.tri(b_matrix)] <- b_matrix[lower.tri(b_matrix)] * 0.25
  z_penalties <- matrix(seq_len(9), ncol = 3, nrow = 3)
  c_matrix <- array(0, dim = c(rep(length(sim_pu_zones_polygons), 2),
                               rep(3, 2)))
  for (z1 in seq_len(3)) {
    for (z2 in seq_len(3)) {
      c_matrix[, , z1, z2] <- as.matrix(z_penalties[z1, z2] * b_matrix)
      if (z1 != z2) {
        diag(c_matrix[, , z1, z2]) <- 0
      }
    }
  }
  # make minimal problem
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
       add_binary_decisions()
  # tests
  expect_error(add_connectivity_penalties(p, NA_real_, c_matrix))
  expect_error(add_connectivity_penalties(p, 1, b_matrix))
  expect_error(add_connectivity_penalties(p, 1, c_matrix[-1, , , ]))
  expect_error(add_connectivity_penalties(p, 1, c_matrix[, -1, , ]))
  expect_error(add_connectivity_penalties(p, 1, c_matrix[, , -1, ]))
  expect_error(add_connectivity_penalties(p, 1, c_matrix[, , , -1]))
  expect_error(add_connectivity_penalties(p, 1, c_matrix[, , , 1]))
})
