context("add_connectivity_penalties")

test_that("minimum set objective (compile binary decisions)", {
  ## make data
  data(sim_pu_raster, sim_features)
  c_matrix <- boundary_matrix(sim_pu_raster)
  class(c_matrix) <- "dgCMatrix"
  p <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_connectivity_penalties(1, c_matrix)
  o <- compile(p)
  ## create variables for debugging
  # number of planning units
  n_pu <- p$number_of_planning_units()
  # number of features
  n_f <- p$number_of_features()
  # total boundary for each planning unit
  c_data <- c_matrix
  total_connections <- Matrix::rowSums(c_data)
  Matrix::diag(c_data) <- 0
  # i,j,x matrix for planning unit boundaries
  c_data <- as(c_data, "dgTMatrix")
  c_data <- Matrix::sparseMatrix(i = c_data@i[c_data@x != 0],
                                j = c_data@j[c_data@x != 0],
                                x = c_data@x[c_data@x != 0],
                                giveCsparse = FALSE, index1 = FALSE,
                                dims = c(n_pu, n_pu))
  # objectives for boundary decision variables
  b_obj <- o$obj()[n_pu + seq_len(length(c_data@i))]
  # upper bound for boundary decision variables
  b_lb <- o$lb()[n_pu + seq_len(length(c_data@i))]
  # lower bound for boundary decision variables
  b_ub <- o$ub()[n_pu + seq_len(length(c_data@i))]
  # vtype bound for boundary decision variables
  b_vtype <- o$vtype()[n_pu + seq_len(length(c_data@i))]
  # pu costs including total boundary
  pu_costs <- o$obj()[seq_len(n_pu)]
  # matrix labels
  b_col_labels <- o$col_ids()[n_pu + seq_len(length(c_data@i))]
  b_row_labels <- o$row_ids()[n_f + seq_len(length(c_data@i) * 2)]
  # sense for boundary decision constraints
  b_sense <- o$sense()[n_f + seq_len(length(c_data@i) * 2)]
  # rhs for boundary decision constraints
  b_rhs <- o$rhs()[n_f + seq_len(length(c_data@i) * 2)]
  ## check that constraints added correctly
  expect_true(all(b_col_labels == "b"))
  expect_equal(pu_costs, p$planning_unit_costs() + total_connections)
  expect_equal(b_obj, -1 * c_data@x)
  expect_true(all(b_lb == 0))
  expect_true(all(b_ub == 1))
  expect_true(all(b_vtype == "B"))
  expect_equal(b_row_labels, rep(c("b1", "b2"), length(c_data@i)))
  expect_equal(b_sense, rep(c("<=", "<="), length(c_data@i)))
  expect_equal(b_rhs, rep(c(0, 0), length(c_data@i)))
  for (pos in seq_along(c_data@i)) {
    # get current planning unit indices
    curr_i <- c_data@i[pos] + 1
    curr_j <- c_data@j[pos] + 1
    # find connections with i and j
    rows_i <- which(o$A()[, curr_i] == -1)
    rows_j <- which(o$A()[, curr_j] == -1)
    # assert that there is a connection between them
    connection_columns_for_i <- vapply(rows_i,
                                       function(r) which(o$A()[r, ] == 1),
                                       integer(1))
    connection_columns_for_j <- vapply(rows_j,
                                       function(r) which(o$A()[r, ] == 1),
                                       integer(1))
    # test that connections exist in matrix
    expect_true(1 == length(intersect(connection_columns_for_i,
                                      connection_columns_for_j)))
  }
  # invalid inputs
  data(sim_pu_raster, sim_features)
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_connectivity_penalties(c_data, NA_real_)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_connectivity_penalties(c_data, -5)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_connectivity_penalties(NA, 0.5)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_connectivity_penalties(c_data[-1, ], NA)
  })
})

test_that("minimum set objective (solve binary decisions)", {
  skip_on_cran()
  # make data
  data(sim_pu_raster, sim_features)
  c_matrix <- boundary_matrix(sim_pu_raster)
  class(c_matrix) <- "dgCMatrix"
  # check that the solution is feasible
  s <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_connectivity_penalties(1, c_matrix) %>%
    add_default_solver(time_limit = 5) %>%
    solve()
  # tests
  expect_is(s, "RasterLayer")
  expect_true(all(na.omit(unique(raster::values(s))) %in% c(0, 1)))
})

test_that("maximum representation objective (compile binary decisions)", {
  ## make data
  data(sim_pu_raster, sim_features)
  c_matrix <- boundary_matrix(sim_pu_raster)
  class(c_matrix) <- "dgCMatrix"
  p <- problem(sim_pu_raster, sim_features) %>%
    add_max_features_objective(budget = 5000) %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_connectivity_penalties(1, c_matrix)
  o <- compile(p)
  ## create variables for debugging
  # number of planning units
  n_pu <- p$number_of_planning_units()
  # number of features
  n_f <- p$number_of_features()
  # total boundary for each planning unit
  c_data <- c_matrix
  total_connections <- Matrix::rowSums(c_data)
  Matrix::diag(c_data) <- 0
  # i,j,x matrix for planning unit boundaries
  c_data <- as(c_data, "dgTMatrix")
  c_data <- Matrix::sparseMatrix(i = c_data@i[c_data@x != 0],
                                 j = c_data@j[c_data@x != 0],
                                 x = c_data@x[c_data@x != 0],
                                 giveCsparse = FALSE, index1 = FALSE,
                                  dims = c(n_pu, n_pu))
  # objectives for boundary decision variables
  b_obj <- o$obj()[n_f + n_pu + seq_len(length(c_data@i))]
  # upper bound for boundary decision variables
  b_lb <- o$lb()[n_pu + seq_len(length(c_data@i))]
  # lower bound for boundary decision variables
  b_ub <- o$ub()[n_pu + seq_len(length(c_data@i))]
  # vtype bound for boundary decision variables
  b_vtype <- o$vtype()[n_pu + seq_len(length(c_data@i))]
  # pu costs including total boundary
  pu_costs <- o$obj()[seq_len(n_pu)]
  # matrix labels
  b_col_labels <- o$col_ids()[n_f + n_pu + seq_len(length(c_data@i))]
  b_row_labels <- o$row_ids()[1 + n_f + seq_len(length(c_data@i) * 2)]
  # sense for boundary decision constraints
  b_sense <- o$sense()[n_f + seq_len(length(c_data@i) * 2)]
  # rhs for boundary decision constraints
  b_rhs <- o$rhs()[1 + n_f + seq_len(length(c_data@i) * 2)]
  ## check that constraints added correctly
  expect_true(all(b_col_labels == "b"))
  expect_equal(pu_costs, 1e-10 - total_connections)
  expect_equal(b_obj, c_data@x)
  expect_true(all(b_lb == 0))
  expect_true(all(b_ub == 1))
  expect_true(all(b_vtype == "B"))
  expect_equal(b_row_labels, rep(c("b1", "b2"), length(c_data@i)))
  expect_equal(b_sense, rep(c("<=", "<="), length(c_data@i)))
  expect_equal(b_rhs, rep(c(0, 0), length(c_data@i)))
  for (pos in seq_along(c_data@i)) {
    # get current planning unit indices
    curr_i <- c_data@i[pos] + 1
    curr_j <- c_data@j[pos] + 1
    # find connections with i and j
    rows_i <- which(o$A()[, curr_i] == -1)
    rows_j <- which(o$A()[, curr_j] == -1)
    # assert that there is a connection between them
    connection_columns_for_i <- vapply(rows_i,
                                       function(r) which(o$A()[r, ] == 1),
                                       integer(1))
    connection_columns_for_j <- vapply(rows_j,
                                       function(r) which(o$A()[r, ] == 1),
                                       integer(1))
    # test that connections exist in matrix
    expect_true(1 == length(intersect(connection_columns_for_i,
                                      connection_columns_for_j)))
  }
  # invalid inputs
  data(sim_pu_raster, sim_features)
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_max_features_objective(budget = 5000) %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_connectivity_penalties(c_data, NA_real_)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_max_features_objective(budget = 5000) %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_connectivity_penalties(c_data, -5)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_max_features_objective(budget = 5000) %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_connectivity_penalties(NA, 0.5)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_max_features_objective(budget = 5000) %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_connectivity_penalties(c_data[-1, ], NA)
  })
})

test_that("maximum representation objective (solve binary decisions)", {
  skip_on_cran()
  ## make data
  data(sim_pu_raster, sim_features)
  c_matrix <- boundary_matrix(sim_pu_raster)
  class(c_matrix) <- "dgCMatrix"
  # check that the solution is feasible
  s <- problem(sim_pu_raster, sim_features) %>%
       add_max_features_objective(budget = 5000) %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_connectivity_penalties(1, c_matrix) %>%
       add_default_solver(time_limit = 5) %>%
       solve()
  # tests
  expect_is(s, "RasterLayer")
  expect_true(all(na.omit(unique(raster::values(s))) %in% c(0, 1)))
})
