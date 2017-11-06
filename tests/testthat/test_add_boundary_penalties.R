context("add_boundary_penalties")

test_that("minimum set objective (compile binary decisions)", {
  ## make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(2, 0.5)
  o <- compile(p)
  ## create variables for debugging
  # number of planning units
  n_pu <- p$number_of_planning_units()
  # number of features
  n_f <- p$number_of_features()
  b_data <- boundary_matrix(p$data$cost)
  b_data <- b_data * 2
  Matrix::diag(b_data) <- Matrix::diag(b_data) * 0.5
  # total boundary for each planning unit
  b_total_boundary <- colSums(b_data)
  class(b_data) <- "dgCMatrix"
  Matrix::diag(b_data) <- 0
  # i,j,x matrix for planning unit boundaries
  b_data <- as(b_data, "dgTMatrix")
  b_data <- Matrix::sparseMatrix(i = b_data@i[b_data@x != 0],
    j = b_data@j[b_data@x != 0], x = b_data@x[b_data@x != 0],
    giveCsparse = FALSE, index1 = FALSE)
  # objectives for boundary decision variables
  b_obj <- o$obj()[n_pu + seq_len(length(b_data@i))]
  # upper bound for boundary decision variables
  # lower bound for boundary decision variables
  b_lb <- o$lb()[n_pu + seq_len(length(b_data@i))]
  b_ub <- o$ub()[n_pu + seq_len(length(b_data@i))]
  # vtype bound for boundary decision variables
  b_vtype <- o$vtype()[n_pu + seq_len(length(b_data@i))]
  # pu costs including total boundary
  pu_costs <- o$obj()[seq_len(n_pu)]
  # matrix labels
  b_col_labels <- o$col_ids()[n_pu + seq_len(length(b_data@i))]
  b_row_labels <- o$row_ids()[n_f + seq_len(length(b_data@i) * 2)]
  # sense for boundary decision constraints
  b_sense <- o$sense()[n_f + seq_len(length(b_data@i) * 2)]
  # rhs for boundary decision constraints
  b_rhs <- o$rhs()[n_f + seq_len(length(b_data@i) * 2)]
  ## check that constraints added correctly
  expect_true(all(b_col_labels == "b"))
  expect_equal(pu_costs, p$planning_unit_costs() + b_total_boundary)
  expect_equal(b_obj, -2 * b_data@x)
  expect_true(all(b_lb == 0))
  expect_true(all(b_ub == 1))
  expect_true(all(b_vtype == "B"))
  expect_equal(b_row_labels, rep(c("b1", "b2"), length(b_data@i)))
  expect_equal(b_sense, rep(c("<=", "<="), length(b_data@i)))
  expect_equal(b_rhs, rep(c(0, 0), length(b_data@i)))
  counter <- n_f
  for (i in seq_along(length(b_data@i))) {
    counter <- counter + 1
    expect_true(o$A()[counter, n_pu + i] == 1)
    expect_true(o$A()[counter, b_data@i[i] + 1] == -1)
    counter <- counter + 1
    expect_true(o$A()[counter, n_pu + i] == 1)
    expect_true(o$A()[counter, b_data@j[i] + 1] == -1)
  }
  # invalid inputs
  expect_error({
    data(sim_pu_raster, sim_features)
    p <- problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_boundary_penalties(-5, 0.5)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_boundary_penalties(9, -0.5)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_boundary_penalties(NA, 0.5)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_boundary_penalties(9, NA)
  })
})

test_that("minimum set objective (solve binary decisions)", {
  skip_on_cran()
  # check that solution is feasible
  data(sim_pu_raster, sim_features)
  s <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(2, 0.5) %>%
    add_default_solver(time_limit = 5) %>%
    solve()
  # tests
  expect_is(s, "RasterLayer")
  expect_true(all(na.omit(unique(raster::values(s))) %in% c(0, 1)))
})

test_that("maximum coverage objective (compile binary decisions)", {
  ## make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = 10000) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(2, 0.5)
  o <- compile(p)
  ## create variables for debugging
  # number of planning units
  n_pu <- p$number_of_planning_units()
  # number of features
  n_f <- p$number_of_features()
  b_data <- boundary_matrix(p$data$cost)
  b_data <- b_data * 2
  Matrix::diag(b_data) <- Matrix::diag(b_data) * 0.5
  # total boundary for each planning unit
  b_total_boundary <- colSums(b_data)
  class(b_data) <- "dgCMatrix"
  Matrix::diag(b_data) <- 0
  # i,j,x matrix for planning unit boundaries
  b_data <- as(b_data, "dgTMatrix")
  b_data <- Matrix::sparseMatrix(i = b_data@i[b_data@x != 0],
                                 j = b_data@j[b_data@x != 0],
                                 x = b_data@x[b_data@x != 0],
                                giveCsparse = FALSE, index1 = FALSE)
  # objectives for boundary decision variables
  b_obj <- o$obj()[n_pu + n_f + seq_len(length(b_data@i))]
  # upper bound for boundary decision variables
  b_lb <- o$lb()[n_pu + n_f + seq_len(length(b_data@i))]
  # lower bound for boundary decision variables
  b_ub <- o$ub()[n_pu + n_f + seq_len(length(b_data@i))]
  # vtype bound for boundary decision variables
  b_vtype <- o$vtype()[n_pu + n_f + seq_len(length(b_data@i))]
  # pu costs including total boundary
  pu_costs <- o$obj()[seq_len(n_pu)]
  # matrix labels
  b_col_labels <- o$col_ids()[n_pu + n_f + seq_len(length(b_data@i))]
  b_row_labels <- o$row_ids()[n_f + 1 + seq_len(length(b_data@i) * 2)]
  # sense for boundary decision constraints
  b_sense <- o$sense()[n_f + 1 + seq_len(length(b_data@i) * 2)]
  # rhs for boundary decision constraints
  b_rhs <- o$rhs()[n_f + 1 + seq_len(length(b_data@i) * 2)]
  ## check that constraints added correctly
  expect_true(all(b_col_labels == "b"))
  expect_equal(pu_costs, 1e-10 - b_total_boundary)
  expect_equal(b_obj, 2 * b_data@x)
  expect_true(all(b_lb == 0))
  expect_true(all(b_ub == 1))
  expect_true(all(b_vtype == "B"))
  expect_equal(b_row_labels, rep(c("b1", "b2"), length(b_data@i)))
  expect_equal(b_sense, rep(c("<=", "<="), length(b_data@i)))
  expect_equal(b_rhs, rep(c(0, 0), length(b_data@i)))
  counter <- n_f + 1
  for (i in seq_along(length(b_data@i))) {
    counter <- counter + 1
    expect_true(o$A()[counter, n_pu + n_f + i] == 1)
    expect_true(o$A()[counter, b_data@i[i] + 1] == -1)
    counter <- counter + 1
    expect_true(o$A()[counter, n_pu + n_f + i] == 1)
    expect_true(o$A()[counter, b_data@j[i] + 1] == -1)
  }
  # invalid inputs
  data(sim_pu_raster, sim_features)
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_max_cover_objective(budget = 10000) %>%
      add_binary_decisions() %>%
      add_boundary_penalties(-5, 0.5)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_max_cover_objective(budget = 10000) %>%
      add_binary_decisions() %>%
      add_boundary_penalties(9, -0.5)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_max_cover_objective(budget = 10000) %>%
      add_binary_decisions() %>%
      add_boundary_penalties(NA, 0.5)
  })
  expect_error({
    p <- problem(sim_pu_raster, sim_features) %>%
      add_max_cover_objective(budget = 10000) %>%
      add_binary_decisions() %>%
      add_boundary_penalties(9, NA)
  })
})

test_that("maximum coverage objective (solve binary decisions)", {
  skip_on_cran()
  # check that solution is feasible
  data(sim_pu_raster, sim_features)
  s <- problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = 10000) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(2, 0.5) %>%
    add_default_solver(time_limit = 5) %>%
    solve()
  # tests
  expect_is(s, "RasterLayer")
  expect_true(all(na.omit(unique(raster::values(s))) %in% c(0, 1)))
})
