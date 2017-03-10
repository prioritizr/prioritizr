context("marxan_problem")

test_that("spatial data input (compile)", {
  # make problems
  p1 <- marxan_problem(sim_pu_polygons, features = sim_features, targets = 0.2,
    targets_type = "relative", penalty = 1, edge_factor = 0.5)
  p2 <- problem(sim_pu_polygons, features = sim_features) %>%
          add_min_set_objective() %>%
          add_relative_targets(0.2) %>%
          add_boundary_penalties(1, 0.5)
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # test that optimization problems are the same
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$modelsense(), o2$modelsense())
  expect_equal(o1$vtype(), o2$vtype())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$row_ids(), o2$row_ids())
  expect_equal(o1$col_ids(), o2$col_ids())
  expect_true(all(o1$A() == o2$A()))
})

test_that("spatial data input (solve)", {
  skip_on_cran()
  # make problem
  data(sim_pu_polygons, sim_features)
  p <- marxan_problem(sim_pu_polygons, features = sim_features, targets = 0.2,
    targets_type = "relative", penalty = 1, edge_factor = 0.5) %>%
      add_default_solver(time_limit = 5)
  # check that problem can be solved
  s <- solve(p)
})

test_that("character filename input (compile symmetric boundary penalties)", {
  ## make problem
  path <- system.file("extdata/input.dat", package = "prioritizr")
  p <- marxan_problem(path)
  ## compile problem
  o <- compile(p)
  ## test optimization problem is correct
  # load data
  wd <- system.file("extdata/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  spec_data <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  puvspr_data <- read.table(file.path(wd, "puvspr.dat"), header = TRUE,
                            sep = ",")
  bound_data <- read.table(file.path(wd, "bound.dat"), header = TRUE,
                           sep = "\t")
  n_pu <- nrow(pu_data)
  n_f <- nrow(spec_data)
  n_edges <- nrow(bound_data)
  bound_data$id1 <- match(bound_data$id1, pu_data$id)
  bound_data$id2 <- match(bound_data$id2, pu_data$id)
  puvspr_data$pu <- match(puvspr_data$pu, pu_data$id)
  puvspr_data$species <- match(puvspr_data$species, spec_data$id)
  pu_data$id <- seq_len(nrow(pu_data))
  spec_data$id <- seq_len(nrow(spec_data))
  # make matrices
  rij_data <- Matrix::sparseMatrix(i = puvspr_data$species, j = puvspr_data$pu,
                                   x = puvspr_data$amount)
  b_data <- triplet_dataframe_to_matrix(bound_data,
    forceSymmetric = TRUE, dims = rep(n_pu, 2))
  # total boundary for each planning unit
  b_total_boundary <- colSums(b_data)
  # remove fixed boundary costs
  b_data <- as(b_data, "dsTMatrix")
  Matrix::diag(b_data) <- 0
  b_data <- Matrix::sparseMatrix(i = b_data@i[b_data@x != 0],
    j = b_data@j[b_data@x != 0], x = b_data@x[b_data@x != 0],
    giveCsparse = FALSE, index1 = FALSE)
  ## extract variables from compiled problem
  # objectives for boundary decision variables
  b_obj <- o$obj()[n_pu + seq_len(length(b_data@i))]
  # upper bound for boundary decision variables
  b_lb <- o$lb()[n_pu + seq_len(length(b_data@i))]
  # lower bound for boundary decision variables
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
  expect_equal(pu_costs, pu_data$cost + b_total_boundary)
  expect_equal(b_obj, -2 * b_data@x)
  expect_true(all(b_lb == 0))
  expect_true(all(b_ub == 1))
  expect_true(all(b_vtype == "B"))
  expect_equal(b_row_labels, rep(c("b1", "b2"), length(b_data@i)))
  expect_equal(b_sense, rep(c("<=", "<="), length(b_data@i)))
  expect_equal(b_rhs, rep(c(0, 0), length(b_data@i)))
  expect_true(all(o$A()[seq_len(n_f), seq_len(n_pu)] ==  rij_data))
  counter <- n_f
  for (i in seq_along(length(b_data@i))) {
    counter <- counter + 1
    expect_true(o$A()[counter, n_pu + i] == 1)
    expect_true(o$A()[counter, b_data@i[i] + 1] == -1)
    counter <- counter + 1
    expect_true(o$A()[counter, n_pu + i] == 1)
    expect_true(o$A()[counter, b_data@j[i] + 1 ] == -1)
  }
})

test_that("character filename input (solve symmetric boundary penalties)", {
  skip_on_cran()
  # make problem
  path <- system.file("extdata/input.dat", package = "prioritizr")
  p <- marxan_problem(path) %>%
    add_default_solver(time_limit = 5)
  # check that problem can be solved
  s <- solve(p)
})

test_that("data.frame input (compile asymmetric boundary penalties)", {
  ## make problem
  # load data
  path <- system.file("extdata/input.dat", package = "prioritizr")
  wd <- system.file("extdata/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  spec_data <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  puvspr_data <- read.table(file.path(wd, "puvspr.dat"), header = TRUE,
                            sep = ",")
  bound_data <- read.table(file.path(wd, "bound.dat"), header = TRUE,
                           sep = "\t")
  p <- marxan_problem(pu_data, spec_data, puvspr_data, bound_data,
                      asymmetric_connectivity = TRUE, blm = 1)
  ## compile problem
  o <- compile(p)
  ## test optimization problem is correct
  n_pu <- nrow(pu_data)
  n_f <- nrow(spec_data)
  n_edges <- nrow(bound_data)
  bound_data$id1 <- match(bound_data$id1, pu_data$id)
  bound_data$id2 <- match(bound_data$id2, pu_data$id)
  puvspr_data$pu <- match(puvspr_data$pu, pu_data$id)
  puvspr_data$species <- match(puvspr_data$species, spec_data$id)
  pu_data$id <- seq_len(nrow(pu_data))
  spec_data$id <- seq_len(nrow(spec_data))
  # make matrices
  rij_data <- Matrix::sparseMatrix(i = puvspr_data$species, j = puvspr_data$pu,
                                   x = puvspr_data$amount)
  c_data <- triplet_dataframe_to_matrix(bound_data, forceSymmetric = FALSE,
                                          dims = rep(n_pu, 2))
  # total boundary for each planning unit
  total_connections <- Matrix::rowSums(c_data)
  # remove fixed boundary costs
  Matrix::diag(c_data) <- 0
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
  # test that problem matrix is correctly specified
  a <- o$A()
  a <- a[-1 * seq_len(n_f), ] # extract connection constraints
  a <- as.matrix(a) # convert to regular matrix
  pu_pos <- which(a == -1, arr.ind = TRUE)
  pu_ij_pos <- which(a == 1, arr.ind = TRUE)
  pu_i <- c_data@i + 1 # add one to convert to base-1 indexing
  pu_j <- c_data@j + 1
  # test that they are expect_equal
  for (pos in seq_along(c_data@i)) {
    rows_i <- pu_pos[, 1] == pu_i[pos]
    rows_j <- pu_pos[, 1] == pu_j[pos]
    columns_ij_i <- pu_ij_pos[, 1] %in% rows_i
    columns_ij_j <- pu_ij_pos[, 1] %in% rows_j
    expect_equal(sum(columns_ij_i & columns_ij_j), 1)
  }
})

test_that("data.frame input (solve symmetric boundary penalties)", {
  skip_on_cran()
  # make problem
  path <- system.file("extdata/input.dat", package = "prioritizr")
  wd <- system.file("extdata/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  spec_data <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  puvspr_data <- read.table(file.path(wd, "puvspr.dat"), header = TRUE,
                            sep = ",")
  bound_data <- read.table(file.path(wd, "bound.dat"), header = TRUE,
                           sep = "\t")
  p <- marxan_problem(pu_data, spec_data, puvspr_data, bound_data,
                      asymmetric_connectivity = TRUE, blm = 1) %>%
    add_default_solver(time_limit = 5)
  # check that problem can be solved
  s <- solve(p)
})
