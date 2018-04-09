context("add_connected_constraints")

test_that("compile (single zone)", {
  # create problem
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.2) %>%
       add_connected_constraints()
  # compile problem
  o <- compile(p)
  # perform preliminary calculations
  cm <- as(connected_matrix(sim_pu_polygons), "dgCMatrix")
  cm <- Matrix::forceSymmetric(cm, uplo = "L")
  class(cm) <- "dgCMatrix"
  cm <- as(cm, "dgTMatrix")
  n_pu <- length(sim_pu_polygons)
  n_f <- raster::nlayers(sim_features)
  n_edges <- length(cm@i)
  n_j_ends <- length(unique(cm@j))
  con_cols <- n_pu + seq_len(n_edges)
  con_rows <- n_f  +  seq_len(n_edges + n_j_ends + 1)
  # check that obj has been added correctly
  expect_equal(o$obj()[con_cols], rep(0, n_edges))
  # check that col_ids have been added correctly
  expect_equal(o$col_ids()[con_cols], rep("c", n_edges))
  # check that rhs has been added correctly
  expect_equal(o$rhs()[con_rows], c(rep(0, n_edges),  rep(0, n_j_ends), -1))
  # check that sense has been added correctly
  expect_equal(o$sense()[con_rows], c(rep("<=", n_edges), rep("<=", n_j_ends),
                                      "="))
  # check that row ids have been added correctly
  expect_equal(o$row_ids()[con_rows], c(rep("c1", n_edges), rep("c2", n_j_ends),
                                        "c3"))
  ## check that connected constraints have been added correctly
  # c1
  for (i in seq_along(n_edges)) {
    correct_row <- rep(0, n_pu + n_edges)
    correct_row[n_pu + i] <- 1
    correct_row[cm@i[i] + 1] <- -1
    expect_equal(o$A()[n_f + i, ], correct_row)
  }
  # c2
  for (j in seq_along(n_j_ends)) {
    curr_j <- unique(cm@j)[j] + 1
    curr_pu_ij <- which( (cm@j + 1) == curr_j)
    correct_row <- rep(0, n_pu + n_edges)
    correct_row[curr_j] <- -1
    correct_row[n_pu + curr_pu_ij] <- 1
    expect_equal(o$A()[n_f + n_edges + j, ], correct_row)
  }
  # c3
  correct_row <- rep(1, n_pu + n_edges)
  correct_row[seq_len(n_pu)] <- -1
  expect_equal(o$A()[n_f + n_edges + n_j_ends + 1, ], correct_row)
})

test_that("solve (single zone)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_connected_constraints() %>%
       add_default_solver(time_limit = 5)
  # solve problem
  s <- solve(p)
  # check that all selected planning units form a contiguous unit
  agg_s <- aggregate(s)
  expect_equal(length(agg_s@polygons), 1)
  expect_equal(length(agg_s@polygons[[1]]), 1)
})

test_that("invalid inputs (single zone)", {
  # create problem
  data(sim_pu_polygons, sim_features)
  cm <- as.matrix(connected_matrix(sim_pu_polygons))
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1)
  # run tests
  expect_error(add_connected_constraints(p, NA_logical_))
  expect_error(add_connected_constraints(p, TRUE, diag(1) + 1))
  expect_error(add_connected_constraints(p, TRUE, diag(1) - 2))
  expect_error(add_connected_constraints(p, TRUE, diag(1) - NA))
  expect_error(add_connected_constraints(p, TRUE, data = cm[-1, ]))
  expect_error(add_connected_constraints(p, TRUE, data = cm[, -1]))
  expect_error(add_connected_constraints(p, TRUE, data = cm + 1))
  expect_error(add_connected_constraints(p, TRUE, data = cm - 1))
  expect_error(add_connected_constraints(p, TRUE, data = `[<-`(cm, 1, 1, NA)))
})
