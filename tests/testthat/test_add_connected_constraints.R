context("add_connected_constraints")

test_that("compile", {
  # create problem
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
  p <- problem(sim_pu_polygons, sim_features) %>%
   add_min_set_objective() %>%
   add_relative_targets(0.2) %>%
   add_connected_constraints()
  # compile problem
  o <- compile(p)
  # perform preliminary calculations
  c_matrix <- connected_matrix(sim_pu_polygons)
  class(c_matrix) <- "dgCMatrix"
  c_matrix <- as(c_matrix, "dgTMatrix")
  n_pu <- length(sim_pu_polygons)
  n_f <- raster::nlayers(sim_features)
  n_edges <- length(c_matrix@i)
  n_j_ends <- length(unique(c_matrix@j))
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
    correct_row[c_matrix@i[i] + 1] <- -1
    expect_equal(o$A()[n_f + i, ], correct_row)
  }
  # c2
  for (j in seq_along(n_j_ends)) {
    curr_j <- unique(c_matrix@j)[j] + 1
    curr_pu_ij <- which( (c_matrix@j + 1) == curr_j)
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

test_that("solve", {
  skip_on_cran()
  data(sim_pu_polygons, sim_features)
  # create problem
  p <- problem(sim_pu_polygons, sim_features) %>%
   add_min_set_objective() %>%
   add_relative_targets(0.1) %>%
   add_connected_constraints() %>%
   add_default_solver(time_limit = 5)
  # solve problem
  s <- solve(p)
  # check that all selected planning units at least one neighbor
  n_neighbors <- vapply(rgeos::gIntersects(s[s$solution_1 == 1, ], byid = TRUE,
                                           returnDense = FALSE),
                        length,
                        integer(1))
  expect_true(all(n_neighbors >= 1))
})
