context("add_neighbor_constraints")

test_that("compile", {
  # create problem
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
  p <- problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_neighbor_constraints(1)
  # compile problem
  o <- compile(p)
  n_pu <- length(sim_pu_polygons)
  n_f <- raster::nlayers(sim_features)
  neighbor_rows <- n_f + seq_len(n_pu)
  c_matrix <- connected_matrix(sim_pu_polygons)
  # check that rhs has been added correctly
  expect_equal(o$rhs()[neighbor_rows], rep(0, n_pu))
  # check that sense has been added correctly
  expect_equal(o$sense()[neighbor_rows], rep(">=", n_pu))
  # check that row ids have been added correctly
  expect_equal(o$row_ids()[neighbor_rows], rep("n", n_pu))
  # check that neighbor constraints have been added correctly
  for (i in seq_len(n_pu)) {
    correct_row <- replace(rep(0, n_pu), which(c_matrix[i, ] > 1e-10), 1)
    correct_row[i] <- -1
    expect_equal(o$A()[neighbor_rows[i], ], correct_row)
  }
})

test_that("solve", {
  skip_on_cran()
  data(sim_pu_polygons, sim_features)
  # create problem
  p <- problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_neighbor_constraints(3) %>%
    add_default_solver(time_limit = 5)
  # solve problem
  s <- solve(p)
  # check that selected planning units have three neighbors
  n_neighbors <- vapply(rgeos::gIntersects(s[s$solution_1 == 1, ], byid = TRUE,
                                           returnDense = FALSE),
                         length,
                         integer(1))
  expect_true(all(n_neighbors >= 3))
})

test_that("invalid input", {
  expect_error({
    data(sim_pu_polygons, sim_features)
    # create problem
    p <- problem(sim_pu_polygons, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.2) %>%
      add_neighbor_constraints(-3)
  })
  expect_error({
    data(sim_pu_polygons, sim_features)
    # create problem
    p <- problem(sim_pu_polygons, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.2) %>%
      add_neighbor_constraints(NA)
  })
  expect_error({
    # create problem
    path <- system.file("extdata/input.dat", package = "prioritizr")
    p <- marxan_problem(path) %>%
      add_neighbor_constraints(2) %>%
      compile()
  })
})
