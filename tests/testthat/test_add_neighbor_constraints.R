context("add_neighbor_constraints")
skip("not implemented")

test_that("compile (single zone)", {
  # create problem
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
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

test_that("compile (single zone, data)", {
  # create problem
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.2)
  # compile problems
  o1 <- p %>% add_neighbor_constraints(1) %>% compile()
  o2 <- p %>% add_neighbor_constraints(1, connected_matrix(sim_pu_polygons)) %>%
    compile()
  o3 <- p %>% add_neighbor_constraints(1, sim_pu_polygons %>%
    connected_matrix() %>% as.matrix()) %>% compile()
  o4 <- p %>% add_neighbor_constraints(1, sim_pu_polygons %>%
    connected_matrix() %>% matrix_to_triplet_dataframe() %>%
    setNames(c("id1", "id2", "boundary"))) %>% compile()
  o5 <- p %>% add_neighbor_constraints(1,
    sim_pu_polygons %>% connected_matrix() %>% as.matrix() %>% c() %>%
    array(dim = c(rep(length(sim_pu_polygons), 2), 1, 1))) %>% compile()
  # compare problems
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$obj(), o3$obj())
  expect_equal(o1$obj(), o4$obj())
  expect_equal(o1$obj(), o5$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_true(all(o1$A() == o3$A()))
  expect_true(all(o1$A() == o4$A()))
  expect_true(all(o5$A() == o5$A()))
})

test_that("solve (single zone)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_neighbor_constraints(3) %>%
       add_default_solver(time_limit = 5)
  # solve problem
  s <- solve(p)
  # check that selected planning units have three neighbors
  n_neighbors <- vapply(rgeos::gIntersects(s[s$solution_1 == 1, ], byid = TRUE,
                                           returnDense = FALSE),
                         length, integer(1))
  expect_true(all(n_neighbors >= 3))
})

test_that("invalid input (single zone)", {
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

test_that("compile (multiple zones)", {
  # load data
  data(sim_pu_zones_stack, sim_features_zones)
  # create array with neighboring data
  c_matrix <- connected_matrix(sim_pu_zones_stack)
  c_array <- array(0, dim = c(nrow(c_matrix), ncol(c_matrix), 3, 3))
  for (z in seq_len(3)) c_array[, , z, z] <- as.matrix(c_matrix)
  c_array[, , 1, 2] <- as.matrix(c_matrix)
  c_array[, , 2, 1] <- as.matrix(c_matrix)
  # create problem
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
       add_neighbor_constraints(seq_len(3), c_array)
  # compile problem
  o <- compile(p)
  image(o$A())
  # run tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  neighbor_rows <- (n_f * n_z) + n_pu + seq_len(n_pu * n_z)
  # check that rhs has been added correctly
  expect_equal(o$rhs()[neighbor_rows], rep(0, n_pu * n_z))
  # check that sense has been added correctly
  expect_equal(o$sense()[neighbor_rows], rep(">=", n_pu * n_z))
  # check that row ids have been added correctly
  expect_equal(o$row_ids()[neighbor_rows], rep("n", n_pu * n_z))
  # check that neighbor constraints have been added correctly
  counter <- 0
  correct_matrix <- matrix(0, ncol = n_pu * n_z, nrow = n_pu * n_z)
  for (z in seq_len(n_z)) {
    for (i in p$planning_unit_indices()) {
      counter <- counter + 1
      indices <- which(c_array[i, p$planning_unit_indices(), z, ] > 1e-10)
      correct_matrix[counter, indices] <- 1
      correct_matrix[counter, counter] <- seq_len(3)[z]
    }
  }
  correct_matrix <- as(correct_matrix, "Matrix")
  expect_true(o$A()[neighbor_rows, ] == correct_matrix)
})
