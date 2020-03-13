context("add_neighbor_constraints")

test_that("compile (Spatial, single zone)", {
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
  c_matrix <- adjacency_matrix(sim_pu_polygons)
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

test_that("compile (manually data specified, single zone)", {
  # create problem
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.2)
  # compile problems
  o1 <- p %>% add_neighbor_constraints(1) %>% compile()
  o2 <- p %>% add_neighbor_constraints(1,
    data = adjacency_matrix(sim_pu_polygons)) %>% compile()
  o3 <- p %>% add_neighbor_constraints(1,
    data = sim_pu_polygons %>% adjacency_matrix() %>% as.matrix()) %>% compile()
  o4 <- p %>% add_neighbor_constraints(1,
    data = sim_pu_polygons %>% adjacency_matrix() %>%
           matrix_to_triplet_dataframe() %>%
           setNames(c("id1", "id2", "boundary"))) %>% compile()
  o5 <- p %>% add_neighbor_constraints(1, zones = NULL,
    data = sim_pu_polygons %>% adjacency_matrix() %>% as.matrix() %>% c() %>%
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

test_that("solve (Spatial, single zone)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_neighbor_constraints(3) %>%
       add_default_solver(time_limit = 5, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # check that selected planning units have three neighbors
  n_neighbors <-
    vapply(rgeos::gIntersects(s1[s1$solution_1 == 1, ], byid = TRUE,
                             returnDense = FALSE), length, integer(1))
  expect_true(all(n_neighbors >= 3))
  expect_equal(s1$solution, s2$solution)
})

test_that("invalid input (single zone)", {
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.2)
  expect_error(add_neighbor_constraints(p, -3))
  expect_error(add_neighbor_constraints(p, NA))
  expect_error({
    # create problem without spatial data
    pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                     spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
    problem(pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
            as.matrix(t(pu[, 3:4]))) %>%
    add_neighbor_constraints(2)
  })
})

test_that("compile (Raster, multiple zones)", {
  # load data
  data(sim_pu_zones_stack, sim_features_zones)
  # create data
  k <- seq_len(3)
  zones <- diag(3)
  zones[1, 2] <- 1
  zones[2, 1] <- 1
  # create problem
  adj_m <- adjacency_matrix(sim_pu_zones_stack, directions = 8)
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
       add_neighbor_constraints(k, zones, data = adj_m)
  # compile problem
  o <- compile(p)
  # preliminary calculations
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  neighbor_rows <- (n_f * n_z) + n_pu + seq_len(n_pu * n_z)
  # create array with neighboring data
  c_matrix <- adj_m
  c_array <- array(0, dim = c(nrow(c_matrix), ncol(c_matrix), 3, 3))
  for (z1 in seq_len(3))
    for (z2 in seq_len(3))
      c_array[, , z1, z2] <- as.matrix(c_matrix) * zones[z1, z2]
  c_array <- c_array[p$planning_unit_indices(), p$planning_unit_indices(), , ]
  # run tests
  ## check that rhs has been added correctly
  expect_equal(o$rhs()[neighbor_rows], rep(0, n_pu * n_z))
  ## check that sense has been added correctly
  expect_equal(o$sense()[neighbor_rows], rep(">=", n_pu * n_z))
  ## check that row ids have been added correctly
  expect_equal(o$row_ids()[neighbor_rows], rep("n", n_pu * n_z))
  ## check that neighbor constraints have been added correctly
  counter <- 0
  correct_matrix <- matrix(0, ncol = n_pu * n_z, nrow = n_pu * n_z)
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_pu)) {
      counter <- counter + 1
      indices <- which(c_array[i, , z, ] > 1e-15)
      correct_matrix[counter, indices] <- 1
      correct_matrix[counter, counter] <- k[z] * -1
    }
  }
  correct_matrix <- as(correct_matrix, "Matrix")
  expect_true(all(o$A()[neighbor_rows, ] == correct_matrix))
})

test_that("compile (manually specified data, multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  # create different data formats
  k <- seq_len(3)
  z <- diag(3)
  z[1, 2] <- 1
  z[2, 1] <- 1
  cm1 <- adjacency_matrix(sim_pu_zones_polygons)
  cm2 <- as.matrix(cm1)
  cdf <- cm1 %>% matrix_to_triplet_dataframe() %>%
                 setNames(c("id1", "id2", "boundary"))
  carray <- array(0, dim = c(nrow(cm1), ncol(cm1), 3, 3))
  for (z1 in seq_len(3))
    for (z2 in seq_len(3))
      carray[, , z1, z2] <- cm2 * z[z1, z2]
  # create basic problem
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.2, ncol = 3, nrow = 5))
  # compile problems
  o1 <- p %>% add_neighbor_constraints(k, z) %>% compile()
  o2 <- p %>% add_neighbor_constraints(k, z, cm1) %>% compile()
  o3 <- p %>% add_neighbor_constraints(k, z, cm2) %>% compile()
  o4 <- p %>% add_neighbor_constraints(k, z, cdf) %>% compile()
  o5 <- p %>% add_neighbor_constraints(k, z = NULL, carray) %>% compile()
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

test_that("solve (Spatial, multiple zones)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  # create data
  k <- c(1, 2, 4)
  z <- diag(3)
  # create and solve problem
  s <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
       add_neighbor_constraints(k, z) %>%
       add_default_solver(verbose = FALSE) %>%
       solve()
  # run tests
  for (z in seq_along(k)) {
    curr_column <- paste0("solution_1_zone_", z)
    n_neighbors <-
      vapply(rgeos::gIntersects(s[s[[curr_column]] == 1, ],byid = TRUE,
                                returnDense = FALSE), length, integer(1))
    expect_true(all(n_neighbors >= k[z]))
  }
})

test_that("invalid input (multiple zones)", {
  # create basic problem
  data(sim_pu_zones_polygons, sim_features_zones)
  k <- seq_len(3)
  z <- diag(3)
  a <- array(1, dim = c(rep(length(sim_pu_zones_polygons), 2), 3, 3))
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.2, ncol = 3, nrow = 5))
  # run tests
  expect_error(add_neighbor_constraints(p, k[-1], z))
  expect_error(add_neighbor_constraints(p, k + 0.5, z))
  expect_error(add_neighbor_constraints(p, k + c(NA, 0, 0), z))
  expect_error(add_neighbor_constraints(p, k, z[, -1]))
  expect_error(add_neighbor_constraints(p, k, z[-1, ]))
  expect_error(add_neighbor_constraints(p, k, z - 1))
  expect_error(add_neighbor_constraints(p, k, z + 1))
  expect_error(add_neighbor_constraints(p, k, `[<-`(z, 1, 1, NA)))
  expect_error(add_neighbor_constraints(p, k, z, a))
  expect_error(add_neighbor_constraints(p, k, NULL, a[-1, , , ]))
  expect_error(add_neighbor_constraints(p, k, NULL, a[, -1, , ]))
  expect_error(add_neighbor_constraints(p, k, NULL, a[, , -1, ]))
  expect_error(add_neighbor_constraints(p, k, NULL, a[, , , -1]))
  expect_error(add_neighbor_constraints(p, k, NULL, a + 1))
  expect_error(add_neighbor_constraints(p, k, NULL, a - 2))
  expect_error(add_neighbor_constraints(p, k, NULL, `[<-`(a, 1, 1, 1, 1, NA)))
})

test_that("compile (sf identical to Spatial, multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_pu_zones_sf, sim_features_zones)
  # create data
  k <- seq_len(3)
  zones <- diag(3)
  zones[1, 2] <- 1
  zones[2, 1] <- 1
  # create problems
  p1 <- problem(sim_pu_zones_polygons, sim_features_zones,
                c("cost_1", "cost_2", "cost_3")) %>%
        add_min_set_objective() %>%
        add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
        add_neighbor_constraints(k, zones)
  p2 <- problem(sf::st_as_sf(sim_pu_zones_polygons), sim_features_zones,
        c("cost_1", "cost_2", "cost_3")) %>%
        add_min_set_objective() %>%
        add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
        add_neighbor_constraints(k, zones)
  # compile problems
  o1 <- as.list(compile(p1))
  o2 <- as.list(compile(p2))
  # run tests
  expect_equal(o1, o2)
})
