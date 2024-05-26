test_that("compile (sf, clamp = FALSE, single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()[c(1:2, 10:12, 20:22), ]
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_neighbor_constraints(1, clamp = FALSE)
  # compile problem
  o <- compile(p)
  # calculations for tests
  n_pu <- nrow(sim_pu_polygons)
  n_f <- terra::nlyr(sim_features)
  neighbor_rows <- n_f + seq_len(n_pu)
  c_matrix <- adjacency_matrix(sim_pu_polygons)
  # tests
  expect_equal(o$rhs()[neighbor_rows], rep(0, n_pu))
  expect_equal(o$sense()[neighbor_rows], rep(">=", n_pu))
  expect_equal(o$row_ids()[neighbor_rows], rep("n", n_pu))
  for (i in seq_len(n_pu)) {
    correct_row <- replace(rep(0, n_pu), which(c_matrix[i, ] > 1e-10), 1)
    correct_row[i] <- -1
    expect_equal(o$A()[neighbor_rows[i], ], correct_row)
  }
})

test_that("compile (sf, clamp = TRUE, single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()[c(1:2, 10:12, 20:22), ]
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_neighbor_constraints(3, clamp = TRUE)
  # compile problem
  o <- compile(p)
  # calculations for tests
  n_pu <- nrow(sim_pu_polygons)
  n_f <- terra::nlyr(sim_features)
  neighbor_rows <- n_f + seq_len(n_pu)
  c_matrix <- adjacency_matrix(sim_pu_polygons)
  # tests
  expect_equal(o$rhs()[neighbor_rows], rep(0, n_pu))
  expect_equal(o$sense()[neighbor_rows], rep(">=", n_pu))
  expect_equal(o$row_ids()[neighbor_rows], rep("n", n_pu))
  for (i in seq_len(n_pu)) {
    correct_row <- replace(rep(0, n_pu), which(c_matrix[i, ] > 1e-10), 1)
    correct_row[i] <- -1 * pmin(3, sum(c_matrix[i, ]))
    expect_equal(o$A()[neighbor_rows[i], ], correct_row)
  }
})

test_that("compile (manually data specified, clamp = FALSE, single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()[c(1:2, 10:12, 20:22), ]
  sim_features <- get_sim_features()
  # create matrix data
  a1 <- adjacency_matrix(sim_pu_polygons)
  a2 <- as.matrix(a1)
  a3 <- setNames(matrix_to_triplet_dataframe(a1), c("id1", "id2", "boundary"))
  a4 <- array(c(a2), dim = c(rep(nrow(sim_pu_polygons), 2), 1, 1))
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2)
  # compile problems
  o1 <-
    p %>%
    add_neighbor_constraints(1, clamp = FALSE) %>%
    compile()
  o2 <-
    p %>%
    add_neighbor_constraints(1, clamp = FALSE, data = a1) %>%
    compile()
  o3 <-
    p %>%
    add_neighbor_constraints(1, clamp = FALSE, data = a2) %>%
    compile()
  o4 <-
    p %>%
    add_neighbor_constraints(1, clamp = FALSE, data = a3) %>%
    compile()
  o5 <-
    p %>%
    add_neighbor_constraints(1, clamp = FALSE, zones = NULL, data = a4) %>%
    compile()
  # test that all problems are equivalent
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$obj(), o3$obj())
  expect_equal(o1$obj(), o4$obj())
  expect_equal(o1$obj(), o5$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_true(all(o1$A() == o3$A()))
  expect_true(all(o1$A() == o4$A()))
  expect_true(all(o5$A() == o5$A()))
})

test_that("solve (sf, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_neighbor_constraints(3, clamp = FALSE) %>%
    add_default_solver(time_limit = 5, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # calculations for tests
  n_neighbors <- vapply(
    sf::st_intersects(s1[s1$solution_1 == 1, ]), length, integer(1)
  )
  # tests
  expect_true(all(n_neighbors >= 3))
  expect_equal(s1$solution_1, s2$solution_1)
})

test_that("invalid input (single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2)
  # tests
  expect_tidy_error(add_neighbor_constraints(p, -3))
  expect_tidy_error(add_neighbor_constraints(p, NA))
  expect_tidy_error(add_neighbor_constraints(p, 1, NA))
  expect_tidy_error(add_neighbor_constraints(p, 1, "a"))
  expect_tidy_error(add_neighbor_constraints(p, 1, -3))
  expect_tidy_error({
    # create problem without spatial data
    pu <- data.frame(
      id = seq_len(10), cost = c(0.2, NA, runif(8)),
      spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
    )
    problem(
      pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    ) %>%
    add_neighbor_constraints(2, clamp = FALSE)
  })
})

test_that("compile (SpatRaster, clamp = FALSE, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create data
  k <- seq_len(3)
  zones <- diag(3)
  zones[1, 2] <- 1
  zones[2, 1] <- 1
  adj_m <- adjacency_matrix(sim_zones_pu_raster, directions = 8)
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
    add_neighbor_constraints(k, clamp = FALSE, zones, data = adj_m)
  # compile problem
  o <- compile(p)
  # calculations for tests
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
  # tests
  expect_equal(o$rhs()[neighbor_rows], rep(0, n_pu * n_z))
  expect_equal(o$sense()[neighbor_rows], rep(">=", n_pu * n_z))
  expect_equal(o$row_ids()[neighbor_rows], rep("n", n_pu * n_z))
  # test that neighbor constraints have been added correctly
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

test_that("compile (SpatRaster, clamp = TRUE, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create data
  k <- c(1, 7, 5)
  zones <- diag(3)
  zones[1, 2] <- 1
  zones[2, 1] <- 1
  adj_m <- adjacency_matrix(sim_zones_pu_raster, directions = 8)
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
    add_neighbor_constraints(k, clamp = TRUE, zones, data = adj_m)
  # compile problem
  o <- compile(p)
  # calculations for tests
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
  # tests
  expect_equal(o$rhs()[neighbor_rows], rep(0, n_pu * n_z))
  expect_equal(o$sense()[neighbor_rows], rep(">=", n_pu * n_z))
  expect_equal(o$row_ids()[neighbor_rows], rep("n", n_pu * n_z))
  # test that neighbor constraints have been added correctly
  counter <- 0
  correct_matrix <- matrix(0, ncol = n_pu * n_z, nrow = n_pu * n_z)
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_pu)) {
      counter <- counter + 1
      indices <- which(c_array[i, , z, ] > 1e-15)
      correct_matrix[counter, indices] <- 1
      correct_matrix[counter, counter] <-
        -1 * min(k[z], sum(rowSums(c_array[i, , z, ]) > 1e-15))
    }
  }
  correct_matrix <- as(correct_matrix, "Matrix")
  expect_true(all(o$A()[neighbor_rows, ] == correct_matrix))
})

test_that("compile (manually specified data, clamp = FALSE, multiple zones)", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create different data formats
  k <- seq_len(3)
  z <- diag(3)
  z[1, 2] <- 1
  z[2, 1] <- 1
  cm1 <- adjacency_matrix(sim_zones_pu_polygons)
  cm2 <- as.matrix(cm1)
  cdf <-
    cm1 %>%
    matrix_to_triplet_dataframe() %>%
    setNames(c("id1", "id2", "boundary"))
  cdf2 <- cdf[0, ]
  cdf2$zone1 <- character(0)
  cdf2$zone2 <- character(0)
  for (z1 in seq_len(3)) {
    for (z2 in seq_len(3)) {
      if (z[z1, z2] > 0.5) {
        d <- cdf
        d$zone1 <- paste0("zone_", z1)
        d$zone2 <- paste0("zone_", z2)
        cdf2 <- rbind(cdf2, d)
      }
    }
  }
  carray <- array(0, dim = c(nrow(cm1), ncol(cm1), 3, 3))
  for (z1 in seq_len(3))
    for (z2 in seq_len(3))
      carray[, , z1, z2] <- cm2 * z[z1, z2]
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, ncol = 3, nrow = 5))
  # compile problems
  o1 <-
    p %>%
    add_neighbor_constraints(k, clamp = FALSE, z) %>%
    compile()
  o2 <-
    p %>%
    add_neighbor_constraints(k, clamp = FALSE, z, cm1) %>%
    compile()
  o3 <-
    p %>%
    add_neighbor_constraints(k, clamp = FALSE, z, cm2) %>%
    compile()
  o4 <-
    p %>%
    add_neighbor_constraints(k, clamp = FALSE, z, cdf) %>%
    compile()
  o5 <-
    p %>%
    add_neighbor_constraints(k, clamp = FALSE, z = NULL, cdf2) %>%
    compile()
  o6 <-
    p %>%
    add_neighbor_constraints(k, clamp = FALSE, z = NULL, carray) %>%
    compile()
  # tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$obj(), o3$obj())
  expect_equal(o1$obj(), o4$obj())
  expect_equal(o1$obj(), o5$obj())
  expect_equal(o1$obj(), o6$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_true(all(o1$A() == o3$A()))
  expect_true(all(o1$A() == o4$A()))
  expect_true(all(o1$A() == o5$A()))
  expect_true(all(o1$A() == o6$A()))
})

test_that("solve (sf, clamp = FALSE, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create data
  k <- c(1, 2, 4)
  z <- diag(3)
  # create and solve problem
  s <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
    add_neighbor_constraints(k, clamp = FALSE, z) %>%
    add_default_solver(verbose = FALSE) %>%
    solve()
  # tests
  for (z in seq_along(k)) {
    curr_column <- paste0("solution_1_zone_", z)
    n_neighbors <- vapply(
      sf::st_intersects(s[s[[curr_column]] == 1, ]), length, integer(1)
    )
    expect_true(all(n_neighbors >= k[z]))
  }
})

test_that("invalid input (multiple zones)", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create neighbor data
  k <- seq_len(3)
  z <- diag(3)
  a <- array(1, dim = c(rep(length(sim_zones_pu_polygons), 2), 3, 3))
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, ncol = 3, nrow = 5))
  # tests
  expect_tidy_error(add_neighbor_constraints(p, k[-1], FALSE, z))
  expect_tidy_error(add_neighbor_constraints(p, k + 0.5, FALSE, z))
  expect_tidy_error(add_neighbor_constraints(p, k + c(NA, 0, 0), FALSE, z))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, z[, -1]))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, z[-1, ]))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, z - 1))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, z + 1))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, `[<-`(z, 1, 1, NA)))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, z, a))
  expect_tidy_error(add_neighbor_constraints(p, k, 1, z))
  expect_tidy_error(add_neighbor_constraints(p, k, "a", z))
  expect_tidy_error(add_neighbor_constraints(p, k, b, z))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, NULL, a[-1, , , ]))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, NULL, a[, -1, , ]))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, NULL, a[, , -1, ]))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, NULL, a[, , , -1]))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, NULL, a + 1))
  expect_tidy_error(add_neighbor_constraints(p, k, FALSE, NULL, a - 2))
  expect_tidy_error(
    add_neighbor_constraints(p, k, FALSE, NULL, `[<-`(a, 1, 1, 1, 1, NA))
  )
})
