context("add_corridor_constraints")

test_that("compile", {
  spp1.habitat <- raster::raster(matrix(c(
    5, 0, 5,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3))
  spp2.habitat <- raster::raster(matrix(c(
    2, 2, 0,
    0, 0, 0,
    20, 0, 20), byrow = TRUE, ncol = 3))
  spp1.conductance <- raster::raster(matrix(c(
    9, 9, 9,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3))
  spp2.conductance <- raster::raster(matrix(c(
    1, 1, 0,
    0, 0, 0,
    1, 1, 1), byrow = TRUE, ncol = 3))
  cost <- raster::setValues(spp1.conductance, 1)
  features <- raster::stack(spp1.habitat, spp2.habitat)
  conductance <- raster::stack(spp1.conductance, spp2.conductance)
  # create problem
  p <- problem(cost, features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_corridor_constraints(conductance, rep(0.8, nlayers(features))) %>%
    add_default_solver(time_limit = 5)
  # compile problem
  o  <- compile(p)
  # perform preliminary calculations
  n_pu <- raster::ncell(cost)
  n_f <- raster::nlayers(features)
  rij <- rij_matrix(cost, features)
  costs <- raster::values(cost)
  c_matrix <- lapply(seq_len(nlayers(features)), function(i) {
    x <- connectivity_matrix(cost, conductance[[i]])
    class(x) <- "dgCMatrix"
    y <- x
    y[y > 0] <- 1
    y[x < min(x) + (diff(range(x)) * 0.8)] <- 0
    return(as(y, "dgTMatrix"))
  })
  targ <- unname(raster::cellStats(features, "sum")) * 0.1
  n_edges <- vapply(c_matrix, function(x) length(x@i), integer(1))
  n_j_ends <- vapply(c_matrix, function(x) length(unique(x@j)), integer(1))
  n_con_cols <- sum(n_edges)
  # test that constraints have been added correctly
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(costs, rep(0, n_pu * n_f), rep(0, n_con_cols)))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("pu_ij", n_pu * n_f),
                              rep("c", n_con_cols)))
  expect_equal(o$lb(), rep(0, n_pu + (n_pu * n_f) + n_con_cols))
  expect_equal(o$ub(), rep(1, n_pu + (n_pu * n_f) + n_con_cols))
  expect_equal(o$row_ids(), c(rep("pu_ij", n_pu * n_f), rep("spp_target", n_f),
                              rep("c1", n_edges[1]), rep("c2", n_j_ends[1]),
                              "c3", rep("c1", n_edges[2]),
                              rep("c2", n_j_ends[2]), "c3"))
  expect_equal(o$sense(), c(rep("<=", n_pu * n_f), rep(">=", n_f),
                              rep("<=", n_edges[1]), rep("<=", n_j_ends[1]),
                              "=", rep("<=", n_edges[2]),
                              rep("<=", n_j_ends[2]), "="))
  expect_equal(o$rhs(), c(rep(0, n_pu * n_f), targ,
                              rep(0, n_edges[1]), rep(0, n_j_ends[1]),
                              -1, rep(0, n_edges[2]),
                              rep(0, n_j_ends[2]), -1))
  # test that the model matrix has been constructed correctly
  row <- 0
  n_cols <- n_pu + (n_pu * n_f) + n_con_cols
  # y_ij <= x_j constraints
  for (i in seq_len(n_f)) {
    for (j in seq_len(n_pu)) {
      row <- row + 1
      curr_row <- rep(0, n_cols)
      curr_row[j] <- -1
      curr_row[n_pu + ( (i - 1) * n_pu) + j] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
  }
  # \sum_{j \in J} y_ij >= target[i]
  for (i in seq_len(n_f)) {
    row <- row + 1
    curr_row <- rep(0, n_cols)
    curr_row[n_pu + ( (i - 1) * n_pu) + seq_len(n_pu)] <- values(features[[i]])
    expect_equal(o$A()[row, ], curr_row)
  }
  # corridor constraints
  for (i in seq_len(n_f)) {
    # c1 constraints
    for (j in seq_len(n_edges[i])) {
      row <- row + 1
      curr_row <- rep(0, n_cols)
      curr_row[n_pu + ( (i - 1) * n_pu) + 1 + c_matrix[[i]]@i[j]] <- -1
      curr_row[n_pu + (n_pu * n_f) + max(0, n_edges[i - 1]) + j] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
    # c2 constraints
    for (j in seq_len(n_j_ends[i])) {
      row <- row + 1
      curr_row <- rep(0, n_cols)
      curr_row[n_pu + ( (i - 1) * n_pu) + 1 +
               unique(c_matrix[[i]]@j)[j]] <- -1
      curr_row[n_pu + (n_pu * n_f) + max(0, n_edges[i - 1]) + j] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
    # c3 constraints
    row <- row + 1
    curr_row <- rep(0, n_cols)
    curr_row[n_pu + ( (i - 1) * n_pu) + seq_len(n_pu)] <- -1
    curr_row[n_pu + (n_pu * n_f) + max(0, n_edges[i - 1]) +
             seq_len(n_edges[i])] <- 1
    expect_equal(o$A()[row, ], curr_row)
  }
})

test_that("solve", {
  skip_on_cran()
  # create data
  spp1.habitat <- raster::raster(matrix(c(
    5, 0, 5,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3))
  spp2.habitat <- raster::raster(matrix(c(
    2, 2, 0,
    0, 0, 0,
    20, 0, 20), byrow = TRUE, ncol = 3))
  spp1.conductance <- raster::raster(matrix(c(
    9, 9, 9,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3))
  spp2.conductance <- raster::raster(matrix(c(
    1, 1, 0,
    0, 0, 0,
    1, 1, 1), byrow = TRUE, ncol = 3))
  cost <- raster::setValues(spp1.conductance, 1)
  features <- raster::stack(spp1.habitat, spp2.habitat)
  conductance <- raster::stack(spp1.conductance, spp2.conductance)
  # create problem
  p <- problem(cost, features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.9) %>%
    add_corridor_constraints(conductance, rep(0.8, nlayers(features))) %>%
    add_default_solver(time_limit = 5)
  o  <- compile(p)
  # solve problem
  s <- solve(p)
  # check that solution is correct
  expect_equal(raster::values(s), c(1, 1, 1, 0, 0, 0, 1, 1, 1))
})

test_that("list input (compile)", {
  spp1.habitat <- raster::raster(matrix(c(
    5, 0, 5,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3))
  spp2.habitat <- raster::raster(matrix(c(
    2, 2, 0,
    0, 0, 0,
    20, 0, 20), byrow = TRUE, ncol = 3))
  spp1.conductance <- raster::raster(matrix(c(
    9, 9, 9,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3))
  spp2.conductance <- raster::raster(matrix(c(
    1, 1, 0,
    0, 0, 0,
    1, 1, 1), byrow = TRUE, ncol = 3))
  cost <- raster::setValues(spp1.conductance, 1)
  features <- raster::stack(spp1.habitat, spp2.habitat)
  conductance <- raster::stack(spp1.conductance, spp2.conductance)
  connectivity_matrices <- list(connectivity_matrix(cost, spp1.conductance),
                                connectivity_matrix(cost, spp2.conductance))
  # create problem
  p1 <- problem(cost, features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_corridor_constraints(connectivity_matrices,
                             rep(0.8, nlayers(features))) %>%
    compile()
  p2 <- problem(cost, features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_corridor_constraints(conductance, rep(0.8, nlayers(features))) %>%
    compile()
  # test that compiled problems are equivalent
  expect_equal(p1$rhs(), p2$rhs())
  expect_equal(p1$sense(), p2$sense())
  expect_equal(p1$modelsense(), p2$modelsense())
  expect_true(all(p1$A() == p2$A()))
  expect_equal(p1$obj(), p2$obj())
  expect_equal(p1$row_ids(), p2$row_ids())
  expect_equal(p1$col_ids(), p2$col_ids())
})

test_that("list input (solve)", {
  skip_on_cran()
  spp1.habitat <- raster::raster(matrix(c(
    5, 0, 5,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3))
  spp2.habitat <- raster::raster(matrix(c(
    2, 2, 0,
    0, 0, 0,
    20, 0, 20), byrow = TRUE, ncol = 3))
  spp1.conductance <- raster::raster(matrix(c(
    9, 9, 9,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3))
  spp2.conductance <- raster::raster(matrix(c(
    1, 1, 0,
    0, 0, 0,
    1, 1, 1), byrow = TRUE, ncol = 3))
  cost <- raster::setValues(spp1.conductance, 1)
  features <- raster::stack(spp1.habitat, spp2.habitat)
  conductance <- raster::stack(spp1.conductance, spp2.conductance)
  conductance_matrices <- list(connectivity_matrix(cost, spp1.conductance),
                               connectivity_matrix(cost, spp2.conductance))
  # create problem
  p1 <- problem(cost, features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_corridor_constraints(conductance_matrices,
                             rep(0.8, nlayers(features))) %>%
    add_default_solver(time_limit = 5) %>%
    solve()
  p2 <- problem(cost, features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_corridor_constraints(conductance,
                             rep(0.8, nlayers(features))) %>%
    add_default_solver(time_limit = 5) %>%
    solve()
  # test that compiled problems are equivalent
  expect_equal(raster::values(p1), raster::values(p2))
})

test_that("invalid inputs", {
  expect_error({
    data(sim_pu_raster, sim_features)
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.9) %>%
    add_corridor_constraints(sim_features, -5)
  })
  expect_error({
    data(sim_pu_raster, sim_features)
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.9) %>%
    add_corridor_constraints(sim_features,
                             rep(-5, raster::nlayers(sim_features)))
  })
  expect_error({
    data(sim_pu_raster, sim_features)
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.9) %>%
    add_corridor_constraints(sim_features,
                             rep(5, raster::nlayers(sim_features)))
  })
  expect_error({
    data(sim_pu_raster, sim_features)
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.9) %>%
    add_corridor_constraints(sim_features, c(0.1, 0.2))
  })
  expect_error({
    data(sim_pu_raster, sim_features)
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.9) %>%
    add_corridor_constraints(sim_features, c(NA_real_,
                             rep(0.1, raster::nlayers(sim_features) - 1)))
  })
})
