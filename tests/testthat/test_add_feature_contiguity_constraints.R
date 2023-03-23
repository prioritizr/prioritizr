context("add_feature_contiguity_constraints")

test_that("compile (single zone)", {
  # create data
  spp1_habitat <- terra::rast(matrix(c(
    5, 0, 5,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp2_habitat <- terra::rast(matrix(c(
    2, 2, 0,
    0, 0, 0,
    20, 0, 20), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp1_conductance <- terra::rast(matrix(c(
    1, 1, 1,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp2_conductance <- terra::rast(matrix(c(
    1, 1, 0,
    0, 0, 0,
    1, 1, 1), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  cost <- terra::setValues(spp1_conductance, 1)
  features <- c(spp1_habitat, spp2_habitat)
  names(features) <- make.unique(names(features))
  cl <- list(
    as_Matrix(connectivity_matrix(cost, spp1_conductance) > 0.3, "dgCMatrix"),
    as_Matrix(connectivity_matrix(cost, spp2_conductance) > 0.3, "dgCMatrix")
  )
  cl <- lapply(cl, Matrix::drop0)
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(6, 30)) %>%
    add_feature_contiguity_constraints(diag(1), data = cl)
  # compile problem
  o  <- compile(p)
  # perform preliminary calculations
  n_pu <- terra::ncell(cost)
  n_f <- terra::nlyr(features)
  rij <- rij_matrix(cost, features)
  costs <- c(terra::values(cost))
  targ <- c(6, 30)
  cm <- lapply(cl, Matrix::forceSymmetric, uplo = "L")
  cm <- lapply(cm, Matrix::tril)
  cm <- lapply(cm, as_Matrix, "dgTMatrix")
  n_edges <- vapply(cm, function(x) length(x@i), integer(1))
  n_j_ends <- vapply(cm, function(x) length(unique(x@j)), integer(1))
  n_con_cols <- sum(n_edges)
  # test that constraints have been added correctly
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(costs, rep(0, n_pu * n_f), rep(0, n_con_cols)))
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu), rep("pu_ijz", n_pu * n_f), rep("fc", n_con_cols))
  )
  expect_equal(o$lb(), rep(0, n_pu + (n_pu * n_f) + n_con_cols))
  expect_equal(o$ub(), rep(1, n_pu + (n_pu * n_f) + n_con_cols))
  expect_equal(
    o$row_ids(),
    c(
      rep("pu_ijz", n_pu * n_f), rep("spp_target", n_f),
      rep("fc1", sum(n_edges)), rep("fc2", sum(n_j_ends)),
      rep("fc3", 2)
    )
  )
  expect_equal(
    o$sense(),
    c(
      rep("<=", n_pu * n_f), rep(">=", n_f), rep("<=", sum(n_edges)),
      rep("<=", sum(n_j_ends)), rep("=", 2)
    )
  )
  expect_equal(
    o$rhs(),
    c(
      rep(0, n_pu * n_f), targ, rep(0, sum(n_edges)), rep(0, sum(n_j_ends)),
      rep(-1, 2)
    )
  )
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
    curr_row[n_pu + ( (i - 1) * n_pu) + seq_len(n_pu)] <-
      terra::values(features[[i]])
    expect_equal(o$A()[row, ], curr_row)
  }
  # fc1 contiguity constraints
  for (i in seq_len(n_f)) {
    for (j in seq_len(n_edges[i])) {
      row <- row + 1
      curr_row <- rep(0, n_cols)
      curr_row[n_pu + ( (i - 1) * n_pu) + 1 + cm[[i]]@i[j]] <- -1
      curr_row[n_pu + (n_pu * n_f) + max(0, n_edges[i - 1]) + j] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
  }
  # fc2 contiguity constraints
  for (i in seq_len(n_f)) {
    for (j in seq_len(n_j_ends[i])) {
      row <- row + 1
      curr_row <- rep(0, n_cols)
      idx1 <- n_pu + ( (i - 1) * n_pu) + 1 + unique(cm[[i]]@j)[j]
      idx2 <- n_pu + (n_pu * n_f) + max(0, n_edges[i - 1]) + j
      curr_row[idx1] <- -1
      curr_row[idx2] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
  }
  # fc3 contiguity constraints
  for (i in seq_len(n_f)) {
    row <- row + 1
    curr_row <- rep(0, n_cols)
    idx1 <- n_pu + ( (i - 1) * n_pu) + seq_len(n_pu)
    idx2 <- n_pu + (n_pu * n_f) + max(0, n_edges[i - 1]) + seq_len(n_edges[i])
    curr_row[idx1] <- -1
    curr_row[idx2] <- 1
    expect_equal(o$A()[row, ], curr_row)
  }
})

test_that("solve (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  spp1_habitat <- terra::rast(matrix(c(
    5, 0, 5,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp2_habitat <- terra::rast(matrix(c(
    2, 2, 0,
    0, 0, 0,
    20, 0, 20), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp1_conductance <- terra::rast(matrix(c(
    1, 1, 1,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp2_conductance <- terra::rast(matrix(c(
    1, 1, 0,
    0, 0, 0,
    1, 1, 1), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  cost <- terra::setValues(spp1_conductance, 1)
  features <- c(spp1_habitat, spp2_habitat)
  names(features) <- make.unique(names(features))
  cl <- list(
    as_Matrix(connectivity_matrix(cost, spp1_conductance) > 0.3, "dgCMatrix"),
    as_Matrix(connectivity_matrix(cost, spp2_conductance) > 0.3, "dgCMatrix")
  )
  cl <- lapply(cl, Matrix::drop0)
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(6, 30)) %>%
    add_feature_contiguity_constraints(diag(1), data = cl) %>%
    add_default_solver(verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # check that solution is correct
  expect_equal(c(terra::values(s1)), c(1, 1, 1, 0, 0, 0, 1, 1, 1))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("compile (multiple zones)", {
  # create data and problem
  spp1_z1 <- terra::rast(matrix(c(
    5, 0, 5,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp2_z1 <- terra::rast(matrix(c(
    2, 2, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    20, 0, 20), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp1_z2 <- terra::rast(matrix(c(
    0, 0, 0,
    0, 0, 0,
    1, 0, 1,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp2_z2 <- terra::rast(matrix(c(
    0, 0, 0,
    0, 0, 0,
    0, 1, 0,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  cost <- terra::setValues(spp1_z1, 1)[[c(1, 1)]]
  features <- zones(c(spp1_z1, spp2_z1), c(spp1_z2, spp2_z2))
  targets <- matrix(c(6, 30, 2, 1), ncol = 2, nrow = 2)
  zm <- list(diag(2), matrix(1, ncol = 2, nrow = 2))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_feature_contiguity_constraints(zm)
  # compile problem
  o <- compile(p)
  # perform preliminary calculations
  n_pu <- terra::ncell(cost)
  n_f <- terra::nlyr(features[[1]])
  n_z <- number_of_zones(features)
  rij <- lapply(seq_len(2), function(i) rij_matrix(cost, features[[i]]))
  costs <- terra::values(cost)
  cm <- adjacency_matrix(cost)
  cm <- Matrix::forceSymmetric(cm, uplo = "L")
  cm <- as_Matrix(Matrix::tril(cm), "dgTMatrix")
  zcl <- lapply(zm, function(x) {
    g <- igraph::graph_from_adjacency_matrix(
      x, diag = FALSE, mode = "undirected", weighted = NULL
    )
    igraph::clusters(g)$membership * diag(x)
  })
  n_clusters_per_feature <- vapply(zcl, max, numeric(1))
  n_clusters <- sum(n_clusters_per_feature)
  cluster_zone_ids <- list(1, 2, c(1, 2))
  cluster_feature_ids <- c(1, 1, 2)
  # construct constraint indices
  pu_i <- list()
  pu_j <- list()
  for (k in seq_along(cluster_zone_ids)) {
    pu_i[[k]] <- numeric(0)
    pu_j[[k]] <- numeric(0)
    for (z1 in seq_along(cluster_zone_ids[[k]])) {
      for (z2 in seq(z1, length(cluster_zone_ids[[k]]))) {
        pu_i[[k]] <- c(
          pu_i[[k]],
          (n_pu * n_z) +
            ((cluster_zone_ids[[k]][z1] - 1) * n_pu * n_f) +
            ((cluster_feature_ids[k] - 1) * n_pu) + cm@i + 1
        )
        pu_j[[k]] <- c(
          pu_j[[k]],
          (n_pu * n_z) +
            ((cluster_zone_ids[[k]][z2] - 1) * n_pu * n_f) +
            ((cluster_feature_ids[k] - 1) * n_pu) + cm@j + 1
        )
      }
    }
  }
  # sort the constraint indices
  for (k in seq_along(cluster_zone_ids)) {
    ord <- order(pu_j[[k]], pu_i[[k]])
    pu_i[[k]] <- pu_i[[k]][ord]
    pu_j[[k]] <- pu_j[[k]][ord]
  }
  # calculate graph statistics
  n_edges_per_cluster <- vapply(pu_i, length, integer(1))
  n_j_ends_per_cluster <- vapply(
    pu_j, function(x) length(unique(x)), integer(1)
  )
  n_con_cols <- sum(n_edges_per_cluster)
  starting_indices <- c(0)
  for (k in seq_along(cluster_zone_ids)[-1]) {
    starting_indices <- c(
      starting_indices,
     starting_indices[k - 1] + length(pu_i[[k - 1]])
    )
  }
  # test that constraints have been added correctly
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(costs, rep(0, n_pu * n_f * n_z), rep(0, n_con_cols)))
  expect_equal(
    o$col_ids(),
    c(
      rep("pu", n_pu * n_z), rep("pu_ijz", n_pu * n_f * n_z),
      rep("fc", n_con_cols)
    )
  )
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + (n_pu * n_f * n_z) + n_con_cols))
  expect_equal(o$ub(), rep(1, (n_pu * n_z) + (n_pu * n_f * n_z) + n_con_cols))
  expect_equal(
    o$row_ids(),
    c(
      rep("pu_ijz", n_pu * n_f * n_z), rep("spp_target", n_f * n_z),
      rep("pu_zone", n_pu), rep("fc1", n_con_cols),
      rep("fc2", sum(n_j_ends_per_cluster)), rep("fc3", 3)
    )
  )
  expect_equal(
    o$sense(),
    c(
      rep("<=", n_pu * n_f * n_z), rep(">=", n_f * n_z),
      rep("<=", n_pu), rep("<=", n_con_cols),
      rep("<=", sum(n_j_ends_per_cluster)), rep("=", 3)
    )
  )
  expect_equal(
    o$rhs(),
    c(
      rep(0, n_pu * n_f * n_z), c(targets), rep(1, n_pu),
      rep(0, n_con_cols), rep(0, sum(n_j_ends_per_cluster)), rep(-1, 3)
    )
  )
  # test that the model matrix has been constructed correctly
  row <- 0
  n_cols <- (n_z * n_pu) + (n_pu * n_f * n_z) + n_con_cols
  # y_ij <= x_j constraints
  for (z in seq_len(n_z)) {
    for (f in seq_len(n_f)) {
      for (j in seq_len(n_pu)) {
        row <- row + 1
        curr_row <- rep(0, n_cols)
        idx1 <- ((z - 1) * n_pu) + j
        idx2 <- (n_pu * n_z) + ((z - 1) * n_pu * n_f) + ((f - 1) * n_pu) + j
        curr_row[idx1] <- -1
        curr_row[idx2] <- 1
        expect_equal(o$A()[row, ], curr_row)
      }
    }
  }
  # \sum_{j \in J} y_ij >= target[i]
  for (z in seq_len(n_z)) {
    for (f in seq_len(n_f)) {
      row <- row + 1
      curr_row <- rep(0, n_cols)
      idx <-
        (n_z * n_pu) + ((z - 1) * n_pu * n_f) + ((f - 1) * n_pu) + seq_len(n_pu)
      curr_row[idx] <- c(terra::values(features[[z]][[f]]))
      expect_equal(o$A()[row, ], curr_row)
    }
  }
  # constraints to ensure each planning unit is only allocated to one zone
  for (j in seq_len(n_pu)) {
    row <- row + 1
    curr_row <- rep(0, n_cols)
    for (z in seq_len(n_z))
      curr_row[((z - 1) * n_pu) + j] <- 1
    expect_equal(o$A()[row, ], curr_row)
  }
  # fc1 contiguity constraints
  for (k in seq_along(cluster_zone_ids)) {
    for (j in seq_len(n_edges_per_cluster[k])) {
      row <- row + 1
      curr_row <- rep(0, n_cols)
      curr_row[pu_i[[k]][j]] <- -1
      idx <- (n_pu * n_z) + (n_pu * n_z * n_f) + starting_indices[k] + j
      curr_row[idx] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
  }
  # fc2 contiguity constraints
  curr_pu_j <- Inf
  for (k in seq_along(cluster_zone_ids)) {
    for (j in seq_along(pu_i[[k]])) {
      if (pu_j[[k]][[j]] != curr_pu_j) {
        if (is.finite(curr_pu_j))
          expect_equal(o$A()[row, ], curr_row)
        row <- row + 1
        curr_pu_j <- pu_j[[k]][j]
        curr_row <- rep(0, n_cols)
        curr_row[curr_pu_j] <- -1
      }
      idx <- (n_pu * n_z) + (n_pu * n_z * n_f) + starting_indices[k] + j
      curr_row[idx] <- 1
    }
  }
  # fc3 contiguity constraints
  for (k in seq_along(cluster_zone_ids)) {
    row <- row + 1
    curr_row <- rep(0, n_cols)
    for (z in cluster_zone_ids[[k]]) {
      idx1 <-
        (n_pu * n_z) + ((z - 1) * n_pu * n_f) +
        ((cluster_feature_ids[k] - 1) * n_pu) + seq_len(n_pu)
      curr_row[idx1] <- -1
    }
    idx2 <-
      (n_pu * n_z) + (n_pu * n_f * n_z) + starting_indices[k] +
      seq_len(n_edges_per_cluster[k])
    curr_row[idx2] <- 1
    expect_equal(o$A()[row, ], curr_row)
  }
})

test_that("solve (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  spp1_z1 <- terra::rast(matrix(c(
    5, 0, 5,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp2_z1 <- terra::rast(matrix(c(
    2, 2, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    20, 0, 20), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp1_z2 <- terra::rast(matrix(c(
    0, 0, 0,
    0, 0, 0,
    0, 0, 2,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  spp2_z2 <- terra::rast(matrix(c(
    0, 0, 0,
    0, 0, 0,
    1, 0, 0,
    0, 0, 0,
    0, 0, 0), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  cost <- c(
    terra::rast(matrix(c(
      1, 1, 1,
      1, 1, 1,
      1, 1, 1,
      1, 1, 1,
      1, 1, 1), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1)),
    terra::rast(matrix(c(
      2, 2, 2,
      2, 2, 2,
      2, 2, 2,
      2, 2, 2,
      2, 2, 2), byrow = TRUE, ncol = 3), ext = terra::ext(0, 1, 0, 1))
  )
  features <- zones(c(spp1_z1, spp2_z1), c(spp1_z2, spp2_z2))
  targets <- matrix(c(6, 30, 2, 1), ncol = 2, nrow = 2)
  zm <- list(diag(2), matrix(1, ncol = 2, nrow = 2))
  # create and solve problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_feature_contiguity_constraints(zm) %>%
    add_default_solver(verbose = FALSE)
  s <- solve(p)
  # run tests
  expect_is(s, "SpatRaster")
  expect_equal(
    c(terra::values(s[[1]])),
    c(1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1)
  )
  expect_equal(
    c(terra::values(s[[2]])),
    c(0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0)
  )
})

test_that("invalid inputs (single zone)", {
  # create data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  cm <- as.matrix(adjacency_matrix(sim_pu_polygons))
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1)
  # tests
  expect_tidy_error(add_feature_contiguity_constraints(p, NA))
  expect_tidy_error(add_feature_contiguity_constraints(p, diag(1) + 1))
  expect_tidy_error(add_feature_contiguity_constraints(p, diag(1) - 2))
  expect_tidy_error(add_feature_contiguity_constraints(p, diag(1) - NA))
  expect_tidy_error(add_feature_contiguity_constraints(p, data = cm[-1, ]))
  expect_tidy_error(add_feature_contiguity_constraints(p, data = cm[, -1]))
  expect_tidy_error(add_feature_contiguity_constraints(p, data = cm + 1))
  expect_tidy_error(add_feature_contiguity_constraints(p, data = cm - 1))
  expect_tidy_error(
    add_feature_contiguity_constraints(p, data = `[<-`(cm, 1, 1, NA))
  )
})

test_that("invalid inputs (multiple zones)", {
  # create data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  cm <- as.matrix(adjacency_matrix(sim_zones_pu_polygons))
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(0.2, nrow = 5, ncol = 3))
  # tests
  expect_tidy_error(add_feature_contiguity_constraints(p, NA))
  expect_tidy_error(add_feature_contiguity_constraints(p, NULL))
  expect_tidy_error(add_feature_contiguity_constraints(p, diag(3) + 1))
  expect_tidy_error(add_feature_contiguity_constraints(p, diag(3) - 2))
  expect_tidy_error(add_feature_contiguity_constraints(p, diag(3) - NA))
  expect_tidy_error(
    add_feature_contiguity_constraints(
      p,
      `[<-`(matrix(1, ncol = 3, nrow = 3), 3, 3, 0)
    )
  )
  expect_tidy_error(add_feature_contiguity_constraints(p, diag(3), cm[-1, ]))
  expect_tidy_error(add_feature_contiguity_constraints(p, diag(3), cm[, -1]))
  expect_tidy_error(add_feature_contiguity_constraints(p, diag(3), cm + 1))
  expect_tidy_error(add_feature_contiguity_constraints(p, diag(3), cm - 1))
  expect_tidy_error(
    add_feature_contiguity_constraints(p, diag(3), `[<-`(cm, 1, 1, NA))
  )
})
