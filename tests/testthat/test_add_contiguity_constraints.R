test_that("compile (single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  sim_pu_polygons <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_contiguity_constraints()
  # compile problem
  o <- compile(p)
  # perform preliminary calculations
  cm <- as_Matrix(adjacency_matrix(sim_pu_polygons), "dgCMatrix")
  cm <- Matrix::forceSymmetric(cm, uplo = "L")
  cm <- as_Matrix(Matrix::tril(cm), "dgTMatrix")
  n_pu <- nrow(sim_pu_polygons)
  n_f <- terra::nlyr(sim_features)
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
  expect_equal(
    o$sense()[con_rows],
    c(rep("<=", n_edges), rep("<=", n_j_ends), "=")
  )
  # check that row ids have been added correctly
  expect_equal(
    o$row_ids()[con_rows],
    c(rep("c1", n_edges), rep("c2", n_j_ends), "c3")
  )
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
  skip_if_no_fast_solvers_installed()
  # load data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_contiguity_constraints() %>%
    add_default_solver(verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # check that all selected planning units form a contiguous unit
  expect_true(is_single_patch_sf(s1[s1$solution_1 == 1, ]))
  expect_equal(s1$solution_1, s2$solution_1)
})

test_that("compile (multiple zones)", {
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  sim_zones_pu_polygons <- sim_zones_pu_polygons[c(1:2, 10:12, 20:22), ]
  # create zones data
  z <- diag(3)
  z[1, 2] <- 1
  z[2, 1] <- 1
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
    add_contiguity_constraints(z)
  # compile problem
  o <- compile(p)
  # perform preliminary calculations
  cm <- as_Matrix(adjacency_matrix(sim_zones_pu_polygons), "dgCMatrix")
  cm <- Matrix::forceSymmetric(cm, uplo = "L")
  cm <- as_Matrix(Matrix::tril(cm), "dgTMatrix")
  cl <- c(1, 1, 2)
  n_pu <- nrow(sim_zones_pu_polygons)
  n_f <- terra::nlyr(sim_zones_features[[1]])
  n_z <- number_of_zones(sim_zones_features)
  # compute edge data
  pu_i <- list()
  pu_j <- list()
  for (i in unique(cl)) {
    # insert dummy index to initialize vectors
    pu_i[[i]] <- c(0)
    pu_j[[i]] <- c(0)
    # extract indices
    for (z1 in seq_along(which(cl == i))) {
      for (z2 in seq(z1, sum(cl == i))) {
        pu_i[[i]] <- c(pu_i[[i]], ((z1 - 1) * n_pu) + cm@i)
        pu_j[[i]] <- c(pu_j[[i]], ((z2 - 1) * n_pu) + cm@j)
      }
    }
    # remove dummy index
    pu_i[[i]] <- pu_i[[i]][-1] + 1
    pu_j[[i]] <- pu_j[[i]][-1] + 1
    # reorder data by pu_j
    curr_ord <- order(pu_j[[i]], pu_i[[i]])
    pu_i[[i]] <- pu_i[[i]][curr_ord]
    pu_j[[i]] <- pu_j[[i]][curr_ord]
  }
  n_edges <- sapply(pu_i, length)
  n_j_ends <- sapply(pu_j, function(x) length(unique(x)))
  con_cols <- (n_pu * n_z) + seq_len(sum(sapply(pu_i, length)))
  con_rows <-
    (n_f * n_z) + n_pu + seq_len(sum(n_edges) + sum(n_j_ends) + 2)
  # check that obj has been added correctly
  expect_equal(o$obj()[con_cols], rep(0, sum(n_edges)))
  # check that col_ids have been added correctly
  expect_equal(o$col_ids()[con_cols], rep("c", sum(n_edges)))
  # check that rhs has been added correctly
  expect_equal(
    o$rhs()[con_rows],
    c(rep(0, sum(n_edges)), rep(0, sum(n_j_ends)), -1, -1)
  )
  # check that sense has been added correctly
  expect_equal(
    o$sense()[con_rows],
    c(rep("<=", sum(n_edges)), rep("<=", sum(n_j_ends)), "=", "=")
  )
  # check that row ids have been added correctly
  expect_equal(
    o$row_ids()[con_rows],
    c(rep("c1", sum(n_edges)), rep("c2", sum(n_j_ends)), "c3", "c3")
  )
  ## check that connected constraints have been added correctly
  # c1
  for (k in seq_len(2)) {
    for (i in seq_len(n_edges[k])) {
      correct_row <- rep(0, (n_pu * n_z) + sum(n_edges))
      correct_row[(n_pu * n_z) + i] <- 1
      correct_row[pu_i[[k]][i]] <- -1
      expect_equal(o$A()[(n_f * n_z) + n_pu + i, ], correct_row)
    }
  }
  # c2
  for (k in seq_len(2)) {
    for (j in seq_len(n_j_ends[k])) {
      curr_j <- unique(pu_j[[k]])[j]
      curr_pu_ij <- which(pu_j[[k]] == curr_j)
      correct_row <- rep(0, (n_pu * n_z) + sum(n_edges))
      correct_row[curr_j] <- -1
      correct_row[(n_pu * n_z) + curr_pu_ij] <- 1
      expect_equal(o$A()[(n_f * n_z) + n_pu + sum(n_edges) + j, ], correct_row)
    }
  }
  # c3
  for (k in seq_len(2)) {
    correct_row <- rep(0, (n_pu * n_z) + sum(n_edges))
    curr_pus <-
      rep((which(cl == k) - 1) * n_pu, each = n_pu) +
        rep(seq_len(n_pu), sum(cl == k))
    curr_edges <- (n_z * n_pu) + seq_len(n_edges[k])
    if (k > 1) curr_edges <- curr_edges + n_edges[k - 1]
    correct_row[curr_pus] <- -1
    correct_row[curr_edges] <- 1
    expect_equal(
      o$A()[(n_f * n_z) + n_pu + sum(n_edges) + sum(n_j_ends) + k, ],
      correct_row
    )
  }
})

test_that("solve (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create zones matrix
  z <- diag(3)
  z[1, 2] <- 1
  z[2, 1] <- 1
  # create and solve problem
  s <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
    add_contiguity_constraints(z) %>%
    add_default_solver(verbose = FALSE) %>%
    solve()
  # tests
  expect_true(
    is_single_patch_sf(s[s$solution_1_zone_1 == 1 | s$solution_1_zone_2 == 1, ])
  )
  expect_true(is_single_patch_sf(s[s$solution_1_zone_3 == 1, ]))
})

test_that("alternative data formats", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create connection matrices
  m <- adjacency_matrix(sim_pu_raster)
  m2 <- as_Matrix(m, "dgTMatrix")
  m2 <- data.frame(id1 = m2@i + 1, id2 = m2@j + 1, boundary = m2@x)
  # create problem
  p0 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.45) %>%
    add_binary_decisions()
  p1 <- p0 %>% add_contiguity_constraints(data = m)
  p2 <- p0 %>% add_contiguity_constraints(data = as.matrix(m))
  p3 <- p0 %>% add_contiguity_constraints(data = m2)
  # create objects
  o1 <- as.list(compile(p1))
  o2 <- as.list(compile(p2))
  o3 <- as.list(compile(p3))
  # tests
  expect_equal(o1, o2)
  expect_equal(o1, o3)
})

test_that("invalid inputs (single zone)", {
  # load data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create matrix
  cm <- as.matrix(adjacency_matrix(sim_pu_polygons))
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1)
  # tests
  expect_tidy_error(add_contiguity_constraints(p, NA))
  expect_tidy_error(add_contiguity_constraints(p, diag(1) + 1))
  expect_tidy_error(add_contiguity_constraints(p, diag(1) - 2))
  expect_tidy_error(add_contiguity_constraints(p, diag(1) - NA))
  expect_tidy_error(add_contiguity_constraints(p, data = cm[-1, ]))
  expect_tidy_error(add_contiguity_constraints(p, data = cm[, -1]))
  expect_tidy_error(add_contiguity_constraints(p, data = cm + 1))
  expect_tidy_error(add_contiguity_constraints(p, data = cm - 1))
  expect_tidy_error(add_contiguity_constraints(p, data = `[<-`(cm, 1, 1, NA)))
})

test_that("invalid inputs (multiple zones)", {
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create matrix
  cm <- as.matrix(adjacency_matrix(sim_zones_pu_polygons))
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, nrow = 5, ncol = 3))
  # run tests
  expect_tidy_error(add_contiguity_constraints(p, NA))
  expect_tidy_error(add_contiguity_constraints(p, NULL))
  expect_tidy_error(add_contiguity_constraints(p, diag(3) + 1))
  expect_tidy_error(add_contiguity_constraints(p, diag(3) - 2))
  expect_tidy_error(add_contiguity_constraints(p, diag(3) - NA))
  expect_tidy_error(
    add_contiguity_constraints(
      p, `[<-`(matrix(1, ncol = 3, nrow = 3), 3, 3, 0)
    )
  )
  expect_tidy_error(add_contiguity_constraints(p, diag(3), cm[-1, ]))
  expect_tidy_error(add_contiguity_constraints(p, diag(3), cm[, -1]))
  expect_tidy_error(add_contiguity_constraints(p, diag(3), cm + 1))
  expect_tidy_error(add_contiguity_constraints(p, diag(3), cm - 1))
  expect_tidy_error(add_contiguity_constraints(p, diag(3), `[<-`(cm, 1, 1, NA)))
})
