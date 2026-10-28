test_that("compile (sf, single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()[c(1:2, 10:12, 20:22), ]
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_separation_constraints()
  # compile problem
  o <- compile(p)
  # calculations for tests
  n_pu <- nrow(sim_pu_polygons)
  n_f <- terra::nlyr(sim_features)
  c_matrix <- as.matrix(adjacency_matrix(sim_pu_polygons))
  pairs <- which(upper.tri(c_matrix) & (c_matrix > 0.5), arr.ind = TRUE)
  sep_rows <- n_f + seq_len(nrow(pairs))
  # tests
  expect_equal(o$nrow(), n_f + nrow(pairs))
  expect_equal(o$rhs()[sep_rows], rep(1, nrow(pairs)))
  expect_equal(o$sense()[sep_rows], rep("<=", nrow(pairs)))
  expect_equal(o$row_ids()[sep_rows], rep("s", nrow(pairs)))
  # each pair of neighbors should have exactly one constraint
  a <- as.matrix(o$A()[sep_rows, , drop = FALSE])
  expect_true(all(rowSums(a) == 2))
  expect_equal(
    sort(apply(a, 1, function(x) paste(which(x > 0.5), collapse = "_"))),
    sort(paste(pairs[, 1], pairs[, 2], sep = "_"))
  )
})

test_that("compile (manually data specified, single zone)", {
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
  o1 <- compile(add_separation_constraints(p))
  o2 <- compile(add_separation_constraints(p, data = a1))
  o3 <- compile(add_separation_constraints(p, data = a2))
  o4 <- compile(add_separation_constraints(p, data = a3))
  o5 <- compile(add_separation_constraints(p, zones = NULL, data = a4))
  # test that all problems are equivalent
  expect_equal(o1$A(), o2$A())
  expect_equal(o1$A(), o3$A())
  expect_equal(o1$A(), o4$A())
  expect_equal(o1$A(), o5$A())
  expect_equal(o1$rhs(), o5$rhs())
  expect_equal(o1$sense(), o5$sense())
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
    add_separation_constraints() %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # calculations for tests
  sel <- which(s$solution_1 > 0.5)
  a <- as.matrix(adjacency_matrix(sim_pu_polygons))
  # tests
  expect_gt(length(sel), 0)
  expect_equal(sum(a[sel, sel]), 0)
})

test_that("solve (locked in neighbors, single zone)", {
  skip_on_cran()
  skip_if_no_solvers_installed()
  # import data
  pu_data <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1), nrow = 2))
  locked_in <- terra::rast(matrix(c(1, 1, 0, 0, 0, 0), nrow = 2))
  ft_data <- terra::rast(matrix(c(0, 1, 0, 0, 0, 1), nrow = 2))
  # create problem
  p <-
    problem(pu_data, ft_data) %>%
    add_min_set_objective() %>%
    add_absolute_targets(1) %>%
    add_locked_in_constraints(locked_in) %>%
    add_separation_constraints() %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # tests
  expect_error(solve(p, force = TRUE, run_checks = FALSE), "impossible")
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
  expect_tidy_error(add_separation_constraints(p, zones = matrix(2)))
  expect_tidy_error(add_separation_constraints(p, zones = matrix(NA_real_)))
  expect_tidy_error(add_separation_constraints(p, zones = diag(2)))
  expect_tidy_error(add_separation_constraints(p, data = "a"))
  expect_tidy_error(add_separation_constraints(p, data = matrix(1, 3, 3)))
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
    add_separation_constraints()
  })
})

test_that("compile (SpatRaster, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create data
  zones <- matrix(1, ncol = 3, nrow = 3)
  diag(zones) <- 0
  adj_m <- adjacency_matrix(sim_zones_pu_raster)
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
    add_separation_constraints(zones, data = adj_m)
  # compile problem
  o <- compile(p)
  # calculations for tests
  n_pu <- p$number_of_planning_units()
  n_z <- p$number_of_zones()
  pu_ind <- p$planning_unit_indices()
  adj_m <- as.matrix(adj_m[pu_ind, pu_ind])
  sep_rows <- which(o$row_ids() == "s")
  a <- as.matrix(o$A()[sep_rows, , drop = FALSE])
  # build the correct constraints, so that for each pair of neighbors
  # i and j and each zone r, x[i, r] + sum_{s != r} x[j, s] <= 1
  correct_rows <- c()
  for (i in seq_len(n_pu)) {
    for (j in which(adj_m[i, ] > 0.5)) {
      if (i == j) next
      for (r in seq_len(n_z)) {
        v <- c(((r - 1) * n_pu) + i, ((setdiff(seq_len(n_z), r) - 1) * n_pu) + j)
        correct_rows <- c(correct_rows, paste(sort(v), collapse = "_"))
      }
    }
  }
  # tests
  expect_equal(o$rhs()[sep_rows], rep(1, length(sep_rows)))
  expect_equal(o$sense()[sep_rows], rep("<=", length(sep_rows)))
  expect_equal(
    sort(apply(a, 1, function(x) paste(which(x > 0.5), collapse = "_"))),
    sort(unique(correct_rows))
  )
})

test_that("solve (SpatRaster, multiple zones, replication)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()[[1:2]]
  # create problem with a zone for each replicate
  n_r <- 3
  z <- zones(
    "r1" = sim_features, "r2" = sim_features, "r3" = sim_features,
    feature_names = names(sim_features)
  )
  sep <- matrix(1, nrow = n_r, ncol = n_r)
  diag(sep) <- 0
  p <-
    problem(c(sim_pu_raster, sim_pu_raster, sim_pu_raster), z) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.05, nrow = 2, ncol = n_r)) %>%
    add_separation_constraints(zones = sep) %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # solve problem
  s <- solve(p)
  # calculations for tests
  m <- terra::as.matrix(category_layer(s), wide = TRUE)
  m[is.na(m)] <- 0
  touching <- 0
  for (i in seq_len(nrow(m))) {
    for (j in seq_len(ncol(m))) {
      if (m[i, j] == 0) next
      if (i < nrow(m) && m[i + 1, j] > 0 && m[i + 1, j] != m[i, j])
        touching <- touching + 1
      if (j < ncol(m) && m[i, j + 1] > 0 && m[i, j + 1] != m[i, j])
        touching <- touching + 1
    }
  }
  # tests
  expect_equal(touching, 0)
  expect_true(all(sort(unique(c(m[m > 0]))) == seq_len(n_r)))
})
