context("add_connectivity_penalties")

test_that("minimum set objective (compile, symmetric, single zone)", {
  # make and compile problems
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  p1 <- p %>% add_boundary_penalties(1, 1)
  p2 <- p %>% add_connectivity_penalties(1, boundary_matrix(sim_pu_raster))
  o1 <- compile(p1)
  o2 <- compile(p2)
  # run tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$A(), o2$A())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(gsub("b", "c", o1$col_ids(), fixed = TRUE), o2$col_ids())
  expect_equal(gsub("b", "c", o1$row_ids(), fixed = TRUE), o2$row_ids())
})

test_that("minimum set objective (solve, symmetric, zone)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # load data
  data(sim_pu_raster, sim_features)
  # create and solve problem
  s <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_connectivity_penalties(1, boundary_matrix(sim_pu_raster)) %>%
       add_default_solver(time_limit = 5) %>%
       solve()
  # tests
  expect_is(s, "RasterLayer")
  expect_true(all(na.omit(unique(raster::values(s))) %in% c(0, 1)))
})

test_that("minimum set objective (compile, asymmetric, single zone)", {
})

test_that("minimum set objective (solve, asymmetric, single zone)", {
})

test_that("invalid inputs (single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  c_matrix <- boundary_matrix(sim_pu_raster)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_error(add_connectivity_penalties(p, c_data, NA_real_))
  expect_error(add_connectivity_penalties(p, NA_real_, 0.5))
  expect_error(add_connectivity_penalties(p, c_data[-1, ], NA_real_))
})

test_that("minimum set objective (compile, symmetric, multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  sim_pu_zones_polygons <- sim_pu_zones_polygons[1:3, ]
  # prepare connectivity matrix that is equivalent to a boundary matrix
  b_penalties <- matrix(0, ncol = 3, nrow = 3)
  diag(b_penalties) <- 10 + seq_len(3)
  b_penalties[upper.tri(b_penalties)] <- seq_len(3)
  b_penalties[lower.tri(b_penalties)] <- b_penalties[upper.tri(b_penalties)]
  b_matrix <- boundary_matrix(sim_pu_zones_polygons)
  c_matrix <- array(0, dim = c(rep(length(sim_pu_zones_polygons), 2),
                               rep(3, 2)))
  for (z1 in seq_len(3)) {
    for (z2 in seq_len(3)) {
      c_matrix[, , z1, z2] <- as.matrix(b_penalties[z1, z2] * b_matrix)
      if (z1 != z2) {
        diag(c_matrix[, , z1, z2]) <- 0
      }
    }
  }
  # make and compile problems
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
       add_binary_decisions()
  p1 <- p %>% add_boundary_penalties(b_penalties, rep(1, 3))
  p2 <- p %>% add_connectivity_penalties(1, c_matrix)
  o1 <- compile(p1)
  o2 <- compile(p2)
  # run tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$A(), o2$A())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(gsub("b", "c", o1$col_ids(), fixed = TRUE), o2$col_ids())
  expect_equal(gsub("b", "c", o1$row_ids(), fixed = TRUE), o2$row_ids())
})

test_that("minimum set objective (solve, symmetric, multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  # prepare connectivity matrix that is equivalent to a boundary matrix
  b_penalties <- matrix(0, ncol = 3, nrow = 3)
  diag(b_penalties) <- 10 + seq_len(3)
  b_penalties[upper.tri(b_penalties)] <- seq_len(3)
  b_penalties[lower.tri(b_penalties)] <- b_penalties[upper.tri(b_penalties)]
  b_matrix <- boundary_matrix(sim_pu_zones_polygons)
  c_matrix <- array(0, dim = c(rep(length(sim_pu_zones_polygons), 2),
                               rep(3, 2)))
  for (z1 in seq_len(3)) {
    for (z2 in seq_len(3)) {
      c_matrix[, , z1, z2] <- as.matrix(b_penalties[z1, z2] * b_matrix)
      if (z1 != z2) {
        c_matrix[seq_len(length(sim_pu_zones_polygons)),
                 seq_len(length(sim_pu_zones_polygons)), z1, z2] <- 0
      }
    }
  }
  # create and solve problem
  s <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
       add_binary_decisions() %>%
       add_connectivity_penalties(1, c_matrix) %>%
       solve()
  # tests
  expect_is(s, "SpatialPolygonsDataFrame")
  expect_true(all(sim_pu_zones_polygons$solution_1_zone_1 %in% c(0, 1)))
  expect_true(all(sim_pu_zones_polygons$solution_1_zone_2 %in% c(0, 1)))
  expect_true(all(sim_pu_zones_polygons$solution_1_zone_3 %in% c(0, 1)))
})

test_that("minimum set objective (compile, asymmetric, multiple zones)", {
})

test_that("minimum set objective (solve, asymmetric, multiple zones)", {
})

test_that("invalid inputs (multiple zones)", {
})
