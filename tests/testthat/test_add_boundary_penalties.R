context("add_boundary_penalties")

test_that("minimum set objective (compile, single zone)", {
  ## make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_boundary_penalties(3, 0.5)
  o <- compile(p)
  ## create variables for debugging
  # number of planning units
  n_pu <- p$number_of_planning_units()
  # number of features
  n_f <- p$number_of_features()
  pu_indices <- p$planning_unit_indices()
  b_data <- boundary_matrix(p$data$cost)[pu_indices, pu_indices]
  b_data <- b_data * 3
  Matrix::diag(b_data) <- Matrix::diag(b_data) * 0.5
  # total boundary for each planning unit
  b_total_boundary <- colSums(b_data)
  class(b_data) <- "dgCMatrix"
  Matrix::diag(b_data) <- 0
  # i,j,x matrix for planning unit boundaries
  b_data <- as(b_data, "dgTMatrix")
  b_data <- Matrix::sparseMatrix(i = b_data@i[b_data@x != 0],
    j = b_data@j[b_data@x != 0], x = b_data@x[b_data@x != 0],
    giveCsparse = FALSE, index1 = FALSE)
  # objectives for boundary decision variables
  b_obj <- o$obj()[n_pu + seq_len(length(b_data@i))]
  # lower bound for boundary decision variables
  b_lb <- o$lb()[n_pu + seq_len(length(b_data@i))]
  # upper bound for boundary decision variables
  b_ub <- o$ub()[n_pu + seq_len(length(b_data@i))]
  # vtype bound for boundary decision variables
  b_vtype <- o$vtype()[n_pu + seq_len(length(b_data@i))]
  # pu costs including total boundary
  pu_costs <- o$obj()[seq_len(n_pu)]
  # matrix labels
  b_col_labels <- o$col_ids()[n_pu + seq_len(length(b_data@i))]
  b_row_labels <- o$row_ids()[n_f + seq_len(length(b_data@i) * 2)]
  # sense for boundary decision constraints
  b_sense <- o$sense()[n_f + seq_len(length(b_data@i) * 2)]
  # rhs for boundary decision constraints
  b_rhs <- o$rhs()[n_f + seq_len(length(b_data@i) * 2)]
  ## check that constraints added correctly
  expect_true(all(b_col_labels == "b"))
  expect_equal(pu_costs, p$planning_unit_costs()[, 1] + b_total_boundary)
  expect_equal(b_obj, -2 * b_data@x)
  expect_true(all(b_lb == 0))
  expect_true(all(b_ub == 1))
  expect_true(all(b_vtype == "B"))
  expect_equal(b_row_labels, rep(c("b1", "b2"), length(b_data@i)))
  expect_equal(b_sense, rep(c("<=", "<="), length(b_data@i)))
  expect_equal(b_rhs, rep(c(0, 0), length(b_data@i)))
  counter <- n_f
  for (i in seq_along(length(b_data@i))) {
    counter <- counter + 1
    expect_true(o$A()[counter, n_pu + i] == 1)
    expect_true(o$A()[counter, b_data@j[i] + 1] == -1)
    counter <- counter + 1
    expect_true(o$A()[counter, n_pu + i] == 1)
    expect_true(o$A()[counter, b_data@i[i] + 1] == -1)
  }
})

test_that("minimum set objective (compile, data, single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  bm <- boundary_matrix(sim_pu_raster)
  bdf <- matrix_to_triplet_dataframe(boundary_matrix(sim_pu_raster)) %>%
         setNames(c("id1", "id2", "boundary"))
  bdf2 <- data.frame(id1 = bdf[[2]], id2 = bdf[[1]], boundary = bdf[[3]])
  # create problems
  p <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions()
  p1 <- p %>% add_boundary_penalties(2, 0.5)
  p2 <- p %>% add_boundary_penalties(2, 0.5, data = bm)
  p3 <- p %>% add_boundary_penalties(2, 0.5, data = bdf)
  p4 <- p %>% add_boundary_penalties(2, 0.5, data = bdf2)
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  o3 <- compile(p3)
  o4 <- compile(p4)
  # compare problems
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$obj(), o3$obj())
  expect_equal(o1$obj(), o4$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_true(all(o1$A() == o3$A()))
  expect_true(all(o1$A() == o4$A()))
})

test_that("minimum set objective (solve, single zone)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # check that solution is feasible
  data(sim_pu_raster, sim_features)
  p1 <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions() %>%
        add_boundary_penalties(10000, 0.5) %>%
        add_default_solver(time_limit = 5)
  s1_1 <- solve(p1)
  s1_2 <- solve(p1)
  p2 <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions() %>%
        add_boundary_penalties(-10000, 0.5) %>%
        add_default_solver(time_limit = 5)
  s2_1 <- solve(p2)
  s2_2 <- solve(p2)
  # tests
  expect_is(s1_1, "RasterLayer")
  expect_is(s1_2, "RasterLayer")
  expect_true(all(na.omit(unique(raster::values(s1_1))) %in% c(0, 1)))
  expect_equal(sum(raster::rasterToPolygons(s1_1, dissolve = TRUE)$layer == 1), 1)
  expect_equal(raster::values(s1_1), raster::values(s1_2))
  expect_is(s2_1, "RasterLayer")
  expect_is(s2_2, "RasterLayer")
  expect_true(all(na.omit(unique(raster::values(s2_1))) %in% c(0, 1)))
  s2_adj <- raster::adjacent(s2_1, raster::Which(s2_1 == 1, cells = TRUE),
                             pairs = FALSE)
  expect_true(all(s2_1[s2_adj] %in% c(0, NA)))
  expect_equal(raster::values(s2_1), raster::values(s2_2))
})

test_that("minimum set objective (compile, multiple zones)", {
  ## make data
  data(sim_pu_zones_polygons, sim_features_zones)
  penalty <- 5
  p_zones <- matrix(0, ncol = 3, nrow = 3)
  diag(p_zones) <- c(0.7, 0.8, 0.9)
  p_zones[upper.tri(p_zones)] <- c(0.1, 0.2, 0.3)
  p_zones[lower.tri(p_zones)] <- p_zones[upper.tri(p_zones)]
  p_edge_factor <- seq(0.1, 0.1 * 3, 0.1)
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
       add_binary_decisions() %>%
       add_boundary_penalties(penalty, p_edge_factor, p_zones)
  o <- compile(p)
  ## create variables for debugging
  # number of planning units
  n_pu <- p$number_of_planning_units()
  # number of features
  n_f <- p$number_of_features()
  # number of zones
  n_z <- p$number_of_zones()
  # generate boundary data
  pu_indices <- p$planning_unit_indices()
  b_matrix <- boundary_matrix(p$data$cost)[pu_indices, pu_indices]
  # create matrix with the scaled boundary components
  zone_cmbs <- matrix("", ncol = n_z, nrow = n_z)
  diag(zone_cmbs) <- paste0("z", seq_len(n_z), "_z", seq_len(n_z))
  ind <- which(upper.tri(zone_cmbs), arr.ind = TRUE)
  zone_cmbs[upper.tri(zone_cmbs)] <- paste0("z", ind[, 1], "_z", ind[, 2])
  ind <- which(lower.tri(zone_cmbs), arr.ind = TRUE)
  zone_cmbs[lower.tri(zone_cmbs)] <- paste0("z", ind[, 1], "_z", ind[, 2])
  zone_cmbs[lower.tri(zone_cmbs)] <- ""
  indices <- matrix(numeric(0), ncol = 2)
  for (i in seq_len(n_z))
    for (j in seq(i, n_z))
      indices <- rbind(indices, c(i, j))
  # create list of matrices
  scb_list <- list()
  for (i in seq_len(nrow(indices))) {
    curr_m <- b_matrix
    curr_m <- curr_m * penalty * p_zones[indices[i, 1], indices[i, 2]]
    if (indices[i, 1] == indices[i, 2]) {
      Matrix::diag(curr_m) <- Matrix::diag(curr_m) *
                              p_edge_factor[indices[i, 1]]
    } else {
      Matrix::diag(curr_m) <- 0
    }
    scb_list[[i]] <- curr_m
  }
  names(scb_list) <- paste0("z", indices[, 1], "_z", indices[, 2])
  # planning unit scaled additional costs
  b_unit_additions <- rep(0, n_pu * n_z)
  b_vars_i <- c()
  b_vars_j <- c()
  b_vars_b <- c()
  b_vars_label <- c()
  for (z in seq_along(scb_list)) {
    z1 <- indices[z, 1]
    z2 <- indices[z, 2]
    curr_m <- as.matrix(scb_list[[z]])
    curr_m_indices <- which(lower.tri(curr_m, diag = TRUE) &
                            (curr_m != 0), arr.ind = TRUE)
    for (i in seq_len(nrow(curr_m_indices))) {
      if ((curr_m_indices[i, 1] == curr_m_indices[i, 2]) &&
          (z1 == z2)) {
        # if boundary is for the same pu in the same zone then just add
        # it to the cost of the planning unit
        pos1 <- ((z1 - 1) * n_pu) + curr_m_indices[i, 1]
        curr_penalty <- curr_m[curr_m_indices[i, 1], curr_m_indices[i, 2]]
        b_unit_additions[pos1] <- b_unit_additions[pos1] + curr_penalty
      } else if (z1 == z2) {
        # if boundary is for two different pus in the same zone then
        # add the boundary to the two different pus
        pos1 <- ((z1 - 1) * n_pu) + curr_m_indices[i, 1]
        pos2 <- ((z1 - 1) * n_pu) + curr_m_indices[i, 2]
        curr_penalty <- curr_m[curr_m_indices[i, 1], curr_m_indices[i, 2]]
        b_unit_additions[pos1] <- b_unit_additions[pos1] + curr_penalty
        b_unit_additions[pos2] <- b_unit_additions[pos2] + curr_penalty
        b_vars_i <- c(b_vars_i, pos1)
        b_vars_j <- c(b_vars_j, pos2)
        b_vars_b <- c(b_vars_b, curr_penalty)
        b_vars_label <- c(b_vars_label,
                          paste0("p", curr_m_indices[i, 1], "z", z1, "_p",
                                 curr_m_indices[i, 2], "z", z1))
      } else {
        # if the boundary is for two different pus in two different zones
        # then add the boundary to the different pus in the different zones
        pos1 <- ((z1 - 1) * n_pu) + curr_m_indices[i, 1]
        pos2 <- ((z2 - 1) * n_pu) + curr_m_indices[i, 2]
        pos3 <- ((z2 - 1) * n_pu) + curr_m_indices[i, 1]
        pos4 <- ((z1 - 1) * n_pu) + curr_m_indices[i, 2]
        curr_penalty <- curr_m[curr_m_indices[i, 1], curr_m_indices[i, 2]]
        b_vars_i <- c(b_vars_i, pos1)
        b_vars_j <- c(b_vars_j, pos2)
        b_vars_b <- c(b_vars_b, curr_penalty)
        b_vars_label <- c(b_vars_label,
                          paste0("p", curr_m_indices[i, 1], "z", z1, "_p",
                                 curr_m_indices[i, 2], "z", z2))
        b_vars_i <- c(b_vars_i, pos3)
        b_vars_j <- c(b_vars_j, pos4)
        b_vars_b <- c(b_vars_b, curr_penalty)
        b_vars_label <- c(b_vars_label,
                          paste0("p", curr_m_indices[i, 1], "z", z2, "_p",
                                 curr_m_indices[i, 2], "z", z1))
      }
    }
  }
  # calculate number of penalty variables
  n_z_p <- length(b_vars_b)
  ## check that constraints added correctly
  correct_obj <- c(c(p$planning_unit_costs()) + b_unit_additions, -2 * b_vars_b)
  names(correct_obj) <- c(paste0("p", rep(seq_len(n_pu), n_z),
                                 "z", rep(seq_len(n_z), each = n_pu)),
                           b_vars_label)
  expect_equivalent(o$obj(), correct_obj)
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + n_z_p))
  expect_equal(o$ub(), rep(1, (n_pu * n_z) + n_z_p))
  expect_equal(o$vtype(), rep("B", (n_pu * n_z) + n_z_p))
  expect_equal(o$row_ids(), c(rep("spp_target", n_f * n_z),
                               rep("pu_zone", n_pu),
                               rep(c("b1", "b2"), n_z_p)))
  expect_equal(o$col_ids(), c(rep("pu", n_pu * n_z),
                               rep("b", n_z_p)))
  expect_equal(o$sense(), c(rep(">=", n_f * n_z),
                            rep("<=", n_pu),
                            rep(c("<=", "<="), n_z_p)))
  expect_equal(o$rhs(), c(rep(0.1, n_f * n_z), rep(1, n_pu),
                          rep(c(0, 0), n_z_p)))
  # check model matrix is defined correctly
  ## targets
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_f)) {
      curr_amount <- rep(0, (n_pu * n_z) + n_z_p)
      curr_amount[((z - 1) * n_pu) + seq_len(n_pu)] <- p$data$rij[[z]][i, ]
      expect_equal(o$A()[((z - 1) * n_f) + i, ], curr_amount)
    }
  }
  ## zone constraints
  counter <- n_z * n_f
  for (j in seq_len(n_pu)) {
    counter <- counter + 1
    curr_vec <- rep(0, (n_pu * n_z) + n_z_p)
    for (z in seq_len(n_z))
      curr_vec[((z - 1) * n_pu) + j] <- 1
    expect_equal(o$A()[counter, ], curr_vec)
  }
  ## penalty variable constraints
  for (i in seq_along(n_z_p)) {
    counter <- counter + 1
    curr_vec <- rep(0, (n_pu * n_z) + n_z_p)
    curr_vec[b_vars_i[i]] <- -1
    curr_vec[(n_z * n_pu) + i] <- 1
    expect_equal(o$A()[counter, ], curr_vec)
    counter <- counter + 1
    curr_vec <- rep(0, (n_pu * n_z) + n_z_p)
    curr_vec[b_vars_j[i]] <- -1
    curr_vec[(n_z * n_pu) + i] <- 1
    expect_equal(o$A()[counter, ], curr_vec)
  }
})

test_that("minimum set objective (compile, data, multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  p_zones <- matrix(0, ncol = 3, nrow = 3)
  diag(p_zones) <- c(0.7, 0.8, 0.9)
  p_zones[upper.tri(p_zones)] <- c(0.1, 0.2, 0.3)
  p_zones[lower.tri(p_zones)] <- p_zones[upper.tri(p_zones)]
  p_edge_factor <- seq(0.1, 0.1 * 3, 0.1)
  bm <- boundary_matrix(sim_pu_zones_polygons)
  bdf <- matrix_to_triplet_dataframe(boundary_matrix(sim_pu_zones_polygons)) %>%
         setNames(c("id1", "id2", "boundary"))
  bdf2 <- data.frame(id1 = bdf[[2]], id2 = bdf[[1]], boundary = bdf[[3]])
  # create problems
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
        add_min_set_objective() %>%
        add_absolute_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
        add_binary_decisions()
  p1 <- p %>% add_boundary_penalties(5, p_edge_factor, p_zones)
  p2 <- p %>% add_boundary_penalties(5, p_edge_factor, p_zones, data = bm)
  p3 <- p %>% add_boundary_penalties(5, p_edge_factor, p_zones, data = bdf)
  p4 <- p %>% add_boundary_penalties(5, p_edge_factor, p_zones, data = bdf2)
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  o3 <- compile(p3)
  o4 <- compile(p4)
  # compare problems
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$obj(), o3$obj())
  expect_equal(o1$obj(), o4$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_true(all(o1$A() == o3$A()))
  expect_true(all(o1$A() == o4$A()))
})

test_that("minimum set objective (solve, multiple zones)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  p_zones <- diag(3)
  p_zones[upper.tri(p_zones)] <- 0.1
  p_zones[lower.tri(p_zones)] <- p_zones[upper.tri(p_zones)]
  p_edge_factor <- rep(0.5, 3)
  # create and solve problem
  s <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
       add_binary_decisions() %>%
       add_boundary_penalties(5, p_edge_factor,  p_zones) %>%
       solve()
  # check that solution forms a single cluster
  expect_is(s, "SpatialPolygonsDataFrame")
  expect_true(all(s$solution_1_zone_1 %in% c(0, 1, NA)))
  expect_true(all(s$solution_1_zone_2 %in% c(0, 1, NA)))
  expect_true(all(s$solution_1_zone_3 %in% c(0, 1, NA)))
})

test_that("invalid inputs (single zone)", {
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_error(add_boundary_penalties(p, 9, 1.5))
  expect_error(add_boundary_penalties(p, 9, -0.5))
  expect_error(add_boundary_penalties(p, 9, NA))
  expect_error(add_boundary_penalties(p, NA, 0.5))
})

test_that("invalid inputs (multiple zones)", {
  data(sim_pu_zones_stack, sim_features_zones)
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
       add_binary_decisions()
  p_zones <- diag(3)
  p_zones[upper.tri(p_zones)] <- 0.1
  p_zones[lower.tri(p_zones)] <- p_zones[upper.tri(p_zones)]
  expect_error(add_boundary_penalties(p, c(0.1, 0.1)))
  expect_error(add_boundary_penalties(p, c(0.1, 0.1, NA)))
  expect_error(add_boundary_penalties(p, c(0.1, 0.1, 5)))
  expect_error(add_boundary_penalties(p, c(0.1, 0.1, -5)))
  expect_error(add_boundary_penalties(p, zones = p_zones[-1, ]))
  expect_error(add_boundary_penalties(p, zones = p_zones[, -1]))
  expect_error(add_boundary_penalties(p, zones = p_zones * runif(9)))
  expect_error(add_boundary_penalties(p, zones = p_zones + 5))
  expect_error(add_boundary_penalties(p, zones = p_zones - 5))
  expect_error(add_boundary_penalties(p, zones = p_zones + c(1, 2, NA)))
})
