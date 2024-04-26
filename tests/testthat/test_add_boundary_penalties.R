test_that("minimum set objective (compile, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(3, 0.5)
  o <- compile(p)
  # print
  suppressMessages(print(p))
  suppressMessages(summary(p))
  # create variables for debugging
  ## number of planning units
  n_pu <- p$number_of_planning_units()
  ## number of features
  n_f <- p$number_of_features()
  ## prepare boundary calculations
  b_data <- boundary_matrix(p$data$cost)
  b_data <- b_data[p$planning_unit_indices(), p$planning_unit_indices()]
  b_exposed <- c(
    Matrix::diag(b_data) - (Matrix::rowSums(b_data) - Matrix::diag(b_data))
  )
  b_total <- Matrix::diag(b_data)
  ## calculate scaled costs with total boundaries
  b_sc_costs <- 3 * ((b_total - b_exposed) + (b_exposed * 0.5))
  ## prepare scaled shared lengths
  Matrix::diag(b_data) <- 0
  b_data <- Matrix::tril(Matrix::drop0(b_data))
  b_data <- as_Matrix(b_data * 3, "dgTMatrix")
  ## objectives for boundary decision variables
  b_obj <- o$obj()[n_pu + seq_len(length(b_data@i))]
  ## lower bound for boundary decision variables
  b_lb <- o$lb()[n_pu + seq_len(length(b_data@i))]
  ## upper bound for boundary decision variables
  b_ub <- o$ub()[n_pu + seq_len(length(b_data@i))]
  ## vtype bound for boundary decision variables
  b_vtype <- o$vtype()[n_pu + seq_len(length(b_data@i))]
  ## pu costs including total boundary
  pu_costs <- o$obj()[seq_len(n_pu)]
  ## matrix labels
  b_col_labels <- o$col_ids()[n_pu + seq_len(length(b_data@i))]
  b_row_labels <- o$row_ids()[n_f + seq_len(length(b_data@i) * 2)]
  ## sense for boundary decision constraints
  b_sense <- o$sense()[n_f + seq_len(length(b_data@i) * 2)]
  ## rhs for boundary decision constraints
  b_rhs <- o$rhs()[n_f + seq_len(length(b_data@i) * 2)]
  # tests
  expect_true(all(b_col_labels == "b"))
  expect_equal(pu_costs, p$planning_unit_costs()[, 1] + b_sc_costs)
  expect_equal(b_obj, -2 * b_data@x)
  expect_true(all(b_lb == 0))
  expect_true(all(b_ub == 1))
  expect_true(all(b_vtype == "B"))
  expect_equal(b_row_labels, rep(c("b1", "b2"), length(b_data@i)))
  expect_equal(b_sense, rep(c("<=", "<="), length(b_data@i)))
  expect_equal(b_rhs, rep(c(0, 0), length(b_data@i)))
  counter <- n_f
  oA <- o$A()
  for (i in seq_along(b_data@i)) {
    counter <- counter + 1
    expect_true(oA[counter, n_pu + i] == 1)
    expect_true(oA[counter, b_data@i[i] + 1] == -1)
    counter <- counter + 1
    expect_true(oA[counter, n_pu + i] == 1)
    expect_true(oA[counter, b_data@j[i] + 1] == -1)
  }
})

test_that("alternative data formats (single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create boundary matrix data
  ## matrix format
  bm <- boundary_matrix(sim_pu_raster)
  ## data frame format
  bdf <- boundary_matrix(sim_pu_raster)
  ### N.B. here we convert diagonals to show exposed length, not total length
  ### this is because data frame format follows marxan conventions
  Matrix::diag(bdf) <- c(
    Matrix::diag(bdf) - (Matrix::rowSums(bdf) - Matrix::diag(bdf))
  )
  bdf <- matrix_to_triplet_dataframe(bdf)
  bdf <- setNames(bdf, c("id1", "id2", "boundary"))
  ## data frame format (reversed ids)
  bdf2 <- data.frame(id1 = bdf[[2]], id2 = bdf[[1]], boundary = bdf[[3]])
  # create problems
  p <-
    problem(sim_pu_raster, sim_features) %>%
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
  # tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$obj(), o3$obj())
  expect_equal(o1$obj(), o4$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_true(all(o1$A() == o3$A()))
  expect_true(all(o1$A() == o4$A()))
})

test_that("minimum set and shortfall objective (obj fun, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problems
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(10000, 1) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  s <- solve_fixed_seed(p)
  # calculations for tests
  obj_value <- unname(attr(s, "objective"))
  total_perim <- terra::perim(
    terra::as.polygons(terra::clamp(s, lower = 0.5, values = FALSE))
  )
  # tests
  expect_equal(
    terra::global(sim_pu_raster * s, "sum", na.rm = TRUE)[[1]] +
      (10000 * total_perim),
    obj_value,
    tolerance = 1e-6
  )
})

test_that("minimum set and shortfall objective (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate budget
  b <- terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.3
  # create problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(10000, 0.5) %>%
    add_default_solver(time_limit = 5, verbose = FALSE)
  s1_1 <- solve_fixed_seed(p1)
  s1_2 <- solve_fixed_seed(p1)
  p2 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(budget = b) %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(-10000000, 0.5) %>%
    add_default_solver(time_limit = 5, verbose = FALSE)
  expect_warning(s2_1 <- solve_fixed_seed(p2, force = TRUE))
  expect_warning(s2_2 <- solve_fixed_seed(p2, force = TRUE))
  # tests
  expect_inherits(s1_1, "SpatRaster")
  expect_inherits(s1_2, "SpatRaster")
  expect_true(all_binary(terra::values(s1_1)))
  expect_true(is_single_patch_raster(s1_1))
  expect_equal(terra::values(s1_1), terra::values(s1_2))
  expect_inherits(s2_1, "SpatRaster")
  expect_inherits(s2_2, "SpatRaster")
  expect_true(all_binary(terra::values(s2_1)))
  expect_true(is_checkerboard_raster(s2_1))
  expect_equal(terra::values(s2_1), terra::values(s2_2))
})

test_that("minimum set objective (compile, multiple zones)", {
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create zones data
  penalty <- 5
  p_zones <- matrix(0, ncol = 3, nrow = 3)
  diag(p_zones) <- c(0.7, 0.8, 0.9)
  p_zones[upper.tri(p_zones)] <- c(0.1, 0.2, 0.3)
  p_zones[lower.tri(p_zones)] <- p_zones[upper.tri(p_zones)]
  p_edge_factor <- seq(0.1, 0.1 * 3, 0.1)
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty, p_edge_factor, p_zones)
  o <- compile(p)
  # create variables for tests
  ## number of planning units
  n_pu <- p$number_of_planning_units()
  ## number of features
  n_f <- p$number_of_features()
  ## number of zones
  n_z <- p$number_of_zones()
  ## generate boundary data
  pu_indices <- p$planning_unit_indices()
  b_matrix <- boundary_matrix(p$data$cost)[pu_indices, pu_indices]
  b_exposed <- c(
    Matrix::diag(b_matrix) -
      (Matrix::rowSums(b_matrix) - Matrix::diag(b_matrix))
  )
  b_total <- Matrix::diag(b_matrix)
  ## create matrix with the scaled boundary components
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
    ## extract data
    curr_m <- b_matrix
    ## prepare scaled shared lengths
    Matrix::diag(curr_m) <- 0
    curr_m <- Matrix::tril(Matrix::drop0(curr_m))
    curr_m <- as_Matrix(
      curr_m * penalty * p_zones[indices[i, 1], indices[i, 2]],
      "dgTMatrix"
    )
    ## if z1 == z2, then store scaled costs in matrix diagonal for later
    if (indices[i, 1] == indices[i, 2]) {
      curr_sc_costs <- c(
        penalty * p_zones[indices[i, 1], indices[i, 2]] *
        ((b_total - b_exposed) + (b_exposed * p_edge_factor[indices[i, 1]]))
      )
      Matrix::diag(curr_m) <- curr_sc_costs
    }
    ## store result
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
    ## extract zone indices
    z1 <- indices[z, 1]
    z2 <- indices[z, 2]
    ## extract data
    curr_sc_costs <- Matrix::diag(scb_list[[z]])
    curr_m <- as.matrix(scb_list[[z]])
    Matrix::diag(curr_m) <- 0
    ## add boundary penalty costs
    if (z1 == z2) {
      pos1 <- ((z1 - 1) * n_pu) + seq_along(curr_sc_costs)
      b_unit_additions[pos1] <- b_unit_additions[pos1] + curr_sc_costs
    }
    ## add constraints for shared edges
    curr_m_indices <- which(
      lower.tri(curr_m, diag = FALSE) & (curr_m != 0), arr.ind = TRUE
    )
    for (i in seq_len(nrow(curr_m_indices))) {
      if (z1 == z2) {
        # if boundary is for two different pus in the same zone then
        # add the boundary to the two different pus
        pos1 <- ((z1 - 1) * n_pu) + curr_m_indices[i, 1]
        pos2 <- ((z1 - 1) * n_pu) + curr_m_indices[i, 2]
        curr_penalty <- curr_m[curr_m_indices[i, 1], curr_m_indices[i, 2]]
        b_vars_i <- c(b_vars_i, pos1)
        b_vars_j <- c(b_vars_j, pos2)
        b_vars_b <- c(b_vars_b, curr_penalty)
        b_vars_label <- c(
          b_vars_label,
          paste0(
            "p", curr_m_indices[i, 1], "z", z1, "_p",
            curr_m_indices[i, 2], "z", z1
          )
        )
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
        b_vars_label <- c(
          b_vars_label,
          paste0(
            "p", curr_m_indices[i, 1], "z", z1, "_p",
            curr_m_indices[i, 2], "z", z2
          )
        )
        b_vars_i <- c(b_vars_i, pos3)
        b_vars_j <- c(b_vars_j, pos4)
        b_vars_b <- c(b_vars_b, curr_penalty)
        b_vars_label <- c(
          b_vars_label,
          paste0(
            "p", curr_m_indices[i, 1], "z", z2, "_p",
            curr_m_indices[i, 2], "z", z1
          )
        )
      }
    }
  }
  ## calculate number of penalty variables
  n_z_p <- length(b_vars_b)
  ## calculate objective function
  correct_obj <- c(c(p$planning_unit_costs()) + b_unit_additions, -2 * b_vars_b)
  names(correct_obj) <- c(
    paste0("p", rep(seq_len(n_pu), n_z), "z", rep(seq_len(n_z), each = n_pu)),
    b_vars_label
  )
  # tests
  expect_equal(o$obj(), unname(correct_obj))
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + n_z_p))
  expect_equal(o$ub(), rep(1, (n_pu * n_z) + n_z_p))
  expect_equal(o$vtype(), rep("B", (n_pu * n_z) + n_z_p))
  expect_equal(
    o$row_ids(),
    c(
      rep("spp_target", n_f * n_z),
      rep("pu_zone", n_pu),
      rep(c("b1", "b2"), n_z_p)
    )
  )
  expect_equal(
    o$col_ids(),
    c(rep("pu", n_pu * n_z), rep("b", n_z_p))
  )
  expect_equal(
    o$sense(),
    c(rep(">=", n_f * n_z), rep("<=", n_pu), rep(c("<=", "<="), n_z_p))
  )
  expect_equal(
    o$rhs(),
    c(rep(0.1, n_f * n_z), rep(1, n_pu), rep(c(0, 0), n_z_p))
  )
  ## check model matrix is defined correctly
  oA <- o$A()
  ## targets
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_f)) {
      curr_amount <- rep(0, (n_pu * n_z) + n_z_p)
      curr_amount[((z - 1) * n_pu) + seq_len(n_pu)] <-
        p$data$rij_matrix[[z]][i, ]
      expect_equal(oA[((z - 1) * n_f) + i, ], curr_amount)
    }
  }
  ## zone constraints
  counter <- n_z * n_f
  for (j in seq_len(n_pu)) {
    counter <- counter + 1
    curr_vec <- rep(0, (n_pu * n_z) + n_z_p)
    for (z in seq_len(n_z))
      curr_vec[((z - 1) * n_pu) + j] <- 1
    expect_equal(oA[counter, ], curr_vec)
  }
  ## penalty variable constraints
  for (i in seq_along(n_z_p)) {
    counter <- counter + 1
    curr_vec <- rep(0, (n_pu * n_z) + n_z_p)
    curr_vec[b_vars_i[i]] <- -1
    curr_vec[(n_z * n_pu) + i] <- 1
    expect_equal(oA[counter, ], curr_vec)
    counter <- counter + 1
    curr_vec <- rep(0, (n_pu * n_z) + n_z_p)
    curr_vec[b_vars_j[i]] <- -1
    curr_vec[(n_z * n_pu) + i] <- 1
    expect_equal(oA[counter, ], curr_vec)
  }
})

test_that("alternative data formats (multiple zones)", {
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # calculate zones data
  p_zones <- matrix(0, ncol = 3, nrow = 3)
  diag(p_zones) <- c(0.7, 0.8, 0.9)
  p_zones[upper.tri(p_zones)] <- c(0.1, 0.2, 0.3)
  p_zones[lower.tri(p_zones)] <- p_zones[upper.tri(p_zones)]
  p_edge_factor <- seq(0.1, 0.1 * 3, 0.1)
  # calculate boundary matrix data
  ## matrix format
  bm <- boundary_matrix(sim_zones_pu_polygons)
  ## data frame format
  bdf <- boundary_matrix(sim_zones_pu_polygons)
  ### N.B. here we convert diagonals to show exposed length, not total length
  ### this is because data frame format follows marxan conventions
  Matrix::diag(bdf) <- c(
    Matrix::diag(bdf) - (Matrix::rowSums(bdf) - Matrix::diag(bdf))
  )
  bdf <- matrix_to_triplet_dataframe(bdf)
  bdf <- setNames(bdf, c("id1", "id2", "boundary"))
  ## data frame format (reversed ids)
  bdf2 <- data.frame(id1 = bdf[[2]], id2 = bdf[[1]], boundary = bdf[[3]])
  # create problems
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
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
  # tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$obj(), o3$obj())
  expect_equal(o1$obj(), o4$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_true(all(o1$A() == o3$A()))
  expect_true(all(o1$A() == o4$A()))
})

test_that("minimum set objective (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate zones data
  m <- matrix(
    c(
      1, 0, 0,
      0, 1, 0,
      0, 0, 1
    ),
    byrow = TRUE, ncol = 3
  )
  # create baseline problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.025, ncol = 3, nrow = 5)) %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # create and solve problems
  s1 <-
    p %>%
    add_boundary_penalties(300, rep(0.5, 3),  m) %>%
    solve_fixed_seed()
  s2 <-
    p %>%
    add_boundary_penalties(-300, rep(0.5, 3),  m) %>%
    solve_fixed_seed()
  # tests
  expect_inherits(s1, "SpatRaster")
  expect_true(all_binary(s1[[1]]))
  expect_true(all_binary(s1[[2]]))
  expect_true(all_binary(s1[[3]]))
  expect_inherits(s2, "SpatRaster")
  expect_true(all_binary(s2[[1]]))
  expect_true(all_binary(s2[[2]]))
  expect_true(all_binary(s2[[3]]))
  expect_true(is_checkerboard_raster(s2[[1]]))
  expect_true(is_checkerboard_raster(s2[[2]]))
  expect_true(is_checkerboard_raster(s2[[3]]))
})

test_that("invalid inputs (single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # tests
  expect_tidy_error(add_boundary_penalties(p, 9, 1.5))
  expect_tidy_error(add_boundary_penalties(p, 9, -0.5))
  expect_tidy_error(add_boundary_penalties(p, 9, NA))
  expect_tidy_error(add_boundary_penalties(p, NA, 0.5))
})

test_that("invalid inputs (multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
    add_binary_decisions()
  # create zones data
  p_zones <- diag(3)
  p_zones[upper.tri(p_zones)] <- 0.1
  p_zones[lower.tri(p_zones)] <- p_zones[upper.tri(p_zones)]
  # tests
  expect_tidy_error(add_boundary_penalties(p, c(0.1, 0.1)))
  expect_tidy_error(add_boundary_penalties(p, c(0.1, 0.1, NA)))
  expect_tidy_error(add_boundary_penalties(p, c(0.1, 0.1, 5)))
  expect_tidy_error(add_boundary_penalties(p, c(0.1, 0.1, -5)))
  expect_tidy_error(add_boundary_penalties(p, zones = p_zones[-1, ]))
  expect_tidy_error(add_boundary_penalties(p, zones = p_zones[, -1]))
  expect_tidy_error(add_boundary_penalties(p, zones = p_zones * runif(9)))
  expect_tidy_error(add_boundary_penalties(p, zones = p_zones + 5))
  expect_tidy_error(add_boundary_penalties(p, zones = p_zones - 5))
  expect_tidy_error(add_boundary_penalties(p, zones = p_zones + c(1, 2, NA)))
})

test_that("throws warnings", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  ## create boundary matrix with weird values
  bm <- boundary_matrix(sim_pu_raster)
  Matrix::diag(bm) <- Matrix::diag(bm) * 0.5
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  ## tests
  expect_warning(
    add_boundary_penalties(p, 5, data = bm),
    "unexpected"
  )
})

test_that("tas_pu works", {
  skip_if_not_installed("prioritizrdata", minimum_version = "0.3.0")
  # import data
  tas_pu <- prioritizrdata::get_tas_pu()
  tas_features <- prioritizrdata::get_tas_features()
  # create boundary matrix
  bm <- boundary_matrix(tas_pu)
  # create problem
  expect_silent({
    problem(tas_pu, tas_features, cost = "cost") %>%
    add_boundary_penalties(0.001, data = bm)
  })
  expect_silent({
    problem(tas_pu, tas_features, cost = "cost") %>%
    add_boundary_penalties(0.001)
  })
})
