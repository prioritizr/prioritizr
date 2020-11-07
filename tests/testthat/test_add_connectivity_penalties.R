context("add_connectivity_penalties")

test_that("minimum set objective (compile, single zone)", {
  # make and compile problems
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_connectivity_penalties(5, data = boundary_matrix(sim_pu_raster))
  o <- compile(p)
  # run tests
  ## create variables for debugging
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  pu_indices <- p$planning_unit_indices()
  c_data <- boundary_matrix(p$data$cost)[pu_indices, pu_indices]
  c_data <- c_data * -5
  # connectivity weights for each planning unit
  c_weights <- Matrix::diag(c_data)
  # i,j,x matrix for planning unit boundaries
  Matrix::diag(c_data) <- 0
  c_data <- Matrix::drop0(c_data)
  c_data <- as(c_data, "dgTMatrix")
  # objectives for boundary decision variables
  c_obj <- o$obj()[n_pu + seq_len(length(c_data@i))]
  # lower bound for boundary decision variables
  c_lb <- o$lb()[n_pu + seq_len(length(c_data@i))]
  # upper bound for boundary decision variables
  c_ub <- o$ub()[n_pu + seq_len(length(c_data@i))]
  # vtype bound for boundary decision variables
  c_vtype <- o$vtype()[n_pu + seq_len(length(c_data@i))]
  # pu costs including total boundary
  pu_costs <- o$obj()[seq_len(n_pu)]
  # matrix labels
  c_col_labels <- o$col_ids()[n_pu + seq_len(length(c_data@i))]
  c_row_labels <- o$row_ids()[n_f + seq_len(length(c_data@i) * 2)]
  # sense for boundary decision constraints
  c_sense <- o$sense()[n_f + seq_len(length(c_data@i) * 2)]
  # rhs for boundary decision constraints
  c_rhs <- o$rhs()[n_f + seq_len(length(c_data@i) * 2)]
  ## check that constraints added correctly
  expect_equal(pu_costs, p$planning_unit_costs()[, 1] + c_weights)
  expect_equal(c_obj, c_data@x)
  expect_equal(c_lb, rep(0, length(c_data@i)))
  expect_equal(c_ub, rep(1, length(c_data@i)))
  expect_equal(c_vtype, rep("B", length(c_data@i)))
  expect_equal(c_col_labels, rep("c", length(c_data@i)))
  expect_equal(c_row_labels, rep(c("c1", "c2"), length(c_data@i)))
  expect_equal(c_sense, rep(c("<=", "<="), length(c_data@i)))
  expect_equal(c_rhs, rep(c(0, 0), length(c_data@i)))
  counter <- n_f
  for (i in seq_along(length(c_data@i))) {
    counter <- counter + 1
    expect_true(o$A()[counter, n_pu + i] == 1)
    expect_true(o$A()[counter, c_data@i[i] + 1] == -1)
    counter <- counter + 1
    expect_true(o$A()[counter, n_pu + i] == 1)
    expect_true(o$A()[counter, c_data@j[i] + 1] == -1)
  }
})

test_that("minimum set objective (solve, single zone)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(any_solvers_installed())
  # load data
  data(sim_pu_raster, sim_features)
  # create and solve problem
  p1 <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.2) %>%
        add_connectivity_penalties(1000,
                                   data = boundary_matrix(sim_pu_raster)) %>%
        add_binary_decisions() %>%
        add_default_solver(time_limit = 5, verbose = FALSE)
  s1_1 <- solve(p1)
  s1_2 <- solve(p1)
  p2 <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.2) %>%
        add_connectivity_penalties(-1000,
                                   data = boundary_matrix(sim_pu_raster)) %>%
        add_binary_decisions() %>%
        add_default_solver(time_limit = 5, verbose = FALSE)
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

test_that("invalid inputs (single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  c_matrix <- boundary_matrix(sim_pu_raster)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_error(add_connectivity_penalties(p, NA_real_, data = c_data))
  expect_error(add_connectivity_penalties(p, 1, 0, data = c_data))
  expect_error(add_connectivity_penalties(p, 5, data = c_data[, -1]))
  expect_error(add_connectivity_penalties(p, 5, data = c_data[-1, ]))
})

test_that("minimum set objective (compile, multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  sim_pu_zones_polygons <- sim_pu_zones_polygons[seq_len(20), ]
  # prepare data for problem
  cm <- boundary_matrix(sim_pu_zones_polygons)
  zm <- matrix(seq_len(9) * 0.1, ncol = 3)
  # make and compile problem
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
       add_connectivity_penalties(500, zm, cm) %>%
       add_binary_decisions()
  o <- compile(p)
  ## prepare data for tests
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  # prepare matrix
  c_data <- cm * -500
  c_weights <- rep(Matrix::diag(c_data), n_z) * rep(diag(zm), each = n_pu)
  Matrix::diag(c_data) <- 0
  c_data <- Matrix::drop0(c_data)
  c_data <- as(c_data, "dgTMatrix")
  c_penalties <- c()
  for (i in seq_len(n_z))
    for (j in seq_len(n_z))
      c_penalties <- c(c_penalties, c_data@x *zm[i, j])
  # objectives for connectivity decision variables
  c_obj <- o$obj()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  # lower bound for connectivity decision variables
  c_lb <- o$lb()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  # upper bound for connectivity decision variables
  c_ub <- o$ub()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  # vtype bound for connectivity decision variables
  c_vtype <- o$vtype()[(n_pu * n_z) + seq_len(length(c_data@i) * n_z * n_z)]
  # pu costs including connectivity penalties
  pu_costs <- o$obj()[seq_len(n_pu * n_z)]
  # matrix labels
  c_col_labels <- o$col_ids()[(n_pu * n_z) +
                              seq_len(length(c_data@i)  * n_z * n_z)]
  c_row_labels <- o$row_ids()[(n_f * n_z) + n_pu +
                               seq_len(length(c_data@i) * 2)]
  # sense for connectivity decision constraints
  c_sense <- o$sense()[(n_f * n_z) + n_pu +
                       seq_len(length(c_data@i) * 2)]
  # rhs for connectivity decision constraints
  c_rhs <- o$rhs()[(n_f * n_z) + n_pu +
                   seq_len(length(c_data@i) * 2)]
  # run tests
  expect_equal(pu_costs, c(p$planning_unit_costs()) + c_weights)
  expect_equal(c_obj, c_penalties)
  expect_equal(c_lb, rep(0, length(c_data@i) * n_z * n_z))
  expect_equal(c_ub, rep(1, length(c_data@i) * n_z * n_z))
  expect_equal(c_vtype, rep("B", length(c_data@i) * n_z * n_z))
  expect_equal(c_col_labels, rep("c", length(c_data@i) * n_z * n_z))
  expect_equal(c_row_labels, rep(c("c1", "c2"), length(c_data@i)))
  expect_equal(c_sense, rep(c("<=", "<="), length(c_data@i)))
  expect_equal(c_rhs, rep(c(0, 0), length(c_data@i)))
  counter <- (n_f * n_z) + n_pu
  counter2 <- 0
  for (i in seq_len(n_z)) {
    for (j in seq_len(n_z)) {
      for (k in seq_along(c_data@i)) {
        counter <- counter + 1
        counter2 <- counter2 + 1
        expect_true(o$A()[counter, (n_pu * n_z) + counter2] == 1)
        expect_true(o$A()[counter, ((i - 1) * n_pu) + c_data@i[k] + 1] == -1)
        counter <- counter + 1
        expect_true(o$A()[counter, (n_pu * n_z) + counter2] == 1)
        expect_true(o$A()[counter, ((j - 1) * n_pu) + c_data@j[k] + 1] == -1)
      }
    }
  }
})

test_that("minimum set objective (compile, array data, multiple zones)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  # prepare data for problem
  cm <- boundary_matrix(sim_pu_zones_polygons)
  zm <- matrix(seq_len(9) * 0.1, ncol = 3)
  ca <- array(0, dim = c(dim(cm), 3, 3))
  for (i in seq_len(3))
    for (j in seq_len(3))
      ca[, , i, j] <- as.matrix(cm * zm[i, j])
  # make and compile problems
  p1 <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
       add_connectivity_penalties(500, zm, cm) %>%
       add_binary_decisions()
  p2 <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
       add_connectivity_penalties(500, NULL, ca) %>%
       add_binary_decisions()
  o1 <- compile(p1)
  o2 <- compile(p2)
  # run tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$modelsense(), o2$modelsense())
  expect_equal(o1$A(), o2$A())
})

test_that("minimum set objective (solve, multiple zones)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(any_solvers_installed())
  # load data
  data(sim_pu_zones_stack, sim_features_zones)
  # make zones matrices
  zm <- matrix(-1, ncol = 3, nrow = 3)
  diag(zm) <- 0.1
  # make connectivity data
  cm <- adjacency_matrix(sim_pu_zones_stack)
  # create and solve problem
  s <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
       add_connectivity_penalties(5000, zm, cm) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0.15, verbose = FALSE) %>%
       solve()
  sc <- category_layer(s)
  # tests
  expect_is(s, "RasterStack")
  expect_true(all(na.omit(unique(raster::values(s))) %in% c(0, 1)))
  for (i in seq_len(3)) {
    # check that each zone forms a single contiguous block
    p <- rasterToPolygons(s[[i]] == 1, dissolve = TRUE)
    p <- p[p$layer == 1, ]
    p <- aggregate(p)
    expect_equal(length(p), 1)
    # check that each zone is not adjacent to another zone
    a <- raster::adjacent(sc, cells = Which(s[[i]] == 1, cells = TRUE),
                          pairs = FALSE)
    expect_true(all(sc[a] %in% c(0, i, NA)))
  }
})

test_that("minimum set objective (compile, Spatial and sf are identical)", {
  # load data
  data(sim_pu_zones_polygons, sim_features_zones)
  sim_pu_zones_polygons <- sim_pu_zones_polygons[seq_len(20), ]
  sim_sf <- sf::st_as_sf(sim_pu_zones_polygons)
  # prepare data for problem
  cm <- boundary_matrix(sim_pu_zones_polygons)
  zm <- matrix(seq_len(9) * 0.1, ncol = 3)
  # make problems
  p1 <- problem(sim_pu_zones_polygons, sim_features_zones,
                c("cost_1", "cost_2", "cost_3")) %>%
        add_min_set_objective() %>%
        add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
        add_connectivity_penalties(500, zm, cm) %>%
        add_binary_decisions()
  p2 <- problem(sim_sf, sim_features_zones,
                c("cost_1", "cost_2", "cost_3")) %>%
        add_min_set_objective() %>%
        add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
        add_connectivity_penalties(500, zm, cm) %>%
        add_binary_decisions()
  # compile problems
  o1 <- as.list(compile(p1))
  o2 <- as.list(compile(p2))
  # tests
  expect_equal(o1, o2)
})

test_that("invalid inputs (multiple zones)", {
  # load data
  data(sim_pu_zones_stack, sim_features_zones)
  # make zones matrices
  zm <- matrix(-1, ncol = 3, nrow = 3)
  diag(zm) <- 1
  # make connectivity data
  cm <- adjacency_matrix(sim_pu_zones_stack)
  ca <- array(1, dim = c(dim(cm), 3, 3))
  # create problem
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
        add_min_set_objective() %>%
        add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
        add_binary_decisions()
  # tests
  expect_error(add_connectivity_penalties(p, NA_real_, zm ,cm))
  expect_error(add_connectivity_penalties(p, Inf, zm ,cm))
  expect_error(add_connectivity_penalties(p, 1, zm[-1, ], cm))
  expect_error(add_connectivity_penalties(p, 1, zm[, -1], cm))
  expect_error(add_connectivity_penalties(p, 1, `[<-`(zm, 1, -2), cm))
  expect_error(add_connectivity_penalties(p, 1, `[<-`(zm, 1, 3), cm))
  expect_error(add_connectivity_penalties(p, 1, `[<-`(zm, 1, NA), cm))
  expect_error(add_connectivity_penalties(p, 1, zm, cm[-1, ]))
  expect_error(add_connectivity_penalties(p, 1, zm, cm[, -1]))
  expect_error(add_connectivity_penalties(p, 1, zm, cm[, -1]))
  expect_error(add_connectivity_penalties(p, 1, zm, ca))
  expect_error(add_connectivity_penalties(p, 1, data = ca))
  expect_error(add_connectivity_penalties(p, 1, NULL, ca[-1, , , ]))
  expect_error(add_connectivity_penalties(p, 1, NULL, ca[, -1, , ]))
  expect_error(add_connectivity_penalties(p, 1, NULL, ca[, , -1, ]))
  expect_error(add_connectivity_penalties(p, 1, NULL, ca[, , , -1]))
})
