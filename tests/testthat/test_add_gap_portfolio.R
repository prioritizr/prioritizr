context("add_gap_portfolio")

test_that("compile", {
  skip_if_not_installed("gurobi")
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_gap_portfolio(number_solutions = 5, gap = 1) %>%
       add_gurobi_solver(gap = 0.2, verbose = FALSE)
  # compile problem
  cmp <- compile(p)
  # tests
  expect_is(cmp, "OptimizationProblem")
})

test_that("solve (RasterLayer, single zone)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("gurobi")
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  locked_in <- 2
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_locked_in_constraints(locked_in) %>%
       add_gap_portfolio(number_solutions = 5, gap = 0.5) %>%
       add_gurobi_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "list")
  expect_equal(length(s), 2) # only two solutions meet this gap
  expect_true(all(sapply(s, inherits, "RasterLayer")))
  expect_equal(names(s), paste0("solution_", seq_along(s)))
  for (i in seq_along(s))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
})

test_that("solve (RasterStack, multiple zones)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("gurobi")
  # create data
  data(sim_pu_zones_stack, sim_features_zones)
  # create problem
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(2,
                            nrow = number_of_features(sim_features_zones),
                            ncol = number_of_zones(sim_features_zones))) %>%
       add_gap_portfolio(number_solutions = 5, gap = 1) %>%
       add_binary_decisions() %>%
       add_gurobi_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "list")
  expect_equal(length(s), 5)
  expect_true(all(sapply(s, inherits, "RasterStack")))
  expect_equal(names(s), paste0("solution_", seq_along(s)))
  for (i in seq_along(s))
    for (z in seq_len(number_of_zones(sim_features_zones)))
      expect_true(all(raster::cellStats(s[[i]][[z]] * sim_features_zones[[z]],
                                        "sum") >= 2))
})

test_that("solve (SpatialPolygonsDataFrame, single zone)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("gurobi")
  # create data
  data(sim_pu_polygons, sim_features)
  # create problem
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_absolute_targets(2) %>%
       add_gap_portfolio(number_solutions = 5, gap = 1) %>%
       add_gurobi_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "SpatialPolygonsDataFrame")
  n_solutions <- length(attr(s, "objective"))
  expect_true(n_solutions > 1)
  expect_true(all(paste0("solution_", seq_len(n_solutions)) %in% names(s)))
  for (i in seq_len(n_solutions)) {
    curr_s <- s[s[[paste0("solution_", i)]] ==  1, ]
    expect_true(all(colSums(raster::extract(sim_features, curr_s,
                                            fun = "sum")) >= 2))
  }
})

test_that("solve (SpatialPolygonsDataFrame, multiple zones)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("gurobi")
  # make data
  data(sim_pu_zones_polygons, sim_features_zones)
  # solve problem
  s <- problem(sim_pu_zones_polygons, sim_features_zones,
               cost_column = c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_absolute_targets(
         matrix(2, nrow = number_of_features(sim_features_zones),
                ncol = number_of_zones(sim_features_zones))) %>%
       add_binary_decisions() %>%
       add_gap_portfolio(number_solutions = 5, gap = 1) %>%
       add_gurobi_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # output checks
  expect_is(s, "SpatialPolygonsDataFrame")
  n_solutions <- length(attr(s, "objective"))
  expect_true(n_solutions > 1)
  expect_true(all(paste0("solution_", rep(seq_len(n_solutions), 3), "_",
                         rep(zone_names(sim_features_zones), each = 10)) %in%
                  names(s)))
  for (i in seq_len(n_solutions)) {
    for (j in zone_names(sim_features_zones)) {
      curr_col <- paste0("solution_", i, "_", j)
      curr_s <- s[s[[curr_col]] ==  1, curr_col]
      k <- match(j, zone_names(sim_features_zones))
      expect_true(all(colSums(raster::extract(sim_features_zones[[k]],
                                              curr_s, fun = "sum",
                                              small = TRUE)) >= 2))
    }
  }
})

test_that("solve (numeric, single zone)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("gurobi")
  # create data
  data(sim_pu_polygons, sim_features)
  costs <- sim_pu_polygons$cost
  features <- data.frame(id = seq_len(nlayers(sim_features)),
                         name = names(sim_features))
  rij_mat <- rij_matrix(sim_pu_polygons, sim_features)
  # create problem
  p <- problem(costs, features, rij_matrix = rij_mat) %>%
       add_min_set_objective() %>%
       add_absolute_targets(2) %>%
       add_gap_portfolio(number_solutions = 5, gap = 1) %>%
       add_binary_decisions() %>%
       add_gurobi_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "list")
  n_solutions <- length(s)
  expect_equal(length(s), 5)
  expect_equal(names(s), paste0("solution_", seq_len(n_solutions)))
  expect_true(all(sapply(s, inherits, "numeric")))
  expect_true(all(sapply(s, length) == length(costs)))
  for (i in seq_len(n_solutions)) {
    curr_s <- which(s[[i]] == 1)
    expect_true(all(rowSums(rij_mat[, curr_s, drop = FALSE]) >= 2))
  }
})

test_that("solve (matrix, multiple zones)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("gurobi")
  # create data
  costs <- data.frame(id = seq_len(7),
                      cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
                      cost_2 = c(10, 10, 10, 10,  4,   1, NA),
                      spp1_z1 = c(1,  2, 0, 0, 0, 0,  0),
                      spp2_z1 = c(NA, 0, 1, 1, 0, 0,  0),
                      spp1_z2 = c(1,  0, 0, 0, 1, 0,  0),
                      spp2_z2 = c(0,  0, 0, 0, 0, 10, 0))
  spp <- data.frame(id = 1:2, name = c("spp1", "spp2"))
  rij_matrix <- list(z1 = t(as.matrix(costs[, c("spp1_z1", "spp2_z1")])),
                     z2 = t(as.matrix(costs[, c("spp1_z2", "spp2_z2")])))
  targs <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  # create problem
  p <- problem(as.matrix(costs[, c("cost_1", "cost_2")]), spp,
               rij_matrix) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targs) %>%
       add_binary_decisions() %>%
       add_gap_portfolio(number_solutions = 5, gap = 1) %>%
       add_gurobi_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "list")
  expect_equal(length(s), 5)
  n_solutions <- length(s)
  expect_equal(names(s), paste0("solution_", seq_len(n_solutions)))
  expect_true(all(sapply(s, inherits, "matrix")))
  expect_true(all(sapply(s, nrow) == nrow(costs)))
  expect_true(all(sapply(s, ncol) == 2))
  for (i in seq_len(n_solutions)) {
    for (j in seq_len(2)) {
      for (k in seq_len(2)) {
        curr_s <- which(s[[i]][, k] == 1)
        expect_true(sum(rij_matrix[[k]][j, curr_s], na.rm = TRUE) >=
                    targs[j, k])
      }
    }
  }
})
