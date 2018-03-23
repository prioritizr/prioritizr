context("add_feature_weights")

test_that("compile (compressed formulation, single zone)", {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  b <- floor(raster::cellStats(sim_pu_raster, "sum")) * 0.25
  p <- problem(sim_pu_raster, sim_features) %>%
       add_max_cover_objective(budget = b) %>%
       add_feature_weights(seq(10, 14))
  o <- compile(p)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- raster::nlayers(sim_features)
  scaled_costs <- p$planning_unit_costs()
  scaled_costs <- scaled_costs * (1e-10 / min(scaled_costs))
  expect_equal(o$modelsense(), "max")
  expect_equal(o$obj(), c(scaled_costs, seq(10, 14)))
  expect_equal(o$sense(), c(rep(">=", n_f), "<="))
  expect_equal(o$rhs(), c(rep(0, n_f), b))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("spp_met", n_f)))
  expect_equal(o$row_ids(), c(rep("spp_target", n_f), "budget"))
  expect_true(all(o$A()[seq_len(n_f),
                        seq_len(n_pu)] == p$data$rij_matrix))
  expect_equal(o$A()[n_f + 1, ],
    c(p$planning_unit_costs(), rep(0, n_f)))
  expect_true(all(o$A()[seq_len(n_f), n_pu + seq_len(n_f)] ==
    Matrix::sparseMatrix(i = seq_len(n_f), j = seq_len(n_f),
      x = rep(-1, n_f), giveCsparse = FALSE)))
  expect_equal(o$lb(), rep(0, n_f + n_pu))
  expect_equal(o$ub(), rep(1, n_f + n_pu))
})

test_that("solve (compressed formulation, single zone)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create data
  budget <- 4.23
  cost <- raster::raster(matrix(c(1, 2, NA, 4), nrow = 1))
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), nrow = 1)),
                            raster::raster(matrix(c(10, 10, 10, 10), nrow = 1)),
                            raster::raster(matrix(c(0, 0, 0, 2), nrow = 1)))
  # create problem
  p <- problem(cost, features) %>%
       add_max_utility_objective(budget = budget) %>%
       add_feature_weights(c(1, 1, 100)) %>%
       add_locked_out_constraints(locked_out) %>%
       add_default_solver(gap = 0)
  # solve problem
  s <- solve(p)
  # test for correct solution
  expect_equal(raster::values(s), c(0, 0, NA, 1))
})

test_that("compile (expanded formulation, single zone)", {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  b <- floor(raster::cellStats(sim_pu_raster, "sum")) * 0.25
  p <- problem(sim_pu_raster, sim_features) %>%
       add_max_cover_objective(budget = b) %>%
       add_feature_weights(seq(10, 14))
  o <- compile(p, compressed_formulation = FALSE)
  # check that constraints and metadata have been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- raster::nlayers(sim_features)
  rij <- rij_matrix(sim_pu_raster, sim_features)
  scaled_costs <- p$planning_unit_costs()
  scaled_costs <- scaled_costs * (1e-10 / min(scaled_costs))
  expect_equal(o$modelsense(), "max")
  expect_equal(o$obj(), c(scaled_costs, rep(0, n_pu * n_f), seq(10, 14)))
  expect_equal(o$sense(), c(rep("<=", n_pu * n_f), rep( ">=", n_f), "<="))
  expect_equal(o$rhs(), c(rep(0, n_f * n_pu), rep(0, n_f), b))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("pu_ij", n_pu * n_f),
                              rep("spp_met", n_f)))
  expect_equal(o$row_ids(), c(rep("pu_ij", n_f * n_pu),
                              rep("spp_target", n_f), "budget"))
  expect_equal(o$lb(), rep(0, n_pu + (n_f * n_pu) + n_f))
  expect_equal(o$ub(), rep(1, n_pu + (n_f * n_pu) + n_f))
  # check that problem matrix is correct
  row <- 0
  for (i in seq_len(n_f)) {
    for (j in seq_len(n_pu)) {
      row <- row + 1
      curr_row <- rep(0, n_pu + (n_pu * n_f) + n_f)
      curr_row[j] <- -1
      curr_row[n_pu + ( (i - 1) * n_pu) + j] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
  }
  for (i in seq_len(n_f)) {
    curr_row <- rep(0, n_pu + (n_pu * n_f) + n_f)
    curr_row[(i * n_pu) + seq_len(n_pu)] <- rij[i, ]
    curr_row[n_pu + (n_f * n_pu) + i] <- -1
    expect_equal(o$A()[(n_f * n_pu) + i, ], curr_row)
  }
  expect_equal(o$A()[(n_pu * n_f) + n_f + 1, ], c(p$planning_unit_costs(),
                                                  rep(0, n_f * n_pu),
                                                  rep(0, n_f)))
})

test_that("solve (expanded formulation, single zone)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create data
  budget <- 4.23
  cost <- raster::raster(matrix(c(1, 2, NA, 4), nrow = 1))
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), nrow = 1)),
                            raster::raster(matrix(c(10, 10, 10, 10), nrow = 1)),
                            raster::raster(matrix(c(0, 0, 0, 2), nrow = 1)))
  # create problem
  p <- problem(cost, features) %>%
       add_max_utility_objective(budget = budget) %>%
       add_feature_weights(c(1, 1, 100)) %>%
       add_locked_out_constraints(locked_out) %>%
       add_default_solver(gap = 0)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # test for correct solution
  expect_equal(raster::values(s), c(0, 0, NA, 1))
})

test_that("invalid inputs (single zone)", {
  # check that invalid arguments result in errors
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
    add_feature_weights(seq_len(4))
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
    add_feature_weights(c(-1, 1, 2, 3, 4))
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
    add_feature_weights(c(1, NA, 2, 3, 4))
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
    add_feature_weights(c(1, Inf, 2, 3, 4))
  })
})
