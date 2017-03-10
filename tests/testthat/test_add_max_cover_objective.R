context("add_max_cover_objective")

test_that("compile (compressed formulation)", {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  b <- floor(raster::cellStats(sim_pu_raster, "sum")) * 0.25
  p <- problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = b)
    o <- compile(p)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_features <- raster::nlayers(sim_features)
  scaled_costs <- p$planning_unit_costs()
  scaled_costs <- scaled_costs * (1e-10 / min(scaled_costs))
  expect_equal(o$modelsense(), "max")
  scaled_costs <- scaled_costs * (1e-10 / min(scaled_costs))
  expect_equal(o$sense(), c(rep("=", n_features), "<="))
  expect_equal(o$rhs(), c(rep(0, n_features), b))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("amount", n_features)))
  expect_equal(o$row_ids(), c(rep("spp_amount", n_features), "budget"))
  expect_true(all(o$A()[seq_len(n_features),
                        seq_len(n_pu)] == p$data$rij_matrix))
  expect_equal(o$A()[n_features + 1, ],
    c(p$planning_unit_costs(), rep(0, n_features)))
  expect_true(all(o$A()[seq_len(n_features), n_pu + seq_len(n_features)] ==
    Matrix::sparseMatrix(i = seq_len(n_features), j = seq_len(n_features),
      x = rep(-1, n_features), giveCsparse = FALSE)))
  expect_equal(o$lb(), rep(0, n_features + n_pu))
  expect_equal(o$ub(), c(rep(1, n_pu),
    unname(p$feature_abundances_in_planning_units())))
})

test_that("solution (compressed formulation)", {
  skip_on_cran()
  # create data
  budget <- 4.23
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 2))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 2)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 2)))
  # create problem
  p <- problem(cost, features) %>%
        add_max_cover_objective(budget = budget) %>%
        add_locked_in_constraints(locked_in) %>%
        add_locked_out_constraints(locked_out) %>%
        add_default_solver(time_limit = 5)
  # solve problem
  s <- solve(p)
  # test that solution is correct
  expect_equal(raster::values(s), c(0, 1, 1, NA))
})

test_that("compile (expanded formulation)", {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  b <- floor(raster::cellStats(sim_pu_raster, "sum")) * 0.25
  p <- problem(sim_pu_raster, sim_features) %>%
    add_max_cover_objective(budget = b)
  o <- compile(p, compressed_formulation = FALSE)
  # check that constraints and metadata have been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- raster::nlayers(sim_features)
  rij <- rij_matrix(sim_pu_raster, sim_features)
  scaled_costs <- p$planning_unit_costs()
  scaled_costs <- scaled_costs * (1e-10 / min(scaled_costs))
  expect_equal(o$modelsense(), "max")
  scaled_costs <- scaled_costs * (1e-10 / min(scaled_costs))
  expect_equal(o$sense(), c(rep("<=", n_pu * n_f), rep( "=", n_f), "<="))
  expect_equal(o$rhs(), c(rep(0, n_f * n_pu), rep(0, n_f), b))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("pu_ij", n_pu * n_f),
                              rep("amount", n_f)))
  expect_equal(o$row_ids(), c(rep("pu_ij", n_f * n_pu), rep("spp_amount", n_f),
                              "budget"))
  expect_equal(o$lb(), rep(0, n_pu + (n_f * n_pu) + n_f))
  expect_equal(o$ub(), c(rep(1, n_pu), rep(1, n_pu * n_f),
    unname(p$feature_abundances_in_planning_units())))
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

test_that("solution (expanded formulation)", {
  skip_on_cran()
  # create data
  budget <- 4.23
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 2))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 2)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 2)))
  # create problem
  p <- problem(cost, features) %>%
        add_max_cover_objective(budget = budget) %>%
        add_locked_in_constraints(locked_in) %>%
        add_locked_out_constraints(locked_out) %>%
        add_default_solver(time_limit = 5)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # test that solution is correct
  expect_equal(raster::values(s), c(0, 1, 1, NA))
})

test_that("invalid inputs", {
  # check that ignored data throws a warning
  expect_warning({
    problem(sim_pu_raster, sim_features) %>%
      add_max_cover_objective(budget = 5000) %>%
      add_relative_targets(0.1) %>%
      compile()
  })
  # check that invalid arguments result in errors
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_max_cover_objective(budget = -5)
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_max_cover_objective(budget = 0)
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_max_cover_objective(budget = NA)
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_max_cover_objective(budget = Inf)
  })
})
