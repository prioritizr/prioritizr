context("add_min_set_objective")

test_that("compile (compressed formulation)", {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  targ <- unname(floor(raster::cellStats(sim_features, "sum") * 0.25))
  p <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  o <- compile(p)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  expect_equal(o$sense(), rep(">=", raster::nlayers(sim_features)))
  expect_equal(o$rhs(), targ)
  expect_equal(o$row_ids(), rep("spp_target", raster::nlayers(sim_features)))
  expect_equal(o$col_ids(), rep("pu", n_pu))
  expect_true(all(o$A() == p$data$rij_matrix))
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))
})

test_that("solve (compressed formulation)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
          add_min_set_objective() %>%
          add_absolute_targets(c(2, 10)) %>%
          add_locked_in_constraints(locked_in) %>%
          add_locked_out_constraints(locked_out) %>%
          add_default_solver(gap = 0)
  # solve problem
  s <- solve(p)
  # test for correct solution
  expect_equal(raster::values(s), c(0, 1, 1, NA))
})

test_that("compile (expanded formulation)", {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  targ <- unname(floor(raster::cellStats(sim_features, "sum") * 0.25))
  p <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  o <- compile(p, FALSE)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- raster::nlayers(sim_features)
  rij <- rij_matrix(sim_pu_raster, sim_features)
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(sim_pu_raster[[1]][!is.na(sim_pu_raster)],
                          rep(0, n_pu * n_f)))
  expect_equal(o$sense(), c(rep("<=", n_pu * n_f),
                            rep(">=", raster::nlayers(sim_features))))
  expect_equal(o$rhs(), c(rep(0, n_pu * n_f), targ))
  expect_equal(o$row_ids(), c(rep("pu_ij", n_pu * n_f),
                              rep("spp_target", raster::nlayers(sim_features))))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("pu_ij", n_pu * n_f)))
  expect_equal(o$lb(), rep(0, n_pu + (n_pu * n_f)))
  expect_equal(o$ub(), rep(1, n_pu + (n_pu * n_f)))
  # test that model matrix is correct
  row <- 0
  for (i in seq_len(n_f)) {
    for (j in seq_len(n_pu)) {
      row <- row + 1
      curr_row <- rep(0, n_pu + (n_pu * n_f))
      curr_row[j] <- -1
      curr_row[n_pu + ( (i - 1) * n_pu) + j] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
  }
  for (i in seq_len(n_f)) {
    curr_row <- rep(0, n_pu + (n_pu * n_f))
    curr_row[(i * n_pu) + seq_len(n_pu)] <- rij[i, ]
    expect_equal(o$A()[(n_f * n_pu) + i, ], curr_row)
  }
})

test_that("solve (expanded formulation)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
          add_min_set_objective() %>%
          add_absolute_targets(c(2, 10)) %>%
          add_locked_in_constraints(locked_in) %>%
          add_locked_out_constraints(locked_out) %>%
          add_default_solver(gap = 0)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # test for correct solution
  expect_equal(raster::values(s), c(0, 1, 1, NA))
})
