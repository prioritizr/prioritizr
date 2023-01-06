context("compile")

test_that("compile (compressed formulation)", {
  # generate optimization problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
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
  expect_equal(o$sense(), rep(">=", terra::nlyr(sim_features)))
  expect_equal(o$rhs(), targ)
  expect_equal(o$row_ids(), rep("spp_target", terra::nlyr(sim_features)))
  expect_equal(o$col_ids(), rep("pu", n_pu))
  expect_true(all(o$A() == p$data$rij_matrix[[1]]))
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))
})

test_that("compile (compressed formulation, negative data)", {
  # generate optimization problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_pu_raster[] <- sim_pu_raster[] * runif(length(sim_pu_raster[]), -0.5, 1)
  sim_features[] <- sim_features[] * runif(length(sim_features[]), -0.5, 1)
  expect_warning(x <- problem(sim_pu_raster, sim_features))
  targ <- unname(floor(raster::cellStats(sim_features, "sum")) * 0.25)
  expect_warning(p <- problem(sim_pu_raster, sim_features) %>%
                      add_min_set_objective() %>%
                      add_absolute_targets(targ) %>%
                      add_binary_decisions())
  o <- compile(p)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  expect_equal(o$sense(), rep(">=", terra::nlyr(sim_features)))
  expect_equal(o$rhs(), targ)
  expect_equal(o$row_ids(), rep("spp_target", terra::nlyr(sim_features)))
  expect_equal(o$col_ids(), rep("pu", n_pu))
  expect_true(all(o$A() == p$data$rij_matrix[[1]]))
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))
})

test_that("compile (expanded formulation)", {
  # generate optimization problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  targ <- unname(floor(raster::cellStats(sim_features, "sum") * 0.25))
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targ) %>%
       add_binary_decisions()
  o <- compile(p, FALSE)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- terra::nlyr(sim_features)
  rij <- rij_matrix(sim_pu_raster, sim_features)
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(sim_pu_raster[[1]][!is.na(sim_pu_raster)],
                          rep(0, n_pu * n_f)))
  expect_equal(o$sense(), c(rep("<=", n_pu * n_f),
                            rep(">=", terra::nlyr(sim_features))))
  expect_equal(o$rhs(), c(rep(0, n_pu * n_f), targ))
  expect_equal(o$row_ids(), c(rep("pu_ijz", n_pu * n_f),
                              rep("spp_target", terra::nlyr(sim_features))))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("pu_ijz", n_pu * n_f)))
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

test_that("compile (expanded formulation, negative data)", {
  # generate optimization problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_pu_raster[] <- sim_pu_raster[] * runif(length(sim_pu_raster[]), -0.5, 1)
  sim_features[] <- sim_features[] * runif(length(sim_features[]), -0.5, 1)
  targ <- unname(floor(raster::cellStats(sim_features, "sum") * 0.25))
  p <- expect_warning(problem(sim_pu_raster, sim_features) %>%
                      add_min_set_objective() %>%
                      add_absolute_targets(targ) %>%
                      add_binary_decisions())
  expect_error(o <- compile(p, FALSE))
})
