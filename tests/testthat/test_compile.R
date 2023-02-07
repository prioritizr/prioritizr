context("compile")

test_that("compile (compressed formulation)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create targets data
  targ <- floor(terra::global(sim_features, "sum", na.rm = TRUE)[[1]] * 0.25)
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  o <- compile(p)
  # calculations for tests
  n_pu <- nrow(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(sim_pu_raster[[1]][!is.na(sim_pu_raster)]))
  expect_equal(o$sense(), rep(">=", terra::nlyr(sim_features)))
  expect_equal(o$rhs(), targ)
  expect_equal(o$row_ids(), rep("spp_target", terra::nlyr(sim_features)))
  expect_equal(o$col_ids(), rep("pu", n_pu))
  expect_true(all(o$A() == p$data$rij_matrix[[1]]))
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))
})

test_that("compile (compressed formulation, negative data)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # modify data
  sim_pu_raster <- terra::setValues(
    sim_pu_raster, runif(terra::ncell(sim_pu_raster), -0.5, 1)
  )
  sim_features[[1]] <- terra::setValues(
    sim_features[[1]], runif(terra::ncell(sim_features), -0.5, -0.1)
  )
  # calculate targets
  targ <- floor(terra::global(sim_features, "sum", na.rm = TRUE)[[1]] * 0.25)
  # create problem
  expect_warning(
    p <- problem(sim_pu_raster, sim_features),
    "negative"
  )
  # update problem
  expect_warning(
    p <-
      p %>%
      add_min_set_objective() %>%
      add_absolute_targets(targ) %>%
      add_binary_decisions(),
    "negative"
  )
  o <- compile(p)
  # calculations for tests
  n_pu <- nrow(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(sim_pu_raster[[1]][!is.na(sim_pu_raster)]))
  expect_equal(o$sense(), rep(">=", terra::nlyr(sim_features)))
  expect_equal(o$rhs(), targ)
  expect_equal(o$row_ids(), rep("spp_target", terra::nlyr(sim_features)))
  expect_equal(o$col_ids(), rep("pu", n_pu))
  expect_true(all(o$A() == p$data$rij_matrix[[1]]))
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))
})

test_that("compile (expanded formulation)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # calculate targets
  targ <- floor(terra::global(sim_features, "sum", na.rm = TRUE)[[1]] * 0.25)
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  o <- compile(p, FALSE)
  # calculations for tests
  n_pu <- nrow(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- terra::nlyr(sim_features)
  rij <- rij_matrix(sim_pu_raster, sim_features)
  # tests
  expect_equal(o$modelsense(), "min")
  expect_equal(
    o$obj(),
    c(sim_pu_raster[[1]][!is.na(sim_pu_raster)], rep(0, n_pu * n_f))
  )
  expect_equal(
    o$sense(),
    c(rep("<=", n_pu * n_f), rep(">=", terra::nlyr(sim_features)))
  )
  expect_equal(o$rhs(), c(rep(0, n_pu * n_f), targ))
  expect_equal(
    o$row_ids(),
    c(rep("pu_ijz", n_pu * n_f), rep("spp_target", terra::nlyr(sim_features)))
  )
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
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # modify data
  sim_pu_raster <- terra::setValues(
    sim_pu_raster, runif(terra::ncell(sim_pu_raster), -0.5, 1)
  )
  sim_features[[1]] <- terra::setValues(
    sim_features[[1]], runif(terra::ncell(sim_features), -0.5, -0.1)
  )
  # calculate targets
  targ <- floor(terra::global(sim_features, "sum", na.rm = TRUE)[[1]] * 0.25)
  # create problem
  expect_warning(
    p <-
      problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_absolute_targets(targ) %>%
      add_binary_decisions(),
    "negative"
  )
  # tests
  expect_error(compile(p, FALSE))
})
