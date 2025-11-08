test_that("no false positive", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # tests
  expect_silent(expect_true(presolve_check(p)))
})

test_that("instability due to boundary data", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create boundary data
  b <- boundary_matrix(sim_pu_raster)
  b[1, 2] <- 1e+10
  b[2, 1] <- 1e+10
  # create problem
  suppressWarnings(
    p <-
      problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_boundary_penalties(1, data = b) %>%
      add_binary_decisions()
  )
  # tests
  expect_warning(expect_false(presolve_check(p)), "boundary")
})

test_that("instability due to connectivity penalties", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create connectivity matrix
  cm <- boundary_matrix(sim_pu_raster)
  diag(cm) <- 0
  cm <- Matrix::drop0(cm)
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_connectivity_penalties(1e+15, data = cm) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "connectivity")
})

test_that("instability due to asymmetric connectivity penalties", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create connectivity matrix
  cm <- as_Matrix(boundary_matrix(sim_pu_raster), "dgCMatrix")
  diag(cm) <- 0
  cm <- Matrix::drop0(cm)
  cm@x <- cm@x + runif(length(cm@x))
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_asym_connectivity_penalties(1e+15, data = cm) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "asymmetric")
})

test_that("instability due to rij data", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update data
  sim_features[[1]][1] <- 1e+15
  # create problem
  p <-
  problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(1) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "rij")
})

test_that("instability due to high budget", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(budget = 1e+9) %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "budget")
})

test_that("budget higher than total costs (single zone)", {
  # import data
  sim_pu_raster <- (get_sim_pu_raster() * 0) + 1
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(budget = 1000) %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "budget")
})

test_that("budget higher than total costs (multiple zones)", {
  # import data
  get_sim_zones_pu_raster <- (get_sim_zones_pu_raster() * 0) + 1
  get_sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(get_sim_zones_pu_raster, get_sim_zones_features) %>%
    add_max_utility_objective(budget = c(1000, 1000, 1000)) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "budget")
})

test_that("instability due to low budget", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(budget = 1e-30) %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "budget")
})

test_that("instability due to cost data (objective function)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update data
  sim_pu_raster[1] <- 1e+15
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "cost")
})

test_that("instability due to cost data (constraint matrix)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update data
  sim_pu_raster[1] <- 1e+15
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(budget = 1000) %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "cost")
})

test_that("instability due to feature weights", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_max_utility_objective(600) %>%
    add_feature_weights(c(1e+15, rep(1, terra::nlyr(sim_features) - 1))) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "weight")
})

test_that("instability due to high targets", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update data
  sim_features[[1]][1] <- 1e+15
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1e+15, rep(1, terra::nlyr(sim_features) - 1))) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "target")
})

test_that("instability due to low targets", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update data
  sim_features[[1]][1] <- 1e+15
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1e-15, rep(1, terra::nlyr(sim_features) - 1))) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "target")
})

test_that("instability due to high target weights", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_max_features_objective(600) %>%
    add_absolute_targets(1) %>%
    add_feature_weights(c(1e+15, rep(1, terra::nlyr(sim_features) - 1))) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "target weight")
})

test_that("instability due to branch lengths", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_phylogeny <- get_sim_phylogeny()
  # update data
  sim_phylogeny$edge.length[length(sim_phylogeny$edge.length)] <- 1e+15
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_max_phylo_div_objective(1900, sim_phylogeny) %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "branch")
})

test_that("instability due to number of neighboring planning units", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_neighbor_constraints(k = 1e+9, clamp = FALSE) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "neighbors")
})

test_that("all planning units locked in", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problems
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_locked_in_constraints(seq_len(terra::ncell(sim_pu_raster))) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "locked in")
})

test_that("all planning units locked out", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_locked_out_constraints(seq_len(terra::ncell(sim_pu_raster))) %>%
    add_binary_decisions()
  # tests
  expect_warning(expect_false(presolve_check(p)), "locked out")
})

test_that("sparse feature data", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # update data
  sim_features <- sim_features * 1e-10
  # create problem
  expect_warning(
    p <-
      problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0) %>%
      add_binary_decisions(),
    "zero values"
  )
  # tests
  expect_warning(
    expect_false(presolve_check(p)),
    "do not have a single feature"
  )
})

test_that("single feature", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()[[1]]
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # tests
  expect_warning(
    expect_false(presolve_check(p)),
    "contains a single feature"
  )
})

test_that("planning unit bounds", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()[[1]]
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_locked_in_constraints(2) %>%
    add_locked_out_constraints(2) %>%
    add_binary_decisions()
  # tests
  expect_warning(
    expect_false(presolve_check(p)),
    "locked in and locked out"
  )
})

test_that("decision variable bounds", {
  # create problem
  o <- optimization_problem(
    list(
      modelsense = "min",
      A_i = c(0L, 1L, 1L),
      A_j = c(0L, 1L, 2L),
      A_x = c(7, 8, 9),
      obj = c(9, 10, 11),
      lb = c(12, 13, 14),
      ub = c(15, 20, 17),
      rhs = c(18, 19),
      number_of_features = 2,
      number_of_planning_units = 1,
      number_of_zones = 1,
      sense = c("=", "="),
      vtype = c("B", "S", "C"),
      row_ids = c("a", "b"),
      col_ids = c("d", "e", "f"),
      compressed_formulation = FALSE
    )
  )
  # modify variable bounds
  o$set_lb(c(15, 20, 20))
  o$set_ub(c(15, 10, 17))
  # tests
  expect_warning(
    expect_false(presolve_check(o)),
    "lower bounds"
  )
})

test_that("multi_problem()", {

})
