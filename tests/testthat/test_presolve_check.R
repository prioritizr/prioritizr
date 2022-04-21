context("presolve_check")

test_that("no false positive", {
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions()
  expect_silent(expect_true(presolve_check(p)))
})

test_that("instability due to range in boundary penalties", {
  data(sim_pu_raster, sim_features)
  b <- boundary_matrix(sim_pu_raster)
  b[1, 2] <- 1e+10
  b[2, 1] <- 1e+10
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_boundary_penalties(1, data = b) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "boundary")
})

test_that("instability due to range in connectivity penalties", {
  data(sim_pu_raster, sim_features)
  cm <- boundary_matrix(sim_pu_raster)
  diag(cm) <- 0
  cm <- Matrix::drop0(cm)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_connectivity_penalties(1e+15, data = cm) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "connectivity", all = TRUE)
})

test_that("instability due to range in asymmetric connectivity penalties", {
  data(sim_pu_raster, sim_features)
  cm <- as(boundary_matrix(sim_pu_raster), "dgCMatrix")
  diag(cm) <- 0
  cm <- Matrix::drop0(cm)
  cm@x <- cm@x + runif(length(cm@x))
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_asym_connectivity_penalties(1e+15, data = cm) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "asymmetric", all = TRUE)
})

test_that("instability due to range in rij data", {
  data(sim_pu_raster, sim_features)
  sim_features[[1]][1] <- 1e+15
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "rij", all = TRUE)
})

test_that("instability due to range in cost data (objective function)", {
  data(sim_pu_raster, sim_features)
  sim_pu_raster <- sim_pu_raster
  sim_pu_raster[1] <- 1e+15
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "cost", all = TRUE)
})

test_that("instability due to range in cost data (rhs matrix)", {
  data(sim_pu_raster, sim_features)
  sim_pu_raster <- sim_pu_raster
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_shortfall_objective(budget = 1e+9) %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "budget", all = TRUE)
})

test_that("instability due to range in cost data (constraint matrix)", {
  data(sim_pu_raster, sim_features)
  sim_pu_raster <- sim_pu_raster
  sim_pu_raster[1] <- 1e+15
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_shortfall_objective(budget = 1000) %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "cost", all = TRUE)
})

test_that("instability due to budget", {
  data(sim_pu_raster, sim_features)
  sim_pu_raster <- sim_pu_raster
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_shortfall_objective(budget = 1e+20) %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "budget", all = TRUE)
})

test_that("instability due to range in feature weights", {
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_max_utility_objective(600) %>%
       add_feature_weights(
         c(1e+15, rep(1, raster::nlayers(sim_features) - 1))) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "weight", all = TRUE)
})

test_that("instability due to range in target values", {
  data(sim_pu_raster, sim_features)
  sim_features[[1]][1] <- 1e+15
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(
         c(1e+15, rep(1, raster::nlayers(sim_features) - 1))) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "target")
})

test_that("instability due to range in target weights", {
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_max_features_objective(600) %>%
       add_absolute_targets(1) %>%
       add_feature_weights(
         c(1e+15, rep(1, raster::nlayers(sim_features) - 1))) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "target weights", all = TRUE)
})

test_that("instability due to range in branch lengths", {
  data(sim_pu_raster, sim_features, sim_phylogeny)
  sim_phylogeny$edge.length[length(sim_phylogeny$edge.length)] <- 1e+15
  p <- problem(sim_pu_raster, sim_features) %>%
       add_max_phylo_div_objective(1900, sim_phylogeny) %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "branch", all = TRUE)
})

test_that("all planning units locked in", {
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_locked_in_constraints(seq_len(raster::ncell(sim_pu_raster))) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "locked in", all = TRUE)
})

test_that("all planning units locked out", {
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_locked_out_constraints(seq_len(raster::ncell(sim_pu_raster))) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "locked out", all = TRUE)
})

test_that("number of neighboring planning units", {
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_neighbor_constraints(k = 1e+9) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "neighbors", all = TRUE)
})

test_that("sparse feature data", {
  data(sim_pu_raster, sim_features)
  sim_features <- sim_features * 1e-10
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "any features", all = TRUE)
})
