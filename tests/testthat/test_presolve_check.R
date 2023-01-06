context("presolve_check")

test_that("no false positive", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions()
  expect_silent(expect_true(presolve_check(p)))
})

test_that("instability due to boundary data", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
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

test_that("instability due to connectivity penalties", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
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

test_that("instability due to asymmetric connectivity penalties", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  cm <- as_Matrix(boundary_matrix(sim_pu_raster), "dgCMatrix")
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

test_that("instability due to rij data", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_features[[1]][1] <- 1e+15
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "rij", all = TRUE)
})

test_that("instability due to high budget", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_pu_raster <- sim_pu_raster
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_shortfall_objective(budget = 1e+9) %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "budget", all = TRUE)
})

test_that("instability due to low budget", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_pu_raster <- sim_pu_raster
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_shortfall_objective(budget = 1e-30) %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "budget", all = TRUE)
})

test_that("instability due to cost data (objective function)", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_pu_raster <- sim_pu_raster
  sim_pu_raster[1] <- 1e+15
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "cost", all = TRUE)
})

test_that("instability due to cost data (constraint matrix)", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_pu_raster <- sim_pu_raster
  sim_pu_raster[1] <- 1e+15
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_shortfall_objective(budget = 1000) %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "cost", all = TRUE)
})

test_that("instability due to feature weights", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_max_utility_objective(600) %>%
       add_feature_weights(
         c(1e+15, rep(1, terra::nlyr(sim_features) - 1))) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "weight", all = TRUE)
})

test_that("instability due to high targets", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_features[[1]][1] <- 1e+15
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(
         c(1e+15, rep(1, terra::nlyr(sim_features) - 1))) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "target")
})

test_that("instability due to low targets", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_features[[1]][1] <- 1e+15
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(
         c(1e-15, rep(1, terra::nlyr(sim_features) - 1))) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "target")
})

test_that("instability due to high target weights", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_max_features_objective(600) %>%
       add_absolute_targets(1) %>%
       add_feature_weights(
         c(1e+15, rep(1, terra::nlyr(sim_features) - 1))) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "target weight", all = TRUE)
})

test_that("instability due to branch lengths", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_phylogeny <- get_sim_phylogeny()

  sim_phylogeny$edge.length[length(sim_phylogeny$edge.length)] <- 1e+15
  p <- problem(sim_pu_raster, sim_features) %>%
       add_max_phylo_div_objective(1900, sim_phylogeny) %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "branch", all = TRUE)
})

test_that("instability due to number of neighboring planning units", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_neighbor_constraints(k = 1e+9) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "neighbors", all = TRUE)
})

test_that("all planning units locked in", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_locked_in_constraints(seq_len(raster::ncell(sim_pu_raster))) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "locked in", all = TRUE)
})

test_that("all planning units locked out", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_locked_out_constraints(seq_len(raster::ncell(sim_pu_raster))) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "locked out", all = TRUE)
})

test_that("sparse feature data", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_features <- sim_features * 1e-10
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0) %>%
       add_binary_decisions()
  expect_warning(expect_false(presolve_check(p)), "any features", all = TRUE)
})
