context('objectives') 

test_that('add_minimum_set_objective', {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  targ <- unname(floor(raster::cellStats(sim_features, 'sum') * 0.25))
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decision()
  o <- compile(p)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  expect_equal(o$modelsense(), 'min')
  expect_equal(o$obj(), sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  expect_equal(o$sense(), rep('>=', raster::nlayers(sim_features)))
  expect_equal(o$rhs(), targ)
  expect_equal(o$row_ids(), rep('spp_target', raster::nlayers(sim_features)))
  expect_equal(o$col_ids(), rep('pu', n_pu))
  expect_true(all(o$A()==p$data$rij_matrix))
  # check that solution mets all targets
  s <- solve(p)
  expect_true(all(unname(raster::cellStats(sim_features * s, 'sum')) >= 
    p$feature_targets()))
})

test_that('add_maximum_coverage_objective', {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  b <- floor(raster::cellStats(sim_pu_raster, 'sum'))
  targ <- unname(floor(raster::cellStats(sim_features, 'sum') * 0.25))
  p <- problem(sim_pu_raster, sim_features) %>%
    add_maximum_coverage_objective(budget=b) %>%
    add_absolute_targets(targ) %>%
    add_binary_decision()  
  o <- compile(p)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_features <- raster::nlayers(sim_features)
  expect_equal(o$modelsense(), 'max')
  expect_equal(o$obj(), c(rep(1e-10, n_pu), rep(1, n_features)))
  expect_equal(o$sense(), c(rep('>=', n_features), '<='))
  expect_equal(o$rhs(), c(rep(0, n_features), b))
  expect_equal(o$col_ids(), c(rep('pu', n_pu), rep('spp_met', n_features)))
  expect_equal(o$row_ids(), c(rep('spp_target', n_features), 'budget'))  
  expect_true(all(o$A()[seq_len(n_features),seq_len(n_pu)]==p$data$rij_matrix))
  expect_equal(o$A()[n_features+1,], 
    c(p$planning_unit_costs(), rep(0, n_features)))
  expect_true(all(o$A()[seq_len(n_features),n_pu+seq_len(n_features)] ==
    Matrix::sparseMatrix(i=seq_len(n_features),j=seq_len(n_features),
      x=(-1*targ),giveCsparse=FALSE)))
  # check that solution meets budget
  s <- solve(p)
  expect_true(raster::cellStats(sim_pu_raster * s, 'sum') <= b)
})

# test_that('phylogenetic_coverage_objective', {
#   
# })
