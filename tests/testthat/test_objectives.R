context('objectives') 

test_that('add_minimum_set_objective', {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_absolute_targets(c(10, 20, 30, 40, 50)) %>%
    add_binary_decision()  
  o <- compile(p)
  # check that objective has been correctly applied
  expect_equal(o$modelsense(), 'min')
  expect_equal(o$obj(), sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  expect_equal(o$sense(), rep('<=', raster::nlayers(sim_features)))
  expect_equal(o$rhs(), c(10, 20, 30, 40, 50))
  expect_equal(o$row_ids(), rep('spp', raster::nlayers(sim_features)))
  expect_equal(o$col_ids(), rep('pu', length(sim_pu_raster)))
})

test_that('add_maximum_coverage_objective', {

})

test_that('phylogenetic_coverage_objective', {
  stop('not implemented')
})
