context('compile') 

test_that('raster planning unit data', {
  x <- compile(problem(sim_pu_raster, sim_features) +
    minimum_set_objective() +
    relative_targets(rep(0.2, raster::nlayers(sim_features))) +
    binary_decision())
})

 
