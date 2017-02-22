context('add_maximum_representation_objective') 

test_that('compile', {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  b <- floor(raster::cellStats(sim_pu_raster, 'sum'))
  targ <- unname(floor(raster::cellStats(sim_features, 'sum') * 0.25))
  p <- problem(sim_pu_raster, sim_features) %>%
    add_maximum_representation_objective(budget=b) %>%
    add_absolute_targets(targ) %>%
    add_binary_decision()  
  o <- compile(p)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_features <- raster::nlayers(sim_features)
  scaled_costs <- p$planning_unit_costs()
  scaled_costs <- scaled_costs * (1e-10/min(scaled_costs))
  expect_equal(o$modelsense(), 'max')
  expect_equal(o$obj(), c(scaled_costs, rep(1, n_features)))
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
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))      
})

test_that('solution', {
  skip_on_cran()
  # create data
  budget <- 4.23
  cost <- raster::raster(matrix(c(1,2,2,NA), ncol=2))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2,1,1,0), ncol=2)),
                            raster::raster(matrix(c(10,10,10,10), ncol=2)))
  # create problem
  p <- problem(cost, features) %>%
        add_maximum_representation_objective(budget=budget) %>%
        add_locked_in_constraint(locked_in) %>%
        add_locked_out_constraint(locked_out) %>%
        add_absolute_targets(c(2,10))  
  # solve problem
  s <- solve(p)
  # test that solution is correct
  expect_equal(raster::values(s), c(0,1,1,NA))
})

test_that('invalid inputs', {
  # check that invalid arguments result in errors
  expect_error({problem(sim_pu_raster, sim_features) %>%
    add_maximum_representation_objective(budget=-5) %>%
    add_absolute_targets(targ)
  })
  expect_error({problem(sim_pu_raster, sim_features) %>%
    add_maximum_representation_objective(budget=0) %>%
    add_absolute_targets(targ)
  })
  expect_error({problem(sim_pu_raster, sim_features) %>%
    add_maximum_representation_objective(budget=NA) %>%
    add_absolute_targets(targ)
  })
  expect_error({problem(sim_pu_raster, sim_features) %>%
    add_maximum_representation_objective(budget=Inf) %>%
    add_absolute_targets(targ)
  })
})
