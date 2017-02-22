context('add_minimum_set_objective')

test_that('compile', {
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
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))
})
 
test_that('solution', {
  skip_on_cran()
  # create data
  cost <- raster::raster(matrix(c(1,2,2,NA), ncol=2))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2,1,1,0), ncol=2)),
                            raster::raster(matrix(c(10,10,10,10), ncol=2)))
  # create problem
  p <- problem(cost, features) %>%
          add_minimum_set_objective() %>%
          add_absolute_targets(c(2, 10)) %>%
          add_locked_in_constraint(locked_in) %>%
          add_locked_out_constraint(locked_out)
  # solve problem
  s <- solve(p)
  # test for correct solution
  expect_equal(raster::values(s), c(0,1,1,NA))
})


