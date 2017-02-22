context('utils') 

# all tests for R interface are in the prioritizrutils package
# the purpose of this test is to assert that the prioritizrutils package
# has been imported correctly

test_that('basic problem', {
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
