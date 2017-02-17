context('targets')

test_that('relative_targets (n=1)', {
  # create objects
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>% add_relative_targets(0.1)
  # basic tests
  print(x)
  x
  # check that invalid arguments cannot be used to create relative_targets
  expect_error(relative_targets(-5))
  expect_error(relative_targets(NA))
  expect_error(relative_targets(-Inf))
  expect_error(relative_targets(5))
  # check validation and synchronization
  expect_true(x$prevalidate(p))
  expect_true(x$synchronize(p))
  expect_true(x$postvalidate(p))
  # check that synchronization was successful
  expect_true(nrow(x$get_parameter('targets')) == 
    raster::nlayers(sim_features))
  expect_true(all(rownames(x$get_parameter('targets')) == names(sim_features)))
  expect_true(all(x$get_parameter('targets')[[1]] == 0.1))
  # check that output is correct
  expect_equal(x$output(), 
    0.1 * unname(raster::cellStats(sim_features, 'sum')))
})

test_that('relative_targets (n > 1)', {
  # create objects
  data(sim_pu_raster, sim_features)
  v <- round(seq(0.1, 0.9, length.out=raster::nlayers(sim_features)), 3)
  x <- relative_targets(v)
  # basic tests
  print(x)
  x
  # check validation and synchronization
  expect_true(x$prevalidate(p))
  expect_true(x$synchronize(p))
  expect_true(x$postvalidate(p))
  # check that synchronization was successful
  expect_true(nrow(x$get_parameter('targets')) == 
    raster::nlayers(sim_features))
  expect_true(all(rownames(x$get_parameter('targets')) == names(sim_features)))
  expect_true(all(x$get_parameter('targets')[[1]] == v))
  # check that output is correct
  expect_equal(x$output(), 
    v * unname(raster::cellStats(sim_features, 'sum')))  
  # check that prevalidation will fail when number of targets is more than
  # two and number of targets is differnet to the number of features
  expect_false(relative_targets(c(0.1, 0.2))$prevalidate(sim_features))
})

test_that('absolute_targets (n=1)', {
  # create objects
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features)  
  x <- absolute_targets(5)
  # basic tests
  print(x)
  x
  # check that invalid arguments cannot be used to create relative_targets
  expect_error(absolute_targets(-5))
  expect_error(relative_targets(NA))
  expect_error(relative_targets(-Inf))
  # check validation and synchronization
  expect_true(x$prevalidate(p))
  expect_true(x$synchronize(p))
  expect_true(x$postvalidate(p))
  # check that synchronization was successful
  expect_true(nrow(x$get_parameter('targets')) == 
    raster::nlayers(sim_features))
  expect_true(all(rownames(x$get_parameter('targets')) == names(sim_features)))
  expect_true(all(x$get_parameter('targets')[[1]] == 5))
  # check that output is correct
  expect_equal(x$output(), rep(5, raster::nlayers(sim_features)))
})

test_that('absolute_targets (n > 1)', {
  x <- absolute_target(c(1, 5))
  print(x)
  x
  expect_equal(x$output(), c(1, 5))
})

test_that('loglinear_targets', {
  x <- loglinear_target(20, 100, 0.9, 0.3)
  print(x)
  x
})
