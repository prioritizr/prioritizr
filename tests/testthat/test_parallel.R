context('parallel')

test_that('cluster functions', {
  # no parallel backend initially setup
  expect_true(is.null(asNamespace('prioritizr')$.pkgenv$cluster))
  # create parallel backend
  set_number_of_threads(2)
  # check that it is created
  expect_true(is.parallel())
  expect_false(is.null(asNamespace('prioritizr')$.pkgenv$cluster))
  expect_equal(get_number_of_threads(), 2L)
  # kill parallel backend
  set_number_of_threads(1)
  # check that is is killed
  expect_false(is.parallel())
  expect_true(is.null(asNamespace('prioritizr')$.pkgenv$cluster))
  expect_equal(get_number_of_threads(), 1L)
  # errors
  expect_error(set_number_of_threads(0))
  expect_error(set_number_of_threads(-5))
  expect_error(set_number_of_threads(NA_integer_))
  expect_error(set_number_of_threads('1'))
  expect_error(set_number_of_threads(parallel::detectCores() + 1))
})

test_that('disribute load', {
  expect_identical(distribute_load(5, 1), list(seq_len(5)))
  expect_identical(distribute_load(5, 5), as.list(seq_len(5)))
  expect_identical(distribute_load(10, 5), list(1:2, 3:4, 
                                                5:6, 7:8,
                                                9:10))
  expect_identical(distribute_load(12, 5), list(1:3, 4:5, 
                                                6:8, 9:10,
                                                11:12))
  # errors
  expect_error(distribute_load(5.3, 5L))
  expect_error(distribute_load(0, 2L))
  expect_error(distribute_load(-1L, 2L))

})

