context("disribute_load")

test_that("disribute_load", {
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
