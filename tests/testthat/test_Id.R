context("Id")

test_that("Id", {
  # constructor
  i <- new_id()
  # methods
  i
  print(i)
  as.character(i)
  expect_true(inherits(i, "Id"))
  expect_true(is.Id(i))
  expect_false(is.Id("a"))
  expect_true(i == i)
  expect_false(i == new_id())
})
