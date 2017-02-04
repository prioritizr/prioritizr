context('id')

test_that('id', {
  # constructor
  i <- new_id()  
  # methods
  i
  print(i)
  as.character(i)
  expect_true(i == i)
  expect_false(i == new_id())
})

