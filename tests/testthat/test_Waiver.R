context('Waiver')

test_that('Waiver', {
  # constructor
  i <- waiver()  
  # methods
  i
  print(i)
  expect_true(inherits(i, 'Waiver'))
  expect_true(is.Waiver(i))
})

