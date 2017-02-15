context('targets')

test_that('relative_targets (n=1)', {
  x <- relative_targets(0.1)
  print(x)
  x
  x$data$abundances <- 100
  expect_equal(x$output(), 10)
  expect_error(relative_targets(-5))
  expect_error(relative_targets(NA))
  expect_error(relative_targets(-Inf))
  expect_error(relative_targets(5))  
})

test_that('relative_targets (n > 1)', {
  x <- relative_targets(c(0.1, 0.9))
  print(x)
  x
  expect_equal(x$output(), c(0.1, 0.9))
  expect_error(relative_targets(c(0.1, -5)))
  expect_error(relative_targets(c(0.1, NA)))
  expect_error(relative_targets(c(0.1, -Inf)))
  expect_error(relative_targets(c(0.1, 5)))
})

test_that('absolute_targets (n=1)', {
  x <- absolute_target(5)
  print(x)
  x
  expect_equal(x$output(), 5)
})

test_that('absolute_targets (n > 1)', {
  x <- absolute_target(5)
  print(x)
  x
  expect_equal(x$output(), 5)
})

test_that('loglinear_targets (n=1)', {
  x <- loglinear_target(20, 100, 0.9, 0.3)
  print(x)
  x
})
