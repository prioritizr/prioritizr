test_that("initialize", {
  x <- ConservationModifier$new()

  expect_inherits(x, "ConservationModifier")
})

test_that("methods", {
  x <- ConservationModifier$new()
  # tests
  suppressMessages(x$print())
  suppressMessages(x$show())
  expect_inherits(x$repr(), "character")
  expect_true(x$calculate())
  expect_inherits(x$get_data("z"), "Waiver")
  x$set_data("z", 1)
  expect_equal(x$get_data("z"), 1)
  expect_inherits(x$get_internal("z"), "Waiver")
  x$set_internal("z", 2)
  expect_equal(x$get_internal("z"), 2)
})
