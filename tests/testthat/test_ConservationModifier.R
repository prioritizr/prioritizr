context("ConservationModifier")

test_that("initialize", {
  x <- ConservationModifier$new()

  expect_is(x, "ConservationModifier")
})

test_that("methods", {
  x <- ConservationModifier$new()
  # tests
  suppressMessages(x$print())
  suppressMessages(x$show())
  expect_is(x$repr(), "character")
  expect_true(x$calculate())
  expect_is(x$get_data("z"), "Waiver")
  x$set_data("z", 1)
  expect_equal(x$get_data("z"), 1)
  expect_is(x$get_internal("z"), "Waiver")
  x$set_internal("z", 2)
  expect_equal(x$get_internal("z"), 2)
})
