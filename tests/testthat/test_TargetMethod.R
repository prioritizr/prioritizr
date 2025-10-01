test_that("new_target_method", {
  # create object
  m <- new_target_method(
    name = "METHOD",
    type = "relative",
    fun = function(x, features, fish, greg, call) {
      return(rep(1 / fish, length(features)))
    },
    args = list(fish = 4, greg = 5)
  )
  # run tests
  print(m)
  expect_inherits(m, "TargetMethod")
  expect_equal(m$name, "METHOD")
  expect_equal(m$type, "relative")
  expect_equal(m$args, list(fish = 4, greg = 5))
  expect_equal(m$calculate_targets(x = 1, features = 1), 0.25)
  expect_equal(m$calculate_targets(x = 1, features = c(1, 3)), rep(0.25, 2))
})
