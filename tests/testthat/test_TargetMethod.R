test_that("relative type", {
  # load data
  pu_data <- get_sim_complex_pu_raster()
  ft_data <- get_sim_complex_features()[[1:5]]
  # build problem
  p <- problem(pu_data, ft_data)
  # create new target setting method that is for fixed 20% targets
  m <- new_target_method(
    name = "METHOD",
    type = "relative",
    fun = function(x, features, prop, call) {
      rep(prop, length(features))
    },
    args = list(prop = 0.2)
  )
  # run tests
  print(m)
  expect_inherits(m, "TargetMethod")
  expect_equal(m$name, "METHOD")
  expect_equal(m$type, "relative")
  expect_equal(m$args, list(prop = 0.2))
  expect_equal(
    m$calculate_targets(x = p, features = 1),
    0.2
  )
  expect_equal(
    m$calculate_targets(x = p, features = c(1, 3)),
    rep(0.2, 2)
  )
  expect_equal(
    m$calculate_relative_targets(x = p, features = c(1, 3)),
    rep(0.2, 2)
  )
  expect_equal(
    m$calculate_absolute_targets(x = p, features = c(1, 3)),
    0.2 * unname(c(p$feature_abundances_in_total_units()[c(1, 3), 1]))
  )
  expect_equal(
    m$calculate_targets_km2(x = p, features = c(1, 3)),
    0.2 * unname(c(p$feature_abundances_km2_in_total_units()[c(1, 3), 1]))
  )
})

test_that("absolute type", {
  # load data
  pu_data <- get_sim_complex_pu_raster()
  ft_data <- get_sim_complex_features()[[1:5]]
  # build problem
  p <- problem(pu_data, ft_data)
  # create new target setting method that is for absolute 5 targets
  m <- new_target_method(
    name = "METHOD",
    type = "absolute",
    fun = function(x, features, threshold, call) {
      rep(threshold, length(features))
    },
    args = list(threshold = 5)
  )
  # run tests
  print(m)
  expect_inherits(m, "TargetMethod")
  expect_equal(m$name, "METHOD")
  expect_equal(m$type, "absolute")
  expect_equal(m$args, list(threshold = 5))
  expect_equal(
    m$calculate_targets(x = p, features = 1),
    5
  )
  expect_equal(
    m$calculate_targets(x = p, features = c(1, 3)),
    rep(5, 2)
  )
  expect_equal(
    m$calculate_relative_targets(x = p, features = c(1, 3)),
    5 / unname(c(p$feature_abundances_in_total_units()[c(1, 3), 1]))
  )
  expect_equal(
    m$calculate_absolute_targets(x = p, features = c(1, 3)),
    rep(5, 2)
  )
  expect_equal(
    m$calculate_targets_km2(x = p, features = c(1, 3)),
    (5 / unname(c(p$feature_abundances_in_total_units()[c(1, 3), 1]))) *
    unname(c(p$feature_abundances_km2_in_total_units()[c(1, 3), 1]))
  )
})
