context("zones")

test_that("zones (Raster input)", {
  # load data
  sim_features <- get_sim_features()
  # make zones
  x <- zones(sim_features, sim_features, sim_features,
             zone_names = c("z1", "z2", "z3"),
             feature_names = names(sim_features))
  print(x)
  # run tests
  expect_is(x, "Zones")
  expect_is(x, "ZonesRaster")
  expect_equal(number_of_zones(x), 3)
  expect_equal(number_of_features(x), 5)
  expect_equal(zone_names(x), c("z1", "z2", "z3"))
  expect_equal(feature_names(x), names(sim_features))
  expect_error(zones(sim_features[[1:2]], sim_features[[1:3]]))
  expect_error(zones(sim_features[[1]], zone_names = c("z1", "z2")))
  expect_error(zones(sim_features[[1]], feature_names = c("z1", "z2")))
})

test_that("zones (character input)", {
  # make zones
  x <- zones(c("a1", "b1"), c("a2", "b2"), c("a3", "b3"),
             zone_names = c("z1", "z2", "z3"),
             feature_names = c("a", "b"))
  print(x)
  # run tests
  expect_is(x, "Zones")
  expect_is(x, "ZonesCharacter")
  expect_equal(number_of_zones(x), 3)
  expect_equal(number_of_features(x), 2)
  expect_equal(zone_names(x), c("z1", "z2", "z3"))
  expect_equal(feature_names(x), c("a", "b"))
  expect_error(zones(c("a1", "b1"), c("a2")))
  expect_error(zones(c("a1"), zone_names = c("z1", "z2")))
  expect_error(zones(c("a1"), feature_names = c("a", "b")))
})
