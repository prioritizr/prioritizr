context("zones")

test_that("zones (Raster input)", {
  # load data
  data(sim_features_zones)
  # make zones
  x <- zones(z1 = sim_features_zones[[1]], z2 = sim_features_zones[[2]],
             z3 = sim_features_zones[[3]])
  y <- as.Zones(list(z1 = sim_features_zones[[1]],
                     z2 = sim_features_zones[[2]],
                     z3 = sim_features_zones[[3]]))
  # run tests
  expect_is(x, "Zones")
  expect_is(y, "Zones")
  expect_is(x, "ZonesRasterStack")
  expect_is(y, "ZonesRasterStack")
  expect_equal(x, y)
  expect_error(zones(sim_features_zones[[1]], sim_features_zones[[2]][[1]]))
  expect_error(zones(z1 = sim_features_zones[[1]],
                     z1 = sim_features_zones[[1]]))
  expect_error(zones(sim_features_zones[[1]], sim_features_zones[[2]][[1:2]]))
})

test_that("zones (character input)", {
  # make zones
  x <- zones(z1 = c("a1", "b1"), z2 = c("a2", "b2"), z3 = c("a3", "b3"))
  y <- as.Zones(list(z1 = c("a1", "b1"), z2 = c("a2", "b2"),
                     z3 =  c("a3", "b3")))
  # run tests
  expect_is(x, "Zones")
  expect_is(y, "Zones")
  expect_is(x, "ZonesCharacter")
  expect_is(y, "ZonesCharacter")
  expect_equal(x, y)
  expect_error(zones(c("a1", "b1"), c("a2")))
  expect_error(zones(z1 = c("a1", "b1"), z1 = c("a2", "b2")))
  expect_error(zones(c("a1", "b1", "c1"), c("a2", NA, "c2")))
})
