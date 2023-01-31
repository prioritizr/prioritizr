context("zones")

test_that("zones (SpatRaster)", {
  # import data
  sim_features <- get_sim_features()
  # create zones data
  x <- zones(
    sim_features, sim_features, sim_features,
    zone_names = c("z1", "z2", "z3"),
    feature_names = names(sim_features)
  )
  # run tests
  suppressMessages(print(x))
  expect_is(x, "Zones")
  expect_is(x, "ZonesSpatRaster")
  expect_equal(number_of_zones(x), 3)
  expect_equal(number_of_features(x), 5)
  expect_equal(zone_names(x), c("z1", "z2", "z3"))
  expect_equal(feature_names(x), names(sim_features))
  expect_tidy_error(zones(sim_features[[1:2]], sim_features[[1:3]]))
  expect_tidy_error(zones(sim_features[[1]], zone_names = c("z1", "z2")))
  expect_tidy_error(zones(sim_features[[1]], feature_names = c("z1", "z2")))
})

test_that("zones (Raster)", {
  # import data
  sim_features <- raster::stack(get_sim_features())
  # create zones data
  expect_warning(
    x <- zones(
      sim_features, sim_features, sim_features,
      zone_names = c("z1", "z2", "z3"),
      feature_names = names(sim_features)
    ),
    "deprecated"
  )
  # run tests
  suppressMessages(print(x))
  expect_is(x, "Zones")
  expect_is(x, "ZonesRaster")
  expect_equal(number_of_zones(x), 3)
  expect_equal(number_of_features(x), 5)
  expect_equal(zone_names(x), c("z1", "z2", "z3"))
  expect_equal(feature_names(x), names(sim_features))
  expect_tidy_error(
    expect_warning(
      zones(sim_features[[1:2]], sim_features[[1:3]]),
      "deprecated"
    )
  )
  expect_tidy_error(
    expect_warning(
      zones(sim_features[[1]], zone_names = c("z1", "z2")),
      "deprecated"
    )
  )
  expect_tidy_error(
    expect_warning(
      zones(sim_features[[1]], feature_names = c("z1", "z2")),
      "deprecated"
    )
  )
})

test_that("zones (character)", {
  # create zones data
  x <- zones(
    c("a1", "b1"), c("a2", "b2"), c("a3", "b3"),
    zone_names = c("z1", "z2", "z3"),
    feature_names = c("a", "b")
  )
  # run tests
  suppressMessages(print(x))
  expect_is(x, "Zones")
  expect_is(x, "ZonesCharacter")
  expect_equal(number_of_zones(x), 3)
  expect_equal(number_of_features(x), 2)
  expect_equal(zone_names(x), c("z1", "z2", "z3"))
  expect_equal(feature_names(x), c("a", "b"))
  expect_tidy_error(zones(c("a1", "b1"), c("a2")))
  expect_tidy_error(zones(c("a1"), zone_names = c("z1", "z2")))
  expect_tidy_error(zones(c("a1"), feature_names = c("a", "b")))
})
