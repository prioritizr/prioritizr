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
  expect_inherits(x, "Zones")
  expect_inherits(x, "ZonesSpatRaster")
  expect_equal(number_of_zones(x), 3)
  expect_equal(number_of_features(x), 5)
  expect_equal(zone_names(x), c("z1", "z2", "z3"))
  expect_equal(feature_names(x), names(sim_features))
  expect_tidy_error(zones(sim_features[[1:2]], sim_features[[1:3]]))
  expect_tidy_error(zones(sim_features[[1]], zone_names = c("z1", "z2")))
  expect_tidy_error(zones(sim_features[[1]], feature_names = c("z1", "z2")))
  # as.list tests
  l <- as.list(x)
  expect_inherits(l, "list")
  expect_length(l, 3)
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
  expect_inherits(x, "Zones")
  expect_inherits(x, "ZonesRaster")
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
  # as.list tests
  l <- as.list(x)
  expect_inherits(l, "list")
  expect_length(l, 3)
  # categorical rasters
  r <- terra::rast(nrows = 10, ncols = 10)
  terra::values(r) <- sample(3, terra::ncell(r), replace = TRUE)
  cls <- data.frame(id = 1:3, cover = c("forest", "water", "urban"))
  levels(r) <- cls
  expect_error(
    zones(r, r),
    "categorical"
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
  expect_inherits(x, "Zones")
  expect_inherits(x, "ZonesCharacter")
  expect_equal(number_of_zones(x), 3)
  expect_equal(number_of_features(x), 2)
  expect_equal(zone_names(x), c("z1", "z2", "z3"))
  expect_equal(feature_names(x), c("a", "b"))
  expect_tidy_error(zones(c("a1", "b1"), c("a2")))
  expect_tidy_error(zones(c("a1"), zone_names = c("z1", "z2")))
  expect_tidy_error(zones(c("a1"), feature_names = c("a", "b")))
})

test_that("class coercion (as.ZonesSpatRaster <-> as.ZonesRaster", {
  # import data
  sim_features <- get_sim_features()
  # create zones data
  x <- zones(
    sim_features, sim_features, sim_features,
    zone_names = c("z1", "z2", "z3"),
    feature_names = names(sim_features)
  )
  # convert to ZonesRaster
  y <- as.ZonesRaster(x)
  x2 <- as.ZonesSpatRaster(y)
  # tests
  expect_inherits(x, "Zones")
  expect_inherits(x, "ZonesSpatRaster")
  expect_inherits(y, "Zones")
  expect_inherits(y, "ZonesRaster")
  expect_inherits(x2, "Zones")
  expect_inherits(x2, "ZonesSpatRaster")
  expect_equal(attr(x, "zone_names"), attr(y, "zone_names"))
  expect_equal(attr(x, "zone_names"), attr(x2, "zone_names"))
  expect_equal(attr(x, "feature_names"), attr(y, "feature_names"))
  expect_equal(attr(x, "feature_names"), attr(x2, "feature_names"))
  expect_equal(length(x), length(y))
  expect_equal(length(x), length(x2))
  for (i in number_of_zones(x)) {
    expect_equal(
      terra::as.data.frame(x[[1]]), terra::as.data.frame(x2[[1]])
    )
  }
})
