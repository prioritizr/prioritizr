context("intersecting_units")

test_that("x = SpatRaster, y = SpatRaster", {
  # create data
  x <- terra::rast(matrix(c(NA, 2:16), byrow = TRUE, ncol = 4))
  y <- terra::as.int(x > 6)
  # run tests
  expect_equal(intersecting_units(x, y), c(7:16))
  # test for invalid
  y_crs <- y
  terra::crs(y_crs) <- as.character(sf::st_crs(4326)[[2]])
  expect_tidy_error(intersecting_units(x, y_crs))
  expect_tidy_error(intersecting_units(x, terra::disagg(y, fact = 2)))
})

test_that("x = sf, y = sf", {
  # create data
  x <- get_sim_pu_polygons()[1:10, ]
  y <- get_sim_pu_polygons()[5:15, ]
  # run tests
  expect_equal(intersecting_units(x, y), 5:10)
  # check that invalid arguments result in errors
  y_crs <- y
  suppressWarnings(sf::st_crs(y_crs) <- sf::st_crs(4326))
  expect_tidy_error(intersecting_units(x, y_crs))
  expect_tidy_error(intersecting_units(x[1:5, ], x[50:55, ]))
})

test_that("x = SpatRaster, y = sf", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_pu_polygons <- get_sim_pu_polygons()
  # create data
  x <- sim_pu_raster
  pu_index <- sort(as.integer(sample.int(nrow(sim_pu_polygons), 5)))
  y <- sim_pu_polygons[pu_index, ]
  # calculate correct result
  cell_index <-
    sim_pu_raster %>%
    terra::as.data.frame(xy = TRUE, na.rm = FALSE) %>%
    sf::st_as_sf(coords = c("x", "y")) %>%
    sf::st_set_crs(sf::st_crs(y)) %>%
    sf::st_intersects(x = sf::st_as_sf(y)) %>%
    unlist()
  # run tests
  expect_equal(intersecting_units(x, y), cell_index)
  # check that invalid arguments result in errors
  y_crs <- y
  suppressWarnings(sf::st_crs(y_crs) <- sf::st_crs(4326))
  expect_tidy_error(intersecting_units(x, y_crs))
})

test_that("x = sf, y = SpatRaster", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_pu_polygons <- get_sim_pu_polygons()
  # create data
  m <- mean(sim_pu_polygons$cost, na.rm = TRUE)
  x <- sim_pu_polygons
  y <- sim_pu_raster < m
  # run tests
  expect_equal(intersecting_units(x, y), which(sim_pu_polygons$cost < m))
  # check that invalid arguments result in errors
  y_crs <- y
  terra::crs(y_crs) <- as.character(sf::st_crs(4326)[[2]])
  expect_tidy_error(intersecting_units(x, y_crs))
})

test_that("x = Raster, y = Raster", {
  # create data
  x <- terra::rast(matrix(c(NA, 2:16), byrow = TRUE, ncol = 4))
  y <- terra::as.int(x > 6)
  # calculations
  expect_warning(
    r <- intersecting_units(raster::raster(x), raster::raster(y)),
    "deprecated"
  )
  # run tests
  expect_equal(intersecting_units(x, y), r)
})

test_that("x = Spatial, y = Spatial", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  # create data
  x <- sim_pu_polygons[1:10, ]
  y <- sim_pu_polygons[5:15, ]
  # calculations
  expect_warning(
    r <- intersecting_units(sf::as_Spatial(x), sf::as_Spatial(y)),
    "deprecated"
  )
  # run tests
  expect_equal(intersecting_units(x, y), r)
})

test_that("x = Raster, y = sf", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_pu_polygons <- get_sim_pu_polygons()
  # create data
  x <- sim_pu_raster
  pu_index <- sort(as.integer(sample.int(nrow(sim_pu_polygons), 5)))
  y <- sim_pu_polygons[pu_index, ]
  # calculations
  expect_warning(
    r <- intersecting_units(raster::raster(x), sf::as_Spatial(y)),
    "deprecated"
  )
  # run tests
  expect_equal(intersecting_units(x, y), r)
})

test_that("x = Spatial, y = Raster", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_pu_polygons <- get_sim_pu_polygons()
  # create data
  m <- mean(sim_pu_polygons$cost, na.rm = TRUE)
  x <- sim_pu_polygons
  y <- terra::as.int(sim_pu_raster < m)
  # calculations
  expect_warning(
    r <- intersecting_units(sf::as_Spatial(x), raster::raster(y)),
    "deprecated"
  )
  # run tests
  expect_equal(intersecting_units(x, y), r)
})

test_that("x = sf/Spatial, y = sf/Spatial", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  # create data
  x <- sim_pu_polygons[1:10, ]
  y <- sim_pu_polygons[5:15, ]
  # calculations
  expect_warning(
    r1 <- intersecting_units(sf::as_Spatial(x), y),
    "deprecated"
  )
  expect_warning(
    r2 <- intersecting_units(x, sf::as_Spatial(y)),
    "deprecated"
  )
  # run tests
  expect_equal(intersecting_units(x, y), r1)
  expect_equal(intersecting_units(x, y), r2)
})

test_that("invalid input", {
  data(iris)
  expect_tidy_error(intersecting_units(iris, iris))
})
