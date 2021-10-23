context("intersecting_units")

test_that("x=Raster,y=Raster", {
  # generate data
  x <- raster::raster(matrix(c(NA, 2:16), byrow = TRUE, ncol = 4))
  x2 <- raster::stack(x)
  y <- x > 6
  y2 <- raster::stack(y)
  # run tests
  expect_equal(intersecting_units(x, y), c(7:16))
  expect_equal(intersecting_units(x, y), intersecting_units(x2, y2))
  # check that invalid arguments result in errors
  y_crs <- `crs<-`(y, value = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  expect_error(intersecting_units(x, y_crs))
  expect_error(intersecting_units(x, raster::disaggregate(y, fact = 2)))
})

test_that("x=Spatial,y=Spatial", {
  # generate data
  data(sim_pu_polygons)
  x <- sim_pu_polygons[1:10, ]
  y <- sim_pu_polygons[5:15, ]
  # run tests
  expect_equal(intersecting_units(x, y), 5:10)
  # check that invalid arguments result in errors
  y_crs <- `crs<-`(y, value = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  expect_error(intersecting_units(x, y_crs))
  expect_error(intersecting_units(x[1:5, ], x[50:55, ]))
})

test_that("x=Raster,y=Spatial", {
  # generate data
  data(sim_pu_raster, sim_pu_polygons)
  x <- sim_pu_raster
  cell_index <- as.integer(sample(as.integer(row.names(sim_pu_polygons@data)),
                                  5))
  cell_index <- sort(cell_index)
  pu_index <- match(cell_index, as.integer(row.names(sim_pu_polygons@data)))
  y <- sim_pu_polygons[pu_index, ]
  # run tests
  expect_equal(intersecting_units(x, y), cell_index)
  # check that invalid arguments result in errors
  y_crs <- `crs<-`(y, value = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  expect_error(intersecting_units(x, y_crs))
})

test_that("x=Spatial,y=Raster", {
  # generate data
  data(sim_pu_raster, sim_pu_polygons)
  m <- mean(sim_pu_polygons$cost, na.rm = TRUE)
  x <- sim_pu_polygons
  y <- sim_pu_raster < m
  # run tests
  expect_equal(intersecting_units(x, y), which(sim_pu_polygons$cost < m))
  # check that invalid arguments result in errors
  y_crs <- `crs<-`(y, value = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  expect_error(intersecting_units(x, y_crs))
})

test_that("x=sf,y=sf", {
  # generate data
  data(sim_pu_sf)
  x <- sim_pu_sf[1:10, ]
  y <- sim_pu_sf[5:15, ]
  # run tests
  expect_equal(intersecting_units(x, y), 5:10)
  # check that invalid arguments result in errors
  y_crs <- sf::st_set_crs(y, 4326)
  expect_error(intersecting_units(x, y_crs))
  expect_error(intersecting_units(x[1:5, ], x[50:55, ]))
})

test_that("x=Raster,y=sf", {
  # generate data
  data(sim_pu_raster, sim_pu_sf)
  x <- sim_pu_raster
  cell_index <- as.integer(sample(as.integer(row.names(sim_pu_sf)), 5))
  cell_index <- sort(cell_index)
  pu_index <- match(cell_index, as.integer(row.names(sim_pu_sf)))
  y <- sim_pu_sf[pu_index, ]
  # run tests
  expect_equal(intersecting_units(x, y), cell_index)
  # check that invalid arguments result in errors
  y_crs <- sf::st_set_crs(y, 4326)
  expect_error(intersecting_units(x, y_crs))
})

test_that("x=sf,y=Raster", {
  # generate data
  data(sim_pu_raster, sim_pu_sf)
  m <- mean(sim_pu_sf$cost, na.rm = TRUE)
  x <- sim_pu_sf
  y <- sim_pu_raster < m
  y2 <- raster::stack(y)
  # run tests
  expect_equal(intersecting_units(x, y), which(sim_pu_sf$cost < m))
  expect_equal(intersecting_units(x, y), intersecting_units(x, y2))
  # check that invalid arguments result in errors
  y_crs <- `crs<-`(y, value = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  expect_error(intersecting_units(x, y_crs))
})

test_that("x=sf,y=Spatial", {
  # generate data
  data(sim_pu_sf, sim_pu_polygons)
  x <- sim_pu_sf[1:10, ]
  y <- sim_pu_polygons[5:15, ]
  # run tests
  expect_equal(intersecting_units(x, y), 5:10)
  # check that invalid arguments result in errors
  y_crs <- `crs<-`(y, value = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  expect_error(intersecting_units(x, y_crs))
  expect_error(intersecting_units(x[1:5, ], x[50:55, ]))
})

test_that("x=Spatial,y=sf", {
  # generate data
  data(sim_pu_sf, sim_pu_polygons)
  x <- sim_pu_polygons[1:10, ]
  y <- sim_pu_sf[5:15, ]
  # run tests
  expect_equal(intersecting_units(x, y), 5:10)
  # check that invalid arguments result in errors
  y_crs <- sf::st_set_crs(y, 4326)
  expect_error(intersecting_units(x, y_crs))
  expect_error(intersecting_units(x[1:5, ], x[50:55, ]))
})

test_that("invalid input", {
  expect_error(intersecting_units(sim_pu_polygons@data, sim_pu_polygons))
})
