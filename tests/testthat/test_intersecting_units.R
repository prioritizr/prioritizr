context("intersecting_units")

test_that("x=Raster,y=Raster", {
  # generate data
  x <- raster::raster(matrix(c(NA, 2:16), byrow = TRUE, ncol = 4))
  y <- x > 6
  # run tests
  expect_equal(intersecting_units(x, y), c(7:16))
  # check that invalid arguments result in errors
  expect_error(intersecting_units(x, `crs<-`(y, sp::CRS("+init=epsg:4326"))))
  expect_error(intersecting_units(x, raster::disaggregate(y, fact = 2)))
})

test_that("x=Spatial,y=Spatial", {
  # generate data
  data(sim_pu_polygons)
  x <- sim_pu_polygons
  y <- sim_pu_polygons[10:15, ]
  # run tests
  expect_equal(intersecting_units(x, y), 10:15)
  # check that invalid arguments result in errors
  expect_error(intersecting_units(x, `crs<-`(y, sp::CRS("+init=epsg:4326"))))
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
  expect_error(intersecting_units(x, `crs<-`(y, sp::CRS("+init=epsg:4326"))))
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
  expect_error(intersecting_units(x, `crs<-`(y, sp::CRS("+init=epsg:4326"))))
})
