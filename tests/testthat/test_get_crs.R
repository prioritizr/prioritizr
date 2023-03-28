test_that("x = sf", {
  crs <- sf::st_crs(3875)
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))), crs = crs)
  x <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(0, NA), geom = g))
  expect_equal(get_crs(x), crs)
})

test_that("x = SpatRaster", {
  crs <- sf::st_crs(3875)
  x <- terra::rast(matrix(c(0, 1, 2, NA)), crs = crs[[2]])
  expect_equal(get_crs(x)[[2]], crs[[2]])
})

test_that("x = ZonesSpatRaster", {
  crs <- sf::st_crs(3875)
  x <- zones(
    terra::rast(matrix(c(0, 1, 2, NA)), crs = crs[[2]]),
    terra::rast(matrix(c(0, 5, 2, NA)), crs = crs[[2]])
  )
  expect_equal(get_crs(x)[[2]], crs[[2]])
})

test_that("x = Raster", {
  crs <- sf::st_crs(3875)
  x <- raster::raster(terra::rast(matrix(c(0, 1, 2, NA)), crs = crs[[2]]))
  expect_equal(get_crs(x), sf::st_crs(x@crs))
})

test_that("x = Spatial", {
  crs <- sf::st_crs(3875)
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))), crs = crs)
  x <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(0, NA), geom = g))
  x <- sf::as_Spatial(x)
  expect_equal(get_crs(x), sf::st_crs(x@proj4string))
})

test_that("x = ZonesRaster", {
  crs <- sf::st_crs(3875)
  expect_warning(
    x <- zones(
      raster::raster(matrix(c(0, 1, 2, NA))),
      raster::raster(matrix(c(0, 1, -2, NA)))
    ),
    "deprecated"
  )
  expect_equal(get_crs(x), sf::st_crs(x[[1]]@crs))
})
