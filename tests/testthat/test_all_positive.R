test_that("x = default", {
  expect_tidy_error(all_positive(new_waiver()), "recognized")
})

test_that("x = numeric", {
  expect_true(all_positive(c(0, 1, 2, NA)))
  expect_true(all_positive(c(0L, 1L, 2L, NA)))
  expect_false(all_positive(c(-1, NA, 0)))
  expect_false(all_positive(c(-1L, NA, 0L)))
  expect_error(
    assert(all_positive(c(0, -1, 2, NA))),
    "negative"
  )
})

test_that("x = Matrix", {
  expect_true(all_positive(Matrix::Matrix(c(0, 1, 2, NA))))
  expect_false(all_positive(Matrix::Matrix(c(-1, NA, 0))))
  expect_error(
    assert(all_positive(Matrix::Matrix(c(-1, NA, 0)))),
    "negative"
  )
})

test_that("x = matrix", {
  expect_true(all_positive(matrix(c(0, 1, 2, NA))))
  expect_false(all_positive(matrix(c(-1, NA, 0))))
  expect_error(
    assert(all_positive(matrix(c(-1, NA, 0)))),
    "negative"
  )
})

test_that("x = data.frame", {
  expect_true(all_positive(data.frame(x = c(0, 1, NA), y = c(0L, 2L, NA))))
  expect_false(all_positive(data.frame(x = c(0, 1, NA), y = c(-1, 1, 2))))
  expect_error(
    assert(all_positive(data.frame(x = c(0, 1, NA), y = c(-1, 1, 2)))),
    "negative"
  )
})

test_that("x = sf", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(0, NA), geom = g))
  y <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(-1, 2), geom = g))
  # tests
  expect_true(all_positive(x))
  expect_false(all_positive(y))
  expect_error(assert(all_positive(y)), "negative")
})

test_that("x = Spatial", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(2, NA), geom = g))
  y <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(-1, 2), geom = g))
  # tests
  expect_true(all_positive(sf::as_Spatial(x)))
  expect_false(all_positive(sf::as_Spatial(y)))
  expect_error(
    assert(all_positive(sf::as_Spatial(y))),
    "negative"
  )
})

test_that("x = SpatRaster", {
  expect_true(all_positive(terra::rast(matrix(c(0, 1, 2, NA)))))
  expect_false(all_positive(terra::rast(matrix(c(-1, NA, 0)))))
  expect_error(
    assert(all_positive(terra::rast(matrix(c(-1, NA, 0))))),
    "negative"
  )
})

test_that("x = Raster", {
  expect_true(all_positive(raster::raster(matrix(c(0, 1, 2, NA)))))
  expect_false(all_positive(raster::raster(matrix(c(-1, NA, 0)))))
  expect_error(
    assert(all_positive(raster::raster(matrix(c(-1, NA, 0))))),
    "negative"
  )
})

test_that("x = ZonesSpatRaster", {
  # create data
  z1 <- zones(
    terra::rast(matrix(c(0, 1, 2, NA))),
    terra::rast(matrix(c(0, 5, 2, NA)))
  )
  z2 <- zones(
    terra::rast(matrix(c(0, 1, 2, NA))),
    terra::rast(matrix(c(0, 1, -2, NA)))
  )
  # tests
  expect_true(all_positive(z1))
  expect_false(all_positive(z2))
  expect_error(
    assert(all_positive(z2)),
    "negative"
  )
})

test_that("x = ZonesRaster", {
  # create data
  expect_warning(
    z1 <- zones(
      raster::raster(matrix(c(0, 1, 2, NA))),
      raster::raster(matrix(c(0, 5, 2, NA)))
    ),
    "deprecated"
  )
  expect_warning(
    z2 <- zones(
      raster::raster(matrix(c(0, 1, 2, NA))),
      raster::raster(matrix(c(0, 1, -2, NA)))
    ),
    "deprecated"
  )
  # tests
  expect_true(all_positive(z1))
  expect_false(all_positive(z2))
  expect_error(
    assert(all_positive(z2)),
    "negative"
  )
})
