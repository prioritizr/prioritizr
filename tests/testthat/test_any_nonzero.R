context("any_nonzero")

test_that("x = default", {
  expect_tidy_error(any_nonzero("a"), "recognized")
})

test_that("x = numeric", {
  expect_true(any_nonzero(c(0, 1)))
  expect_true(any_nonzero(c(0L, 1L)))
  expect_false(any_nonzero(c(0L, NA)))
  expect_false(any_nonzero(c(0, NA)))
  expect_error(assert(any_nonzero(c(0, NA))), "zero")
})

test_that("x = Matrix", {
  expect_true(any_nonzero(Matrix::Matrix(c(0, 1))))
  expect_true(any_nonzero(Matrix::Matrix(c(0L, 1L))))
  expect_false(any_nonzero(Matrix::Matrix(c(0L, NA))))
  expect_false(any_nonzero(Matrix::Matrix(c(0, NA))))
  expect_error(
    assert(any_nonzero(Matrix::Matrix(c(0L, NA)))),
    "zero"
  )
})

test_that("x = matrix", {
  expect_true(any_nonzero(matrix(c(0, 1))))
  expect_true(any_nonzero(matrix(c(0L, 1L))))
  expect_false(any_nonzero(matrix(c(0L, NA))))
  expect_false(any_nonzero(matrix(c(0, NA))))
  expect_error(
    assert(any_nonzero(matrix(c(0L, NA)))),
    "zero"
  )
})

test_that("x = data.frame", {
  expect_true(any_nonzero(data.frame(x = c(0, 1, NA), y = c(0L, 1L, NA))))
  expect_false(any_nonzero(data.frame(x = c(0, NA, NA), y = c(1, NA, NA))))
  expect_false(any_nonzero(data.frame(x = c(1, NA, NA), y = c(0L, NA, NA))))
  expect_error(
    assert(any_nonzero(data.frame(x = c(0, NA)))),
    "zero"
  )
})

test_that("x = sf", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, 1), y = c(0L, 1L), geom = g))
  y <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(0L, 1L), geom = g))
  z <- sf::st_as_sf(tibble::tibble(x = c(1, NA), y = c(NA, 0), geom = g))
  # tests
  expect_true(any_nonzero(x))
  expect_false(any_nonzero(y))
  expect_false(any_nonzero(z))
  expect_error(assert(any_nonzero(z)), "zero")
})

test_that("x = Spatial", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, 1), y = c(0L, 1L), geom = g))
  y <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(0L, 1L), geom = g))
  z <- sf::st_as_sf(tibble::tibble(x = c(1, NA), y = c(NA, 0), geom = g))
  # tests
  expect_true(any_nonzero(sf::as_Spatial(x)))
  expect_false(any_nonzero(sf::as_Spatial(y)))
  expect_false(any_nonzero(sf::as_Spatial(z)))
  expect_error(
    assert(any_nonzero(sf::as_Spatial(z))),
    "zero"
  )
})

test_that("x = SpatRaster", {
  expect_true(any_nonzero(terra::rast(matrix(c(0, 1, NA)))))
  expect_false(any_nonzero(terra::rast(matrix(c(0, 0, NA)))))
  expect_error(
    assert(any_nonzero(terra::rast(matrix(c(0, 0, NA))))),
    "zero"
  )
})

test_that("x = Raster", {
  expect_true(any_nonzero(raster::raster(matrix(c(0, 1, NA)))))
  expect_false(any_nonzero(raster::raster(matrix(c(0, 0, NA)))))
  expect_error(
    assert(any_nonzero(raster::raster(matrix(c(0, 0, NA))))),
    "zero"
  )
})

test_that("x = ZonesRaster", {
  # create data
  z1 <- zones(
    terra::rast(matrix(c(0, 1, NA))),
    terra::rast(matrix(c(0, NA, 1)))
  )
  z2 <- zones(
    terra::rast(matrix(c(0, 1, NA))),
    terra::rast(matrix(c(0, NA, 0)))
  )
  # tests
  expect_true(any_nonzero(z1))
  expect_false(any_nonzero(z2))
  expect_error(
    assert(any_nonzero(z2)),
    "zero"
  )
})

test_that("x = ZonesSpatRaster", {
  # create data
  expect_warning(
    z1 <- zones(
      raster::raster(matrix(c(0, 1, NA))),
      raster::raster(matrix(c(0, NA, 1)))
    ),
    "deprecated"
  )
  expect_warning(
    z2 <- zones(
      raster::raster(matrix(c(0, 1, NA))),
      raster::raster(matrix(c(0, NA, 0)))
    ),
    "deprecated"
  )
  # tests
  expect_true(any_nonzero(z1))
  expect_false(any_nonzero(z2))
  expect_error(
    assert(any_nonzero(z2)),
    "zero"
  )
})
