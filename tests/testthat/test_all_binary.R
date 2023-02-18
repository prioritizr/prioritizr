context("all_binary")

test_that("x = default", {
  expect_tidy_error(all_binary("a"), "recognized")
})

test_that("x = numeric", {
  expect_true(all_binary(c(0, 1)))
  expect_true(all_binary(c(0L, 1L)))
  expect_true(all_binary(c(0L, NA, 1L)))
  expect_true(all_binary(c(0, NA, 1)))
  expect_false(all_binary(c(-1, 0, 1)))
  expect_error(assert(all_binary(c(-1, 0, 1))), "binary")
})

test_that("x = Matrix", {
  expect_true(all_binary(Matrix::Matrix(c(0, 1))))
  expect_true(all_binary(Matrix::Matrix(c(0L, 1L))))
  expect_true(all_binary(Matrix::Matrix(c(0L, NA, 1L))))
  expect_true(all_binary(Matrix::Matrix(c(0, NA, 1))))
  expect_false(all_binary(Matrix::Matrix(c(-1, 0, 1))))
  expect_error(
    assert(all_binary(Matrix::Matrix(c(-1, 0, 1)))),
    "binary"
  )
})

test_that("x = matrix", {
  expect_true(all_binary(matrix(c(0, 1))))
  expect_true(all_binary(matrix(c(0L, 1L))))
  expect_true(all_binary(matrix(c(0L, NA, 1L))))
  expect_true(all_binary(matrix(c(0, NA, 1))))
  expect_false(all_binary(matrix(c(-1, 0, 1))))
  expect_error(
    assert(all_binary(matrix(c(-1, 0, 1)))),
    "binary"
  )
})

test_that("x = data.frame", {
  expect_true(all_binary(data.frame(x = c(0, 1), y = c(0L, 1L))))
  expect_true(all_binary(data.frame(x = c(0, 1, NA), y = c(0L, NA, 1L))))
  expect_false(all_binary(data.frame(x = c(0, 1, -1), y = c(0L, NA, 1L))))
  expect_error(
    assert(all_binary(data.frame(x = c(0, 1, -1)))),
    "binary"
  )
})

test_that("x = sf", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, 1), y = c(0L, 1L), geom = g))
  g <- sf::st_sfc(
    list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1)), sf::st_point(c(0, 1)))
  )
  y <- sf::st_as_sf(
    tibble::tibble(x = c(0, 1, NA), y = c(0L, 1L, NA), geom = g)
  )
  z <- y
  z$x[2] <- -1
  # tests
  expect_true(all_binary(x))
  expect_true(all_binary(y))
  expect_false(all_binary(z))
  expect_error(assert(all_binary(z)), "binary")
})

test_that("x = Spatial", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, 1), y = c(0L, 1L), geom = g))
  g <- sf::st_sfc(
    list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1)), sf::st_point(c(0, 1)))
  )
  y <- sf::st_as_sf(
    tibble::tibble(x = c(0, 1, NA), y = c(0L, 1L, NA), geom = g)
  )
  z <- y
  z$x[2] <- -1
  # tests
  expect_true(all_binary(sf::as_Spatial(x)))
  expect_true(all_binary(sf::as_Spatial(y)))
  expect_false(all_binary(sf::as_Spatial(z)))
  expect_error(
    assert(all_binary(sf::as_Spatial(z))),
    "binary"
  )
})

test_that("x = SpatRaster", {
  expect_true(all_binary(terra::rast(matrix(c(0, 1)))))
  expect_true(all_binary(terra::rast(matrix(c(0L, 1L)))))
  expect_true(all_binary(terra::rast(matrix(c(0L, NA, 1L)))))
  expect_true(all_binary(terra::rast(matrix(c(0, NA, 1)))))
  expect_false(all_binary(terra::rast(matrix(c(-1, 0, 1)))))
  expect_error(
    assert(all_binary(terra::rast(matrix(c(-1, 0, 1))))),
    "binary"
  )
})

test_that("x = Raster", {
  expect_true(all_binary(raster::raster(matrix(c(0, 1)))))
  expect_true(all_binary(raster::raster(matrix(c(0L, 1L)))))
  expect_true(all_binary(raster::raster(matrix(c(0L, NA, 1L)))))
  expect_true(all_binary(raster::raster(matrix(c(0, NA, 1)))))
  expect_false(all_binary(raster::raster(matrix(c(-1, 0, 1)))))
  expect_error(
    assert(all_binary(raster::raster(matrix(c(-1, 0, 1))))),
    "binary"
  )
})
