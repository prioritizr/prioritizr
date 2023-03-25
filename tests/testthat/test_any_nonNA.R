test_that("x = default", {
  expect_tidy_error(any_nonNA(new_waiver()), "recognized")
})

test_that("x = numeric", {
  expect_true(any_nonNA(c(0, 1, 2, NA)))
  expect_true(any_nonNA(c(0L, 1L, 2L, NA)))
  expect_false(any_nonNA(c(NA_real_, NA_real_)))
  expect_false(any_nonNA(c(NA_integer_, NA_integer_)))
  expect_error(
    assert(any_nonNA(c(NA_real_, NA_real_))),
    "missing"
  )
})

test_that("x = character", {
  expect_true(any_nonNA(c("a", "b", "c", NA)))
  expect_false(any_nonNA(c(NA_character_, NA_character_)))
  expect_error(
    assert(any_nonNA(c(NA_character_, NA_character_))),
    "missing"
  )
})

test_that("x = factor", {
  expect_true(any_nonNA(factor(c("a", "b", "c", NA))))
  expect_false(any_nonNA(factor(c(NA_character_, NA_character_))))
  expect_error(
    assert(any_nonNA(factor(c(NA_character_, NA_character_)))),
    "missing"
  )
})

test_that("x = logical", {
  expect_true(any_nonNA(factor(c(TRUE, FALSE, FALSE, NA))))
  expect_false(any_nonNA(factor(c(NA, NA))))
  expect_error(
    assert(any_nonNA(factor(c(NA, NA)))),
    "missing"
  )
})

test_that("x = Matrix", {
  expect_true(any_nonNA(Matrix::Matrix(c(0, 1, NA))))
  expect_false(any_nonNA(Matrix::Matrix(c(NA, NA, NA))))
  expect_error(
    assert(any_nonNA(Matrix::Matrix(c(NA, NA, NA)))),
    "missing"
  )
})

test_that("x = matrix", {
  expect_true(any_nonNA(matrix(c(0, 1, NA))))
  expect_false(any_nonNA(matrix(c(NA, NA, NA))))
  expect_error(
    assert(any_nonNA(matrix(c(NA, NA, NA)))),
    "missing"
  )
})

test_that("x = data.frame", {
  expect_true(any_nonNA(data.frame(x = c(0, 1, NA), y = c(0L, 1L, NA))))
  expect_false(any_nonNA(data.frame(x = c(NA, NA, NA), y = c(0, 1, 2))))
  expect_error(
    assert(any_nonNA(data.frame(x = c(NA, NA), y = c(0, 1)))),
    "missing"
  )
})

test_that("x = sf", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(0L, NA), geom = g))
  y <- sf::st_as_sf(tibble::tibble(x = c(NA, NA), y = c(1, 2), geom = g))
  # tests
  expect_true(any_nonNA(x))
  expect_false(any_nonNA(y))
  expect_error(assert(any_nonNA(y)), "missing")
})

test_that("x = Spatial", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(0L, NA), geom = g))
  y <- sf::st_as_sf(tibble::tibble(x = c(NA, NA), y = c(1, 2), geom = g))
  # tests
  expect_true(any_nonNA(sf::as_Spatial(x)))
  expect_false(any_nonNA(sf::as_Spatial(y)))
  expect_error(
    assert(any_nonNA(sf::as_Spatial(y))),
    "missing"
  )
})

test_that("x = SpatRaster", {
  expect_true(any_nonNA(terra::rast(matrix(c(0, 1, NA)))))
  expect_false(any_nonNA(terra::rast(matrix(c(NA, NA, NA)))))
  expect_error(
    assert(any_nonNA(terra::rast(matrix(c(NA, NA, NA))))),
    "missing"
  )
})

test_that("x = Raster", {
  expect_true(any_nonNA(raster::raster(matrix(c(0, 1, NA)))))
  expect_false(any_nonNA(raster::raster(matrix(c(NA, NA, NA)))))
  expect_error(
    assert(any_nonNA(raster::raster(matrix(c(NA, NA, NA))))),
    "missing"
  )
})

test_that("x = ZonesSpatRaster", {
  # create data
  z1 <- zones(
    terra::rast(matrix(c(0, 1, NA))),
    terra::rast(matrix(c(0, 1, 2)))
  )
  z2 <- zones(
    terra::rast(matrix(c(0, 1, NA))),
    terra::rast(matrix(c(NA, NA, NA)))
  )
  # tests
  expect_true(any_nonNA(z1))
  expect_false(any_nonNA(z2))
  expect_error(
    assert(any_nonNA(z2)),
    "missing"
  )
})

test_that("x = ZonesRaster", {
  # create data
  expect_warning(
    z1 <- zones(
      raster::raster(matrix(c(0, 1, NA))),
      raster::raster(matrix(c(0, 1, 2)))
    ),
    "deprecated"
  )
  expect_warning(
    z2 <- zones(
      raster::raster(matrix(c(0, 1, NA))),
      raster::raster(matrix(c(NA, NA, NA)))
    ),
    "deprecated"
  )
  # tests
  expect_true(any_nonNA(z1))
  expect_false(any_nonNA(z2))
  expect_error(
    assert(any_nonNA(z2)),
    "missing"
  )
})
