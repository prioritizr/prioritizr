test_that("SpatRaster", {
  # create data
  x <- c(
    terra::rast(matrix(c(1, 0, 0, 0, NA, 0), byrow = TRUE, nrow = 3)),
    terra::rast(matrix(c(0, 1, 0, 0, NA, 0), byrow = TRUE, nrow = 3)),
    terra::rast(matrix(c(0, 0, 1, 0, NA, 1), byrow = TRUE, nrow = 3))
  )
  # convert to binary stack
  y <- category_layer(x)
  # tests
  expect_inherits(y, "SpatRaster")
  expect_equal(terra::nlyr(y), 1)
  expect_true(is_comparable_raster(x, y))
  expect_equal(c(terra::values(y)), c(1, 2, 3, 0, NA, 3))
})

test_that("Raster", {
  # create data
  x <- c(
    terra::rast(matrix(c(1, 0, 0, 1, NA, 0), byrow = TRUE, nrow = 3)),
    terra::rast(matrix(c(0, 1, 0, 0, NA, 0), byrow = TRUE, nrow = 3)),
    terra::rast(matrix(c(0, 0, 1, 0, NA, 1), byrow = TRUE, nrow = 3))
  )
  # convert to binary stack
  y <- category_layer(x)
  expect_warning(
    z <- category_layer(raster::stack(x)),
    "deprecated"
  )
  # tests
  expect_equal(
    terra::as.data.frame(y),
    terra::as.data.frame(terra::rast(z))
  )
})
