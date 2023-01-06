context("binary_layer")

test_that("valid inputs", {
  # create data
  x <- raster::raster(matrix(c(1, 2, 4, 1, NA, 1)))
  # convert to binary stack
  y <- binary_stack(x)
  # run tests
  expect_is(y, "RasterStack")
  expect_equal(terra::nlyr(y), 4)
  expect_true(raster::compareRaster(x, y, stopiffalse = FALSE,
                                    tolerance = 1e-5,
                                    crs = TRUE))
  expect_equal(raster::values(y[[1]]), c(1, 0, 0, 1, NA, 1))
  expect_equal(raster::values(y[[2]]), c(0, 1, 0, 0, NA, 0))
  expect_equal(raster::values(y[[3]]), c(0, 0, 0, 0, NA, 0))
  expect_equal(raster::values(y[[4]]), c(0, 0, 1, 0, NA, 0))
})

test_that("invalid inputs", {
  expect_error({
    suppressWarnings(category_layer(raster::raster(matrix(c(NA, NA)))))
  })
  expect_error({
    category_layer(raster::raster(matrix(c(NA, 5.1))))
  })
  expect_error({
    category_layer(raster::raster(matrix(c(NA, -1))))
  })
  expect_error({
    category_layer(raster::raster(matrix(c(NA, 0))))
  })
})
