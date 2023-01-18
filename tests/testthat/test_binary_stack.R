context("binary_layer")

test_that("SpatRaster", {
  # create data
  x <- terra::rast(matrix(c(1, 2, 4, 1, NA, 1)))
  # convert to binary stack
  y <- binary_stack(x)
  # run tests
  expect_is(y, "SpatRaster")
  expect_equal(terra::nlyr(y), 4)
  expect_true(is_comparable_raster(x, y))
  expect_equal(c(terra::values(y[[1]])), c(1, 0, 0, 1, NA, 1))
  expect_equal(c(terra::values(y[[2]])), c(0, 1, 0, 0, NA, 0))
  expect_equal(c(terra::values(y[[3]])), c(0, 0, 0, 0, NA, 0))
  expect_equal(c(terra::values(y[[4]])), c(0, 0, 1, 0, NA, 0))
})

test_that("Raster", {
  # create data
  r <- terra::rast(matrix(c(1, 2, 4, 1, NA, 1)))
  # convert to binary stack
  x <- binary_stack(r)
  expect_warning(
    y <- binary_stack(raster::raster(r)),
    "deprecated"
  )
  # tests
  expect_equivalent(
    terra::as.data.frame(x),
    terra::as.data.frame(terra::rast(y))
  )
})
