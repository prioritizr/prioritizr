test_that("SpatRaster", {
  # create data
  x <- terra::rast(matrix(c(1, 2, 4, 0, NA, 1)))
  # convert to binary stack
  y <- binary_stack(x)
  # run tests
  expect_inherits(y, "SpatRaster")
  expect_equal(terra::nlyr(y), 4)
  expect_named(y, c("1", "2", "3", "4"))
  expect_true(is_comparable_raster(x, y))
  expect_equal(c(terra::values(y[[1]])), c(1, 0, 0, 0, NA, 1))
  expect_equal(c(terra::values(y[[2]])), c(0, 1, 0, 0, NA, 0))
  expect_equal(c(terra::values(y[[3]])), c(0, 0, 0, 0, NA, 0))
  expect_equal(c(terra::values(y[[4]])), c(0, 0, 1, 0, NA, 0))
})

test_that("keep_all = FALSE", {
  # create data
  x <- terra::rast(matrix(c(1, 2, 4, 0, NA, 1)))
  # convert to binary stack
  y <- binary_stack(x, keep_all = FALSE)
  # run tests
  expect_inherits(y, "SpatRaster")
  expect_equal(terra::nlyr(y), 3)
  expect_named(y, c("1", "2", "4"))
  expect_true(is_comparable_raster(x, y))
  expect_equal(c(terra::values(y[[1]])), c(1, 0, 0, 0, NA, 1))
  expect_equal(c(terra::values(y[[2]])), c(0, 1, 0, 0, NA, 0))
  expect_equal(c(terra::values(y[[3]])), c(0, 0, 1, 0, NA, 0))
})
