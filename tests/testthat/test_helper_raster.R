test_that("is_single_patch_raster", {
  # create data
  x <- matrix(
    c(
      0, 1, 0, 0, 0,
      0, 1, 1, 1, 0,
      0, 0, 0, 1, 0
    ),
    ncol = 5, nrow = 3, byrow = TRUE
  )
  y <- matrix(
    c(
      1, 0, 0, 0, 0,
      0, 0, 1, 1, 0,
      0, 0, 0, 1, 0
    ), ncol = 5, nrow = 3, byrow = TRUE
  )
  # tests
  expect_true(is_single_patch_raster(terra::rast(x)))
  expect_false(is_single_patch_raster(terra::rast(y)))
})

test_that("is_checkerboard_raster", {
  # create data
  x <- matrix(
    c(
      1, 0, 0, 0, 1,
      0, 0, 1, 0, 0,
      0, 0, 0, 0, 0
    ),
    ncol = 5, nrow = 3, byrow = TRUE
  )
  y <- matrix(
    c(
      1, 1, 0, 0, 0,
      0, 0, 0, 1, 0,
      0, 1, 0, 0, 0
    ),
    ncol = 5, nrow = 3, byrow = TRUE
  )
  # tests
  expect_true(is_checkerboard_raster(terra::rast(x)))
  expect_false(is_checkerboard_raster(terra::rast(y)))
})

test_that("is_distinct_zones_raster", {
  # create data
  x <- matrix(
    c(
      2, 2, 0, 1, 1,
      0, 0, 0, 1, 1,
      0, 3, 0, 0, 0
    ),
    ncol = 5, nrow = 3, byrow = TRUE
  )
  y <- matrix(
    c(
      1, 1, 0, 3, 0,
      2, 1, 0, 3, 0,
      0, 0, 0, 0, 0
    ),
    ncol = 5, nrow = 3, byrow = TRUE
  )
  # tests
  expect_true(is_distinct_zones_raster(terra::rast(x)))
  expect_false(is_distinct_zones_raster(terra::rast(y)))
})
