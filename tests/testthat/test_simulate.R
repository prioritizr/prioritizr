test_that("simulate_data (SpatRaster, single layer)", {
  skip_if_not_installed("fields")
  # create data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  # simulate data
  s <- simulate_data(r, n = 1, transform = plogis)
  # tests
  expect_inherits(s, "SpatRaster")
  expect_true(is_comparable_raster(r, s[[1]]))
  expect_equal(terra::nlyr(s), 1)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(terra::values(s) <= 1))
  expect_true(all(is.finite(c(terra::values(s)))))
})

test_that("simulate_data (SpatRaster, multiple layers)", {
  skip_if_not_installed("fields")
  # create data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  # simulate data
  s <- simulate_data(r, n = 2, transform = plogis)
  # tests
  expect_inherits(s, "SpatRaster")
  expect_true(is_comparable_raster(r, s[[1]]))
  expect_equal(terra::nlyr(s), 2)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(terra::values(s) <= 1))
  expect_true(all(is.finite(c(terra::values(s)))))
})

test_that("simulate_species (SpatRaster)", {
  skip_if_not_installed("fields")
  # create data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  # simulate data
  s <- simulate_species(r, n = 2)
  # tests
  expect_inherits(s, "SpatRaster")
  expect_true(is_comparable_raster(r, s[[1]]))
  expect_equal(terra::nlyr(s), 2)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(terra::values(s) <= 1))
  expect_true(all(is.finite(terra::values(s))))
})

test_that("simulate_cost (SpatRaster)", {
  skip_if_not_installed("fields")
  # create data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  # simulate data
  s <- simulate_cost(r, n = 2)
  # tests
  expect_inherits(s, "SpatRaster")
  expect_true(is_comparable_raster(r, s[[1]]))
  expect_equal(terra::nlyr(s), 2)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(is.finite(terra::values(s))))
})

test_that("simulate_data (RasterStack)", {
  skip_if_not_installed("fields")
  # create data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  # simulate data
  expect_warning(
    s1 <- simulate_data(raster::stack(r), n = 1, transform = plogis),
    "deprecated"
  )
  s2 <- terra::rast(s1)
  # tests
  expect_inherits(s1, "RasterStack")
  expect_true(is_comparable_raster(r, s2[[1]]))
  expect_equal(terra::nlyr(s2), 1)
  expect_true(all(terra::values(s2) >= 0))
  expect_true(all(terra::values(s2) <= 1))
  expect_true(all(is.finite(c(terra::values(s2)))))
})

test_that("simulate_species (RasterStack)", {
  skip_if_not_installed("fields")
  # create data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  # simulate data
  expect_warning(
    s1 <- simulate_species(raster::stack(r), n = 2),
    "deprecated"
  )
  s2 <- terra::rast(s1)
  # tests
  expect_inherits(s1, "RasterStack")
  expect_true(is_comparable_raster(r, s2[[1]]))
  expect_equal(terra::nlyr(s2), 2)
  expect_true(all(terra::values(s2) >= 0))
  expect_true(all(terra::values(s2) <= 1))
  expect_true(all(is.finite(terra::values(s2))))
})

test_that("simulate_cost (RasterStack)", {
  skip_if_not_installed("fields")
  # create data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  # simulate data
  expect_warning(
    s1 <- simulate_cost(raster::stack(r), n = 2),
    "deprecated"
  )
  s2 <- terra::rast(s1)
  # tests
  expect_inherits(s1, "RasterStack")
  expect_true(is_comparable_raster(r, s2[[1]]))
  expect_equal(terra::nlyr(s2), 2)
  expect_true(all(terra::values(s2) >= 0))
  expect_true(all(is.finite(terra::values(s2))))
})
