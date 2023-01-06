context("simulation")

test_that("simulate_data (single output layer)", {
  skip_if_not_installed("fields")
  # simulate data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  s <- simulate_data(r, n = 1, transform = plogis)
  # check simulated data
  expect_true(raster::compareRaster(r, s[[1]], res = TRUE, orig = TRUE,
                                    values = FALSE, tolerance = 1e-5))
  expect_equal(terra::nlyr(s), 1)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(terra::values(s) <= 1))
  expect_true(all(is.finite(terra::values(s))))
})

test_that("simulate_data (multiple output layers)", {
  skip_if_not_installed("fields")
  # simulate data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  s <- simulate_data(r, n = 2, transform = plogis)
  # check simulated data
  expect_true(raster::compareRaster(r, s[[1]], res = TRUE, orig = TRUE,
                                    values = FALSE, tolerance = 1e-5))
  expect_equal(terra::nlyr(s), 2)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(terra::values(s) <= 1))
  expect_true(all(is.finite(terra::values(s))))
})

test_that("simulate_species", {
  skip_if_not_installed("fields")
  # simulate data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  s <- simulate_species(r, n = 2)
  # check simulated data
  expect_true(raster::compareRaster(r, s[[1]], res = TRUE, orig = TRUE,
                                    values = FALSE, tolerance = 1e-5))
  expect_equal(terra::nlyr(s), 2)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(terra::values(s) <= 1))
  expect_true(all(is.finite(terra::values(s))))
})

test_that("simulate_cost", {
  skip_if_not_installed("fields")
  # simulate data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  s <- simulate_cost(r, n = 2)
  # check simulated data
  expect_true(raster::compareRaster(r, s[[1]], res = TRUE, orig = TRUE,
                    values = FALSE, tolerance = 1e-5))
  expect_equal(terra::nlyr(s), 2)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(is.finite(terra::values(s))))
})
