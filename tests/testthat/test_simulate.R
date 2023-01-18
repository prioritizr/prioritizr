context("simulate")

test_that("simulate_data (single output layer)", {
  skip_if_not_installed("fields")
  # create data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  # simulate data
  s <- simulate_data(r, n = 1, transform = plogis)
  # tests
  expect_true(is_comparable_raster(r, s[[1]]))
  expect_equal(terra::nlyr(s), 1)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(terra::values(s) <= 1))
  expect_true(all(is.finite(c(terra::values(s)))))
})

test_that("simulate_data (multiple output layers)", {
  skip_if_not_installed("fields")
  # create data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  # simulate data
  s <- simulate_data(r, n = 2, transform = plogis)
  # tests
  expect_true(is_comparable_raster(r, s[[1]]))
  expect_equal(terra::nlyr(s), 2)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(terra::values(s) <= 1))
  expect_true(all(is.finite(c(terra::values(s)))))
})

test_that("simulate_species", {
  skip_if_not_installed("fields")
  # create data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  # simulate data
  s <- simulate_species(r, n = 2)
  # tests
  expect_true(is_comparable_raster(r, s[[1]]))
  expect_equal(terra::nlyr(s), 2)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(terra::values(s) <= 1))
  expect_true(all(is.finite(terra::values(s))))
})

test_that("simulate_cost", {
  skip_if_not_installed("fields")
  # create data
  r <- terra::rast(matrix(1, ncol = 10, nrow = 10))
  # simulate data
  s <- simulate_cost(r, n = 2)
  # tests
  expect_true(is_comparable_raster(r, s[[1]]))
  expect_equal(terra::nlyr(s), 2)
  expect_true(all(terra::values(s) >= 0))
  expect_true(all(is.finite(terra::values(s))))
})
