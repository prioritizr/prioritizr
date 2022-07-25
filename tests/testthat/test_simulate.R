context("simulation")

test_that("simulate_data (single output layer)", {
  # simulate data
  r <- raster::raster(matrix(1, ncol = 10, nrow = 10))
  s <- simulate_data(r, n = 1, transform = plogis)
  # check simulated data
  expect_true(raster::compareRaster(r, s[[1]], res = TRUE, orig = TRUE,
                                    values = FALSE, tolerance = 1e-5))
  expect_equal(raster::nlayers(s), 1)
  expect_true(all(raster::values(s) >= 0))
  expect_true(all(raster::values(s) <= 1))
  expect_true(all(is.finite(raster::values(s))))
})

test_that("simulate_data (multiple output layers)", {
  # simulate data
  r <- raster::raster(matrix(1, ncol = 10, nrow = 10))
  s <- simulate_data(r, n = 2, transform = plogis)
  # check simulated data
  expect_true(raster::compareRaster(r, s[[1]], res = TRUE, orig = TRUE,
                                    values = FALSE, tolerance = 1e-5))
  expect_equal(raster::nlayers(s), 2)
  expect_true(all(raster::values(s) >= 0))
  expect_true(all(raster::values(s) <= 1))
  expect_true(all(is.finite(raster::values(s))))
})

test_that("simulate_species", {
  # simulate data
  r <- raster::raster(matrix(1, ncol = 10, nrow = 10))
  s <- simulate_species(r, n = 2)
  # check simulated data
  expect_true(raster::compareRaster(r, s[[1]], res = TRUE, orig = TRUE,
                                    values = FALSE, tolerance = 1e-5))
  expect_equal(raster::nlayers(s), 2)
  expect_true(all(raster::values(s) >= 0))
  expect_true(all(raster::values(s) <= 1))
  expect_true(all(is.finite(raster::values(s))))
})

test_that("simulate_cost", {
  # simulate data
  r <- raster::raster(matrix(1, ncol = 10, nrow = 10))
  s <- simulate_cost(r, n = 2)
  # check simulated data
  expect_true(raster::compareRaster(r, s[[1]], res = TRUE, orig = TRUE,
                    values = FALSE, tolerance = 1e-5))
  expect_equal(raster::nlayers(s), 2)
  expect_true(all(raster::values(s) >= 0))
  expect_true(all(is.finite(raster::values(s))))
})
