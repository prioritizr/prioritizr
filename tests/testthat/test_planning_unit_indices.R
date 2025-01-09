test_that("x = sf (single cost column)", {
  # create data
  d <- get_sim_pu_polygons()
  # correct results
  x <- planning_unit_indices(d, "cost")
  y <- which(!is.na(d$cost))
  # tests
  expect_equal(x, y)
})

test_that("x = sf (single multiple column)", {
  # create data
  d <- get_sim_pu_polygons()
  d$cost2 <- 1
  d$cost2[1:6] <- NA_real_
  # correct results
  x <- planning_unit_indices(d, c("cost", "cost2"))
  y <- which(!is.na(d$cost) | !is.na(d$cost2))
  # tests
  expect_equal(x, y)
})

test_that("x = SpatRaster (single layer)", {
  # create data
  d <- get_sim_pu_raster()
  # correct results
  x <- planning_unit_indices(d)
  y <- terra::cells(terra::allNA(d), 0)[[1]]
  # tests
  expect_equal(x, y)
})

test_that("x = SpatRaster (multiple layers)", {
  # create data
  d <- get_sim_zones_pu_raster()
  # correct results
  x <- planning_unit_indices(d)
  y <- terra::cells(terra::allNA(d), 0)[[1]]
  # tests
  expect_equal(x, y)
})

test_that("x = data.frame", {
  # create data
  d <- matrix(runif(30), ncol = 3, nrow = 10)
  d[1, ] <- NA
  d[4, ] <- NA
  d[c(6, 8, 12, 23, 27)] <- NA
  d <- as.data.frame(d)
  # correct results
  x <- planning_unit_indices(d, names(d))
  y <- which(rowSums(is.na(d)) != ncol(d))
  # tests
  expect_equal(x, y)
})

test_that("x = matrix", {
  # create data
  d <- matrix(runif(30), ncol = 3, nrow = 10)
  d[1, ] <- NA
  d[4, ] <- NA
  d[c(6, 8, 12, 23, 27)] <- NA
  # correct results
  x <- planning_unit_indices(d)
  y <- which(rowSums(is.na(d)) != ncol(d))
  # tests
  expect_equal(x, y)
})

test_that("x = numeric", {
  # create data
  d <- runif(1)
  d[c(1, 4, 8)] <- NA_real_
  # correct results
  x <- planning_unit_indices(d)
  y <- which(!is.na(d))
  # tests
  expect_equal(x, y)
})

test_that("x = Spatial (single cost column)", {
  # create data
  d <- get_sim_pu_polygons()
  # correct results
  x <- planning_unit_indices(d, "cost")
  y <- planning_unit_indices(sf::as_Spatial(d), "cost")
  # tests
  expect_equal(x, y)
})

test_that("x = Spatial (multiple cost column)", {
  # create data
  d <- get_sim_pu_polygons()
  d$cost2 <- 1
  d$cost2[1:6] <- NA_real_
  # correct results
  x <- planning_unit_indices(d, c("cost", "cost2"))
  y <- planning_unit_indices(sf::as_Spatial(d), c("cost", "cost2"))
  # tests
  expect_equal(x, y)
})

test_that("x = Raster (single layer)", {
  # create data
  d <- get_sim_pu_raster()
  # correct results
  x <- planning_unit_indices(raster::raster(d))
  y <- planning_unit_indices(d)
  # tests
  expect_equal(x, y)
})

test_that("x = Raster (multiple layer)", {
  # create data
  d <- get_sim_zones_pu_raster()
  # correct results
  x <- planning_unit_indices(d)
  y <- planning_unit_indices(raster::stack(d))
  # tests
  expect_equal(x, y)
})

test_that("invalid input", {
  expect_error(
    planning_unit_indices(letters[1:4]),
    "not a recognized"
  )
})
