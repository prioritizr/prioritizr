test_that("x = SpatRaster, y = sf (polygons)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # calculations
  x <- fast_extract(sim_features, sim_pu_polygons, fun = "sum")
  # calculate correct results
  y <- exactextractr::exact_extract(
    sim_features, sim_pu_polygons, fun = "sum", progress = FALSE
  )
  # tests
  expect_equal(nrow(x), nrow(sim_pu_polygons))
  expect_equal(ncol(x), terra::nlyr(sim_features))
  expect_equal(x, as.matrix(y), ignore_attr = TRUE)
})

test_that("x = SpatRaster, y = sf (lines)", {
  # import data
  sim_pu_lines <- get_sim_pu_lines()
  sim_features <- get_sim_features()
  # calculationss
  x <- fast_extract(sim_features, sim_pu_lines, fun = "sum")
  # calculate correct results
  y <- as.matrix(
    terra::extract(
      x = sim_features,
      y = terra::vect(sim_pu_lines),
      ID = FALSE,
      fun = sum,
      na.rm = FALSE
    )
  )
  # tests
  expect_equal(nrow(x), nrow(sim_pu_lines))
  expect_equal(ncol(x), terra::nlyr(sim_features))
  expect_equal(x, y, ignore_attr = TRUE)
  # test for double counting of cells
  expect_equal(
    fast_extract(sim_features[[1]], sim_pu_lines[1, ], fun = "sum"),
    as.matrix(sim_features[[1]][1]),
    ignore_attr = TRUE
  )
})

test_that("x = SpatRaster, y = sf (points)", {
  # import data
  sim_pu_points <- get_sim_pu_points()
  sim_features <- get_sim_features()
  # calculations
  x <- fast_extract(sim_features, sim_pu_points, fun = "sum")
  # calculate correct results
  y <- as.matrix(
    terra::extract(
      x = sim_features,
      y = sf::st_coordinates(sim_pu_points)
    )
  )
  # tests
  expect_equal(nrow(x), nrow(sim_pu_points))
  expect_equal(ncol(x), terra::nlyr(sim_features))
  expect_equal(x, y, ignore_attr = TRUE)
})

test_that("x = SpatRaster, y = sfc", {
  # import
  set.seed(500)
  sim_pu_points <- get_sim_pu_points()
  sim_pu_lines <- get_sim_pu_lines()
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create data
  sim_data <- rbind(sim_pu_points, sim_pu_lines, sim_pu_polygons)
  idx <- sample.int(nrow(sim_data))
  sim_data <- sf::st_geometry(sim_data[idx, ])
  # calculations
  x <- fast_extract(sim_features, sim_data, fun = "sum")
  # calculate correct result
  y <- fast_extract(sim_features, sim_pu_points, fun = "sum")
  y <- rbind(y, fast_extract(sim_features, sim_pu_lines, fun = "sum"))
  y <- rbind(y, fast_extract(sim_features, sim_pu_polygons, fun = "sum"))
  y <- y[idx, , drop = FALSE]
  # tests
  expect_equal(nrow(x), length(sim_data))
  expect_equal(ncol(x), terra::nlyr(sim_features))
  expect_equal(x, y, ignore_attr = TRUE)
})

test_that("x = Raster, y = sfc", {
  # import
  sim_pu_points <- get_sim_pu_points()
  sim_pu_lines <- get_sim_pu_lines()
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create data
  sim_data <- rbind(sim_pu_points, sim_pu_lines, sim_pu_polygons)
  idx <- sample.int(nrow(sim_data))
  sim_data <- sf::st_geometry(sim_data[idx, ])
  # calculations
  expect_warning(
    x <- fast_extract(raster::stack(sim_features), sim_data, fun = "sum"),
    "deprecated"
  )
  # calculate correct result
  y <- fast_extract(sim_features, sim_data, fun = "sum")
  # tests
  expect_equal(nrow(x), length(sim_data))
  expect_equal(ncol(x), terra::nlyr(sim_features))
  expect_equal(x, y, ignore_attr = TRUE)
})

test_that("x = Raster, y = SpatialPolygonsDataFrame", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # calculations
  x <- fast_extract(sim_features, sim_pu_polygons, fun = "sum")
  expect_warning(
    expect_warning(
      y <- fast_extract(
        raster::stack(sim_features),
        sf::as_Spatial(sim_pu_polygons),
        fun = "sum"
      ),
      "deprecated"
    ),
    "deprecated"
  )
  # tests
  expect_equal(x, y)
})

test_that("x = Raster, y = SpatialLinesDataFrame", {
  # import data
  sim_pu_lines <- get_sim_pu_lines()
  sim_features <- get_sim_features()
  # calculations
  x <- fast_extract(sim_features, sim_pu_lines, fun = "sum")
  expect_warning(
    expect_warning(
      y <- fast_extract(
        raster::stack(sim_features),
        sf::as_Spatial(sim_pu_lines),
        fun = "sum"
      ),
      "deprecated"
    ),
    "deprecated"
  )
  # tests
  expect_equal(x, y)
})

test_that("x = Raster, y = SpatialPointsDataFrame", {
  # import data
  sim_pu_points <- get_sim_pu_points()
  sim_features <- get_sim_features()
  # calculations
  x <- fast_extract(sim_features, sim_pu_points, fun = "sum")
  expect_warning(
    expect_warning(
      y <- fast_extract(
        raster::stack(sim_features),
        sf::as_Spatial(sim_pu_points),
        fun = "sum"
      ),
      "deprecated"
    ),
    "deprecated"
  )
  # tests
  expect_equal(x, y)
})

test_that("x = Raster, y = sf (polygons)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # calculations
  expect_warning(
    x <- fast_extract(
      raster::stack(sim_features),
      sim_pu_polygons,
      fun = "sum"
    ),
    "deprecated"
  )
  # calculate correct results
  y <- exactextractr::exact_extract(
    sim_features, sim_pu_polygons, fun = "sum", progress = FALSE
  )
  # tests
  expect_equal(nrow(x), nrow(sim_pu_polygons))
  expect_equal(ncol(x), terra::nlyr(sim_features))
  expect_equal(x, as.matrix(y), ignore_attr = TRUE)
})

test_that("invalid inputs", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # invalid arguments
  expect_tidy_error(
    fast_extract(sim_features, sim_pu_polygons, fun = mean)
  )
  expect_tidy_error(
    fast_extract(sim_features, sim_pu_polygons, fun = "median")
  )
  expect_tidy_error(
    fast_extract(sim_features, sim_pu_polygons, fun = c("mean", "sum"))
  )
  # incorrect spatial data format
  expect_error(fast_extract(sim_pu_polygons, sim_pu_polygons))
  expect_error(fast_extract(sim_features, sim_features))
  # area out of extent
  bad_pt <- sf::st_sfc(sf::st_point(c(10, 10)))
  expect_tidy_error(fast_extract(sim_features, bad_pt))
  # different crs
  bad_crs <- suppressWarnings(sf::st_set_crs(sim_pu_polygons, 4326))
  expect_tidy_error(fast_extract(sim_features, bad_crs))
})

test_that("process in memory/on disk", {
  skip_on_cran()
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # in memory
  x <- fast_extract(sim_features, sim_pu_polygons)
  # on disk
  terra::terraOptions(todisk = TRUE)
  y <- fast_extract(sim_features, sim_pu_polygons)
  terra::terraOptions(todisk = FALSE)
  # tests
  expect_equal(round(x, 5), round(y, 5))
})
