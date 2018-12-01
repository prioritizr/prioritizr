context("rij_matrix")

test_that("cost=RasterLayer, features=RasterLayer", {
  # create data
  data(sim_pu_raster, sim_features)
  m <- rij_matrix(sim_pu_raster, sim_features[[1]])
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_equal(m, {
    included <- raster::Which(!is.na(sim_pu_raster), cells = TRUE)
    m <- as(matrix(sim_features[[1]][included], nrow = 1), "dgCMatrix")
    m
  })
})

test_that("cost=RasterLayer, features=RasterStack", {
  # create data
  data(sim_pu_raster, sim_features)
  m <- rij_matrix(sim_pu_raster, sim_features)
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_equal(m, {
    included <- raster::Which(!is.na(sim_pu_raster), cells = TRUE)
    m <- sim_features[included]
    m[is.na(m)] <- 0
    m <- Matrix::t(as(m, "dgCMatrix"))
    m
  })
})

test_that("cost=RasterStack, features=RasterStack", {
  # create data
  costs <- raster::stack(
    raster::raster(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7)),
    raster::raster(matrix(c(10, 10, 10, 10,  4,   1, NA), ncol = 7)))
  spp <- raster::stack(
    raster::raster(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    raster::raster(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    raster::raster(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    raster::raster(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7)))
  m <- rij_matrix(costs, spp)
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_equal(m, {
    included <- raster::Which(max(!is.na(costs)) > 0, cells = TRUE)
    m <- spp[included]
    m[is.na(m)] <- 0
    m <- Matrix::t(as(m, "dgCMatrix"))
    m
  })
})

test_that("cost=SpatialPolygons, features=RasterStack", {
  # skip test if velox not installed
  skip_if_not_installed("velox")
  # create data
  data(sim_pu_polygons, sim_features)
  m_1 <- rij_matrix(sim_pu_polygons, sim_features, fun = mean,
                             na.rm = TRUE, velox = FALSE)
  m_2 <- rij_matrix(sim_pu_polygons, sim_features, fun = mean,
                             velox = TRUE)
  set_number_of_threads(2)
  m_3 <- suppressWarnings(rij_matrix(sim_pu_polygons, sim_features,
                                              fun = mean, na.rm = TRUE,
                                              velox = FALSE))
  m_4 <- suppressWarnings(rij_matrix(sim_pu_polygons, sim_features,
                                              fun = mean, velox = TRUE))
  set_number_of_threads(1)
  # run tests
  expect_true(inherits(m_1, "dgCMatrix"))
  expect_true(inherits(m_2, "dgCMatrix"))
  expect_true(inherits(m_3, "dgCMatrix"))
  expect_true(inherits(m_4, "dgCMatrix"))
  expect_equal(m_1, {
    m <- raster::extract(sim_features, sim_pu_polygons, fun = mean,
                         na.rm = TRUE, sp = FALSE)
    m <- as(m, "dgCMatrix")
    Matrix::t(m)
  })
})

test_that("cost=SpatialLines, features=RasterStack", {
  # create data
  data(sim_pu_lines, sim_features)
  m_1 <- rij_matrix(sim_pu_lines, sim_features, fun = mean,
                             na.rm = TRUE, velox = FALSE)
  set_number_of_threads(2)
  m_2 <- suppressWarnings(rij_matrix(sim_pu_lines, sim_features,
                                              fun = mean, na.rm = TRUE,
                                              velox = FALSE))
  set_number_of_threads(1)
  # run tests
  expect_true(inherits(m_1, "dgCMatrix"))
  expect_true(inherits(m_2, "dgCMatrix"))
  expect_equal(m_1, {
    m <- raster::extract(sim_features, sim_pu_lines, fun = mean, na.rm = TRUE,
                         sp = FALSE)
    m <- as(m, "dgCMatrix")
    Matrix::t(m)
  })
  expect_equal(m_1, m_2)
})

test_that("cost=SpatialPoints, features=RasterStack", {
  # create data
  data(sim_pu_points, sim_features)
  m <- rij_matrix(sim_pu_points, sim_features)
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_equal(m, {
    m <- raster::extract(sim_features, sim_pu_points, na.rm = TRUE, sp = FALSE)
    m <- as(m, "dgCMatrix")
    Matrix::t(m)
  })
})
