context("rij matrix functions")

test_that("cost=Raster, features=RasterLayer", {
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


test_that("cost=Raster, features=RasterStack", {
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

test_that("cost=SpatialPolygons, features=RasterStack", {
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
