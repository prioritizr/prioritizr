context('rij matrix functions') 

test_that('cost=Raster, features=RasterLayer', {
  data(sim_pu_raster, sim_features)
  rij_matrix <- rij_matrix(sim_pu_raster, sim_features[[1]])
  expect_equal(rij_matrix, {
    included <- raster::Which(!is.na(sim_pu_raster), cells=TRUE)
    m <- as(matrix(sim_features[[1]][included], nrow=1), 'dgTMatrix')
    m
  })
})


test_that('cost=Raster, features=RasterStack', {
  data(sim_pu_raster, sim_features)
  rij_matrix <- rij_matrix(sim_pu_raster, sim_features)
  expect_equal(rij_matrix, {
    included <- raster::Which(!is.na(sim_pu_raster), cells=TRUE)
    m <- sim_features[included]
    m[is.na(m)] <- 0
    m <- as(m, 'dgTMatrix')
    Matrix::t(m)
  })
})

test_that('cost=SpatialPolygons, features=RasterStack', {
  data(sim_pu_polygons, sim_features)
  rij_matrix_1 <- rij_matrix(sim_pu_polygons, sim_features, fun=mean,
    na.rm=TRUE, velox=FALSE)
  rij_matrix_2 <- rij_matrix(sim_pu_polygons, sim_features, fun=mean,
    velox=TRUE)
  set_number_of_threads(2)
  rij_matrix_3 <- suppressWarnings(rij_matrix(sim_pu_polygons, sim_features,
    fun=mean, na.rm=TRUE, velox=FALSE))
  rij_matrix_4 <- suppressWarnings(rij_matrix(sim_pu_polygons, sim_features, 
    fun=mean, velox=TRUE))
  set_number_of_threads(1)
  expect_equal(rij_matrix_1, {
    m <- raster::extract(sim_features, sim_pu_polygons, fun=mean, na.rm=TRUE,
      sp=FALSE)
    m <- as(m, 'dgTMatrix')
    Matrix::t(m)
  })
  expect_equal(rij_matrix_1, rij_matrix_2)
  expect_equal(rij_matrix_1, rij_matrix_3)
  expect_equal(rij_matrix_1, rij_matrix_4)  
})
 
test_that('cost=SpatialLines, features=RasterStack', {
  data(sim_pu_lines, sim_features)
  rij_matrix_1 <- rij_matrix(sim_pu_lines, sim_features, fun=mean, na.rm=TRUE,
    velox=FALSE)
  set_number_of_threads(2)
  rij_matrix_2 <- suppressWarnings(rij_matrix(sim_pu_lines, sim_features, 
    fun=mean, na.rm=TRUE, velox=FALSE))
  set_number_of_threads(1)
  expect_equal(rij_matrix_1, {
    m <- raster::extract(sim_features, sim_pu_lines, fun=mean, na.rm=TRUE,
      sp=FALSE)
    m <- as(m, 'dgTMatrix')
    Matrix::t(m)
  })
  expect_equal(rij_matrix_1, rij_matrix_2)
})
 
test_that('cost=SpatialPoints, features=RasterStack', {
  data(sim_pu_points, sim_features)
  rij_matrix <- rij_matrix(sim_pu_points, sim_features)
  expect_equal(rij_matrix, {
    m <- raster::extract(sim_features, sim_pu_points, na.rm=TRUE, sp=FALSE)
    m <- as(m, 'dgTMatrix')
    Matrix::t(m)
  })
})
 