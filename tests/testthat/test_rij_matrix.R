context('rij matrix functions') 

test_that('cost=Raster, features=RasterLayer', {
  data(sim_pu, sim_features)
  rij_matrix <- rij_matrix(sim_pu, sim_features[[1]])
  expect_equal(rij_matrix, {
    m <- matrix(sim_features[[1]][seq_len(raster::ncell(sim_pu))], ncol=1)
    m[is.na(m)] <- 0
    m <- as(m, 'dgTMatrix')
    m
  })
})


test_that('cost=Raster, features=RasterStack', {
  data(sim_pu, sim_features)
  rij_matrix <- rij_matrix(sim_pu, sim_features)
  expect_equal(rij_matrix, {
    m <- sim_features[seq_len(raster::ncell(sim_pu))]
    m[is.na(m)] <- 0
    m <- as(m, 'dgTMatrix')
    m
  })
})

test_that('cost=SpatialPolygons, features=RasterStack', {
  data(tas_pu, tas_features)
  tas_pu <- tas_pu[10:20,]
  tas_features <- tas_features[[seq_len(10)]]
  rij_matrix_1 <- rij_matrix(tas_pu, tas_features, fun=mean, na.rm=TRUE, velox=FALSE)
  rij_matrix_2 <- rij_matrix(tas_pu, tas_features, fun=mean, velox=TRUE)
  set_number_of_threads(2)
  rij_matrix_3 <- suppressWarnings(rij_matrix(tas_pu, tas_features, fun=mean, na.rm=TRUE, velox=FALSE))
  rij_matrix_4 <- suppressWarnings(rij_matrix(tas_pu, tas_features, fun=mean, velox=TRUE))
  set_number_of_threads(1)
  expect_equal(rij_matrix_1, {
    m <- raster::extract(tas_features, tas_pu, fun=mean, na.rm=TRUE, sp=FALSE)
    m <- as(m, 'dgTMatrix')
    m
  })
  expect_equal(rij_matrix_1, rij_matrix_2)
  expect_equal(rij_matrix_1, rij_matrix_3)
  expect_equal(rij_matrix_1, rij_matrix_4)  
})

 
test_that('cost=SpatialLines, features=RasterStack', {
  data(tas_pu, tas_features)
  tas_pu <- as(tas_pu[10:20,], 'SpatialLines')
  tas_features <- tas_features[[seq_len(10)]]
  rij_matrix_1 <- rij_matrix(tas_pu, tas_features, fun=mean, na.rm=TRUE, velox=FALSE)
  set_number_of_threads(2)
  rij_matrix_2 <- suppressWarnings(rij_matrix(tas_pu, tas_features, fun=mean, na.rm=TRUE, velox=FALSE))
  set_number_of_threads(1)
  expect_equal(rij_matrix_1, {
    m <- raster::extract(tas_features, tas_pu, fun=mean, na.rm=TRUE, sp=FALSE)
    m <- as(m, 'dgTMatrix')
    m
  })
  expect_equal(rij_matrix_1, rij_matrix_2)
})
 
test_that('cost=SpatialPoints, features=RasterStack', {
  data(tas_pu, tas_features)
  tas_pu <- rgeos::gCentroid(tas_pu[10:20,], byid=TRUE)
  tas_features <- tas_features[[seq_len(10)]]
  rij_matrix <- rij_matrix(tas_pu, tas_features)
  expect_equal(rij_matrix, {
    m <- raster::extract(tas_features, tas_pu, na.rm=TRUE, sp=FALSE)
    m <- as(m, 'dgTMatrix')
    m
  })
})
 
