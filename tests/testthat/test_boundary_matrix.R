context('boundary matrix')

test_that('SpatialPolygons (squares)', {
  # data
  x <- raster::rasterToPolygons(raster::raster(matrix(0:8, byrow=TRUE, ncol=3),
                                               xmn=0, xmx=3, ymn=0, ymx=3),
                                n=4)
  b <- boundary_matrix(x)
  s <- Matrix::sparseMatrix(
    i=c(0,0,0,1,1,1,2,2,3,3,3,4,4,5,5,6,6,7,7,8)+1,
    j=c(0,1,3,1,2,4,2,5,3,4,6,5,7,5,8,6,7,7,8,8)+1,
    x=c(2,1,1,1,1,1,2,1,1,1,1,1,1,1,1,2,1,1,1,2),
    symmetric=TRUE, giveCsparse=FALSE)
  # tests
  expect_true(all(b==s))
})

test_that('SpatialPolygons (hexagons)', {
  # data
  set.seed(401)
  x <- sp::HexPoints2SpatialPolygons(sp::spsample(as(raster::extent(c(0, 5, 0, 5)),
                                                     'SpatialPolygons'),
                                                  type='hexagonal', cellsize=1 * sqrt(3)))
  b <- boundary_matrix(x)
  b <- data.frame(b@j, b@i, b@x)
  b <- b[order(b[[1]], b[[2]]),]
  s <- data.frame(
    i=c(0,0,0,1,1,1,1,2,2,3,3,3,3,4,4,4,5,5,6,6,7),
    j=c(0,1,3,1,2,3,4,2,4,3,4,5,6,4,6,7,5,6,6,7,7),
    x=c(4,1,1,2,1,1,1,4,1,1,1,1,1,1,1,1,4,1,2,1,4))
  # tests
  expect_equal(b[[1]], s[[1]])
  expect_equal(b[[2]], s[[2]])
  expect_equal(b[[3]], s[[3]])
})

test_that('RasterLayer', {
  # data
  x <- raster::raster(matrix(c(NA, 2:9), ncol=3),
               xmn=0, ymn=0, xmx=6, ymx=3)
  b <- boundary_matrix(x)
  s <- boundary_matrix(raster::rasterToPolygons(x, n=4))
  # tests
  expect_true(all(b==s))
})

test_that('SpatialLines', {
  # data
  x <- sp::SpatialLines(list(
    sp::Lines(ID='1',list(sp::Line(matrix(
      c(
      0,0,
      1,1,
      2,2), ncol=2, byrow=TRUE)))),
    sp::Lines(ID='2',list(sp::Line(matrix(
      c(
      2,2,
      3,3,
      4,4), ncol=2, byrow=TRUE)))),
    sp::Lines(ID='3',list(sp::Line(matrix(
      c(
      5,5,
      7,7), ncol=2, byrow=TRUE)))))) 
  # tests
  expect_error(boundary_matrix(x))
})

test_that('SpatialPoints', {
  # data
  x <- sp::SpatialPoints(coords=matrix(runif(10), ncol=2))
  # tests
  expect_error(boundary_matrix(x))
})


