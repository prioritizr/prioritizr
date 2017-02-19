context('connected matrix')

test_that('RasterLayer', {
  # data
  x <- raster::raster(matrix(c(NA, 2:9), ncol=3),
               xmn=0, ymn=0, xmx=6, ymx=3)
  m <- connected_matrix(x, directions=4)
  s <- boundary_matrix(raster::rasterToPolygons(x, n=4))
  s[s > 0] <- 1
  # tests
  expect_true(inherits(m, 'dsCMatrix'))
  expect_true(all(as.matrix(m)[upper.tri(m)] == as.matrix(s)[upper.tri(s)]))
})

test_that('SpatialPolygons', {
  # data
  r <- raster::raster(matrix(0:8, byrow=TRUE, ncol=3),
                      xmn=0, xmx=3, ymn=0, ymx=3)
  s <- connected_matrix(r, directions=8)
  x <- raster::rasterToPolygons(r, n=4)
  m <- connected_matrix(x)
  # tests
  expect_true(inherits(m, 'dsCMatrix'))
  expect_true(all(as.matrix(m)[upper.tri(m)] == as.matrix(s)[upper.tri(s)]))
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
      7,7), ncol=2, byrow=TRUE)))),
    sp::Lines(ID='4',list(sp::Line(matrix(
      c(
      0,1,
      4,1), ncol=2, byrow=TRUE))))))
  m <- connected_matrix(x)
  s <- Matrix::sparseMatrix(
    i=c(1, 1),
    j=c(2, 4),
    x=c(1, 1),
    dims=c(4,4), symmetric=TRUE)
  # tests
  expect_true(inherits(m, 'dsCMatrix'))
  expect_true(all(m==s))
})

test_that('SpatialPoints', {
  # data
  x <- sp::SpatialPoints(matrix(c(
    0,0,
    0,5,
    0,10,
    100,100
  ), byrow=TRUE, ncol=2))
  m <- connected_matrix(x, distance=6)
  s <- Matrix::sparseMatrix(
    i=c(1,2),
    j=c(2,3),
    x=c(1,1),
    dims=c(4,4), symmetric=TRUE, giveCsparse=FALSE)
  # tests
  expect_true(inherits(m, 'dsCMatrix'))
  expect_true(all(m==s))
})
