#' @include internal.R generics.R
NULL

#' @name connected_matrix
#' @rdname connected_matrix
methods::setMethod(
  'connected_matrix',
  signature(x='Raster'),
  function(x, directions=4, ...) {
    assertthat::assert_that(inherits(x, 'Raster'))
    assertthat::assert_that(
      isTRUE(raster::nlayers(x)==1))
  
  # indices of cells with finite values
  include <- raster::Which(is.finite(x), cells=TRUE)
  # find the neighboring indices of these cells
  m <- raster::adjacent(x, include, pairs=TRUE, directions = directions)
  m <- m[(m[,1] %in% include) & (m[,2] %in% include),]
  # coerce to sparse matrix object
  m <- Matrix::forceSymmetric(Matrix::sparseMatrix(i=m[,1], j=m[,2], x=rep(1, nrow(m)),
                                                   dims=rep(raster::ncell(x), 2)))
  # return included planning units
  return(m[include,include])
})

#' @name connected_matrix
#' @rdname connected_matrix
methods::setMethod(
  'connected_matrix',
  signature(x='SpatialPolygons'),
  function(x, ...) {
    assertthat::assert_that(inherits(x, 'SpatialPolygons'))
  
  m <- rcpp_list_to_matrix_indices(rgeos::gIntersects(x, byid=TRUE,
                                                   returnDense=FALSE))
  m <- Matrix::forceSymmetric(Matrix::sparseMatrix(i=m$i, j=m$j, x=m$x))
  # return matrix
  return(m)
})

#' @name connected_matrix
#' @rdname connected_matrix
methods::setMethod(
  'connected_matrix',
  signature(x='SpatialLines'),
  function(x,  ...) {
    assertthat::assert_that(inherits(x, 'SpatialLines'))

  m <- rcpp_list_to_matrix_indices(rgeos::gIntersects(x, byid=TRUE, 
                                                      returnDense=FALSE))
  m <- Matrix::forceSymmetric(Matrix::sparseMatrix(i=m$i, j=m$j, x=m$x))
  # return matrix
  return(m)  
})

#' @name connected_matrix
#' @rdname connected_matrix
methods::setMethod(
  'connected_matrix',
  signature(x='SpatialPoints'),
  function(x, distance, ...) {
    assertthat::assert_that(inherits(x, 'SpatialPoints'),
                              assertthat::is.scalar(distance),
                              isTRUE(distance >= 0))
  m <- rgeos::gWithinDistance(x, dist=distance, byid=TRUE)
  m[m] <- 1
  diag(m) <- 0
  m <- Matrix::Matrix(m,sparse=TRUE)
  m <- Matrix::forceSymmetric(m)
  # return matrix
  return(m)  
})
 
