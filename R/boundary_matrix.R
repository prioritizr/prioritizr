#' @include internal.R 
NULL


#' Boundary matrix
#'
#' Generate a boundary matrix showing the shared and exposed edges of 
#' planning units. \strong{This function assumes the 
#' data are in a projected coordinate system where distances can
#' be correctly calculated. Thus spatial data in a longitude/latitude
#' coordinate system should be reprojected prior to using this function.}
#' 
#' @param x \code{\link[raster]{Raster-class}},
#'   \code{\link[sp]{SpatialLines-class}}, or
#'   \code{\link[sp]{SpatialPolygons-class}} object. Note that if \code{x} is a 
#'   \code{\link[raster]{Raster-class}} object then it must have only one
#'   layer.
#' 
#' @param edges \code{integer} number of edges that each planning unit has when
#'   argument to \code{x} is a \code{\link[raster]{Raster-class}} object. 
#'  Defaults to 4. Valid values are 4 and 6.
#' 
#' @details This function returns a matrix. Cells along the off-diagonal 
#'   indicating the length of the shared boundary between two planning units. 
#'   Cells along the diagonal indicate length of a given planning unit's edges 
#'   that have no neighbors (eg. for edges of planning units found along the 
#'   coastline).
#'
#' @return \code{\link{Matrix}{dsCMatrix-class}} object.
#'
#' @name boundary_matrix
#'
#' @exportMethod boundary_matrix
#'
#' @export
methods::setGeneric('boundary_matrix', 
                    signature=methods::signature('x'),
                    function(x, ...) standardGeneric('boundary_matrix'))


#' @name boundary_matrix
#' @rdname boundary_matrix
methods::setMethod(
  'boundary_matrix',
  signature(x='Raster'),
  function(x, ...) {
  # assert that arguments are valid
    assertthat::assert_that(inherits(x, 'Raster'))
    assertthat::assert_that(
      isTRUE(raster::nlayers(x)==1))
  # indices of cells with finite values
  include <- raster::Which(is.finite(x), cells=TRUE)
  # find the neighboring indices of these cells
  ud <- matrix(c(NA, NA, NA, 1, 0, 1, NA, NA, NA), 3, 3)
  lf <- matrix(c(NA, 1, NA, NA, 0, NA, NA, 1, NA), 3, 3)
  b <- rbind(
    data.frame(raster::adjacent(x, include, pairs=TRUE,
                                directions = ud),
                boundary = raster::res(x)[1]),
    data.frame(raster::adjacent(x, include, pairs=TRUE,
                                directions = lf),
                boundary = raster::res(x)[2]))
  names(b) <- c('id1', 'id2', 'boundary')
  b$id1 <- as.integer(b$id1)
  b$id2 <- as.integer(b$id2)
  # subset neighbors to only include cells with finite values
  b <- b[which((b$id1 %in% include) & (b$id2 %in% include)),]
  # coerce to sparse matrix object
  m <- Matrix::forceSymmetric(Matrix::sparseMatrix(i=b[[1]], j=b[[2]], x=b[[3]],
                                                   dims=rep(raster::ncell(x), 2)))
  m <- m[include,include]
  # if cells don't have four neighbors then set the diagonal to be the total
  # perimeter of the cell minus the boundaries of its neighbors
  diag(m) <- (sum(raster::res(x))*2) - Matrix::colSums(m)
  # return matrix
  as(m, 'dsCMatrix')
})

#' @name boundary_matrix
#' @rdname boundary_matrix
methods::setMethod(
  'boundary_matrix',
  signature(x='SpatialPolygons'),
  function(x, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, 'SpatialPolygons'))
    # calculate boundary data
    x <- rcpp_boundary_data(rcpp_sp_to_polyset(x@polygons, 'Polygons'))$data
    # show warnings generated if any
    if (length(x$warnings) > 0)
      sapply(x$warnings, warning)
    # return result
    Matrix::sparseMatrix(i=x[[1]], j=x[[2]], x=x[[3]], giveCsparse=TRUE,
                         symmetric=TRUE)
})

#' @name boundary_matrix
#' @rdname boundary_matrix
methods::setMethod(
  'boundary_matrix',
  signature(x='SpatialLines'),
  function(x, ...) {
    assertthat::assert_that(inherits(x, 'SpatialLines'))
    stop('Planning units represented by lines have no boundaries. ',
      'See ?constraints for alternative constraints.')
})

#' @name boundary_matrix
#' @rdname boundary_matrix
methods::setMethod(
  'boundary_matrix',
  signature(x='SpatialPoints'),
  function(x, ...) {
    assertthat::assert_that(inherits(x, 'SpatialPoints'))
    stop('Planning units represented by points have no boundaries. ',
      'See ?constraints alternative constraints.')
})
