#' @include internal.R
NULL

#' Connected matrix
#'
#' Create a matrix showing which planning units are spatially connected.
#' Created automatically when \code{\link{add_connected_constraints}} is added
#' to a \code{ConservationProblem} object.
#'
#' @param x \code{\link[raster]{Raster-class}} or
#'   \code{\link[sp]{Spatial-class}} object. Note that if \code{x} is a
#'   \code{\link[raster]{Raster-class}} object then it must have only one
#'   layer.
#'
#' @param directions \code{integer} If \code{x} is a
#'   \code{\link[raster]{Raster-class}} object, the number of directions
#'    in which cells should be connected: 4 (rook's case), 8 (queen's case),
#'    16 (knight and one-cell queen moves), or "bishop" to connect cells with
#'    one-cell diagonal moves.
#'
#' @param distance \code{numeric} If \code{x} is a
#'   \code{\link{SpatialPoints-class}} object, the distance that planning units
#'   have to be within in order to qualify as being connected.
#'
#' @param ... not used.
#'
#' @details This function returns a \code{\link[Matrix]{dgCMatrix-class}} sparse
#'   matrix. Cells along the off-diagonal indicate if two planning units are
#'   connected. Cells along the diagonal are zero to reduce memory consumption.
#'
#' @return \code{\link[Matrix]{dsCMatrix-class}} object.
#'
#' @name connected_matrix
#'
#' @rdname connected_matrix
#'
#' @examples
#' ## load data
#' data(sim_pu_raster, sim_pu_polygons, sim_pu_lines, sim_pu_points)
#'
#' ## create connected matrix using raster data
#' # crop raster to 9 cells
#' r <- crop(sim_pu_raster, c(0, 0.3, 0, 0.3))
#' # make connected matrix
#' cm_raster <- connected_matrix(r)
#'
#' ## create connected matrix using polygon data
#' # subset 9 polygons
#' ply <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
#' # make connected matrix
#' cm_ply <- connected_matrix(ply)
#'
#' ## create connected matrix using polygon line
#' # subset 9 lines
#' lns <- sim_pu_lines[c(1:2, 10:12, 20:22), ]
#' # make connected matrix
#' cm_lns <- connected_matrix(lns)
#'
#' ## create connected matrix using point data
#' # subset 9 points
#' pts <- sim_pu_points[c(1:2, 10:12, 20:22), ]
#' # make connected matrix
#' cm_pts <- connected_matrix(pts, distance = 0.1)
#'
#' ## plot data and the connected matrices
#' par(mfrow = c(4,2))
#'
#' # plot raster and connected matrix
#' plot(r, main = "raster", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_raster)), main = "connected matrix", axes = FALSE,
#'      box = FALSE)
#'
#' # plot polygons and connected matrix
#' plot(r, main = "polygons", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_ply)), main = "connected matrix", axes = FALSE,
#'     box = FALSE)
#'
#' # plot lines and connected matrix
#' plot(r, main = "lines", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_lns)), main = "connected matrix", axes = FALSE,
#'      box = FALSE)
#'
#' # plot points and connected matrix
#' plot(r, main = "points", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_pts)), main = "connected matrix", axes = FALSE,
#'      box = FALSE)
#'
#' @export
connected_matrix <- function(x, ...) UseMethod("connected_matrix")

#' @rdname connected_matrix
#' @method connected_matrix Raster
#' @export
connected_matrix.Raster <- function(x, directions=4L, ...) {
  assertthat::assert_that(inherits(x, "Raster"),
                          assertthat::is.count(directions),
                          isTRUE(raster::nlayers(x) == 1))
  # indices of cells with finite values
  include <- raster::Which(is.finite(x), cells = TRUE)
  # find the neighboring indices of these cells
  m <- raster::adjacent(x, include, pairs = TRUE, directions = directions)
  m <- m[(m[, 1] %in% include) & (m[, 2] %in% include), ]
  # coerce to sparse matrix object
  m <- Matrix::forceSymmetric(Matrix::sparseMatrix(i = m[, 1], j = m[, 2],
                                                   x = rep(1, nrow(m)),
                                                   dims = rep(raster::ncell(x),
                                                              2)))
  # return included planning units
  return(m[include, include])
}

#' @rdname connected_matrix
#' @method connected_matrix SpatialPolygons
#' @export
connected_matrix.SpatialPolygons <- function(x, ...) {
  assertthat::assert_that(inherits(x, "SpatialPolygons"))
  sp::spChFIDs(x) <- as.character(seq_len(length(x)))
  m <- rcpp_list_to_matrix_indices(rgeos::gIntersects(x, byid = TRUE,
                                                      returnDense = FALSE))
  m <- Matrix::forceSymmetric(Matrix::sparseMatrix(i = m$i, j = m$j, x = m$x,
                                                   dims = rep(length(x), 2)))
  # return matrix
  return(m)
}

#' @rdname connected_matrix
#' @method connected_matrix SpatialLines
#' @export
connected_matrix.SpatialLines <- function(x,  ...) {
  assertthat::assert_that(inherits(x, "SpatialLines"))
  sp::spChFIDs(x) <- as.character(seq_len(length(x)))
  m <- rcpp_list_to_matrix_indices(rgeos::gIntersects(x, byid = TRUE,
                                                      returnDense = FALSE))
  m <- Matrix::forceSymmetric(Matrix::sparseMatrix(i = m$i, j = m$j, x = m$x,
                                                   dims = rep(length(x), 2)))
  # return matrix
  return(m)
}

#' @rdname connected_matrix
#' @method connected_matrix SpatialPoints
#' @export
connected_matrix.SpatialPoints <- function(x, distance, ...) {
    assertthat::assert_that(inherits(x, "SpatialPoints"),
                                     assertthat::is.scalar(distance),
                                     isTRUE(distance >= 0))
  m <- rgeos::gWithinDistance(x, dist = distance, byid = TRUE)
  m[m] <- 1
  diag(m) <- 0
  m <- Matrix::Matrix(m, sparse = TRUE)
  m <- Matrix::forceSymmetric(m)
  # return matrix
  return(m)
}

#' @rdname connected_matrix
#' @method connected_matrix data.frame
#' @export
connected_matrix.data.frame <- function(x, ...) {
  assertthat::assert_that(inherits(x, "data.frame"))
  stop("data stored in a data.frame do not contain spatial information")
}
