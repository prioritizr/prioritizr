#' @include internal.R
NULL

#' Adjacency matrix
#'
#' Create a matrix showing which planning units are spatially adjacent to
#' each other. Note that this also include planning units that overlap
#' with each other too.
#'
#' @param x [`Raster-class`],
#'   [`SpatialPolygons-class`],
#'   [`SpatialLines-class`],
#'   or [sf::sf()] object
#'   representing planning units.
#'
#' @param directions `integer` If `x` is a
#'   [`Raster-class`] object, the number of directions
#'   in which cells should be considered adjacent: 4 (rook's case), 8 (queen's
#'   case), 16 (knight and one-cell queen moves), or "bishop" to for cells
#'   with one-cell diagonal moves.
#'
#' @param ... not used.
#'
#' @details Spatial processing is completed using
#'   [sf::st_intersects()] for [`Spatial-class`] and
#'   [sf::sf()] objects,
#'   and [raster::adjacent()] for [`Raster-class`]
#'   objects.
#'   Prior to version 5.0.0,  this function was named
#'   `connected_matrix`. It has been renamed to be consistent
#'   with other spatial association matrix functions.
#'
#' @return [`dsCMatrix-class`] sparse symmetric matrix.
#'   Each row and column represents a planning unit.
#'   Cells values indicate if different planning units are
#'   adjacent to each other or not (using ones and zeros).
#'   To reduce computational burden, cells among the matrix diagonal are
#'   set to zero. Furthermore, if the argument to `x` is a
#'   [`Raster-class`] object, then cells with `NA`
#'   values are set to zero too.
#'
#' @name adjacency_matrix
#'
#' @rdname adjacency_matrix
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_sf, sim_pu_lines)
#'
#' # create adjacency matrix using raster data
#' ## crop raster to 9 cells
#' r <- crop(sim_pu_raster, c(0, 0.3, 0, 0.3))
#'
#' ## make adjacency matrix
#' am_raster <- adjacency_matrix(r)
#'
#' # create adjacency matrix using polygons (sf) data
#' ## subset 9 polygons
#' ply <- sim_pu_sf[c(1:2, 10:12, 20:22), ]
#'
#' ## make adjacency matrix
#' am_ply <- adjacency_matrix(ply)
#'
#' # create adjacency matrix using lines (Spatial) data
#' ## subset 9 lines
#' lns <- sim_pu_lines[c(1:2, 10:12, 20:22), ]
#'
#' ## make adjacency matrix
#' am_lns <- adjacency_matrix(lns)
#'
#' # plot data and the adjacency matrices
#' \donttest{
#' par(mfrow = c(4,2))
#'
#' ## plot raster and adjacency matrix
#' plot(r, main = "raster", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(am_raster)), main = "adjacency matrix", axes = FALSE,
#'      box = FALSE)
#'
#' ## plot polygons (sf) and adjacency matrix
#' plot(r, main = "polygons (sf)", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(am_ply)), main = "adjacency matrix", axes = FALSE,
#'     box = FALSE)
#'
#' ## plot lines (Spatial) and adjacency matrix
#' plot(r, main = "lines (Spatial)", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(am_lns)), main = "adjacency matrix", axes = FALSE,
#'      box = FALSE)
#' }
#' @export
adjacency_matrix <- function(x, ...) UseMethod("adjacency_matrix")

#' @rdname adjacency_matrix
#' @method adjacency_matrix Raster
#' @export
adjacency_matrix.Raster <- function(x, directions = 4L, ...) {
  assertthat::assert_that(inherits(x, "Raster"),
                          no_extra_arguments(...),
                          assertthat::is.count(directions),
                          raster::nlayers(x) >= 1)
  if (raster::nlayers(x) == 1) {
    # indices of cells with finite values
    include <- raster::Which(is.finite(x), cells = TRUE)
  } else {
    # indices of cells with finite values
    include <- raster::Which(sum(is.finite(x)) > 0, cells = TRUE)
    suppressWarnings(x <- raster::setValues(x[[1]], NA_real_))
    # set x to a single raster layer with only values in pixels that are not
    # NA in all layers
    x[include] <- 1
  }
  # find the neighboring indices of these cells
  m <- raster::adjacent(x, include, pairs = TRUE, directions = directions)
  m <- m[(m[, 1] %in% include) & (m[, 2] %in% include), ]
  # coerce to sparse matrix object
  m <- Matrix::sparseMatrix(i = m[, 1], j = m[, 2], x = rep(1, nrow(m)),
                            dims = rep(raster::ncell(x), 2))
  # return result
  Matrix::drop0(Matrix::forceSymmetric(m))
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix SpatialPolygons
#' @export
adjacency_matrix.SpatialPolygons <- function(x, ...) {
  adjacency_matrix(sf::st_as_sf(x), ...)
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix SpatialLines
#' @export
adjacency_matrix.SpatialLines <- function(x,  ...) {
  adjacency_matrix(sf::st_as_sf(x), ...)
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix SpatialPoints
#' @export
adjacency_matrix.SpatialPoints <- function(x, ...) {
  adjacency_matrix(sf::st_as_sf(x), ...)
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix sf
#' @export
adjacency_matrix.sf <- function(x, ...) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, "sf"), no_extra_arguments(...))
  # verify that geometry types are supported
  geomc <- geometry_classes(x)
  if (any(grepl("POINT", geomc, fixed = TRUE)))
    stop("data represented by points are no longer supported - ",
      "see ?proximity_matrix instead")
  if (any(grepl("GEOMETRYCOLLECTION", geomc, fixed = TRUE)))
    stop("geometry collection data are not supported")
  # return sparse intersection matrix
  int <- sf::st_intersects(x, sparse = TRUE)
  names(int) <- as.character(seq_len(nrow(x)))
  int <- rcpp_list_to_matrix_indices(int)
  int <- Matrix::sparseMatrix(i = int$i, j = int$j, x = 1,
                              dims = rep(nrow(x), 2))
  Matrix::diag(int) <- 0
  Matrix::drop0(Matrix::forceSymmetric(int))
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix default
#' @export
adjacency_matrix.default <- function(x, ...) {
  stop("data are not stored in a spatial format")
}
