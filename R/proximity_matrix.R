#' @include internal.R
NULL

#' Proximity matrix
#'
#' Create a matrix showing which planning units are within a certain
#' spatial proximity to each other.
#'
#' @param x \code{\link[raster]{Raster-class}},
#'   \code{\link[sp]{Spatial-class}}, or \code{\link[sf]{sf}} object
#'   representing planning units.
#'
#' @param distance \code{numeric} distance threshold. Planning units
#'   that are further apart from each other than this threshold are
#'   not treated as being within proximity of each other.
#'
#' @details Proximity calculations are performed using
#'   \code{\link[sf]{st_is_within_distance}}.
#'
#' @return \code{\link[Matrix]{dsCMatrix-class}} sparse symmetric matrix.
#'   Each row and column represents a planning unit.
#'   Cells values indicate
#'   if the pair-wise distances between different planning units are within
#'   the distance threshold or not (using ones and zeros).
#'   To reduce computational burden, cells among the matrix diagonal are
#'   set to zero. Furthermore, if the argument to \code{x} is a
#'   \code{\link[raster]{Raster-class}} object, then cells with \code{NA}
#'   values are set to zero too.
#'
#' @name proximity_matrix
#'
#' @rdname proximity_matrix
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_sf, sim_pu_lines, sim_pu_points)
#'
#' # create proximity matrix using raster data
#' ## crop raster to 9 cells to provide a small example
#' r <- crop(sim_pu_raster, c(0, 0.3, 0, 0.3))
#'
#' ## make proximity matrix using a distance threshold of 2
#' cm_raster <- proximity_matrix(r, distance = 2)
#'
#' # create proximity matrix using polygon (sf) data
#' ## subset 9 polygons to provide a small example
#' ply <- sim_pu_sf[c(1:2, 10:12, 20:22), ]
#'
#' ## make proximity matrix using a distance threshold of 2
#' cm_ply <- proximity_matrix(ply, distance = 2)
#'
#' # create proximity matrix using line (Spatial) data
#' ## subset 9 lines to provide a small example
#' lns <- sim_pu_lines[c(1:2, 10:12, 20:22), ]
#'
#' ## make proximity matrix
#' cm_lns <- proximity_matrix(lns, distance = 2)
#'
#' ## create proximity matrix using point (Spatial) data
#' ## subset 9 points to provide a small example
#' pts <- sim_pu_points[c(1:2, 10:12, 20:22), ]
#'
#' # make proximity matrix
#' cm_pts <- proximity_matrix(pts, distance = 2)
#'
#' # plot data and the proximity matrices
#' \donttest{
#' par(mfrow = c(4,2))
#'
#' ## plot raster and proximity matrix
#' plot(r, main = "raster", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_raster)), main = "proximity matrix", axes = FALSE,
#'      box = FALSE)
#'
#' ## plot polygons and proximity matrix
#' plot(r, main = "polygons (sf)", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_ply)), main = "proximity matrix", axes = FALSE,
#'     box = FALSE)
#'
#' ## plot lines and proximity matrix
#' plot(r, main = "lines (Spatial)", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_lns)), main = "proximity matrix", axes = FALSE,
#'      box = FALSE)
#'
#' ## plot points and proximity matrix
#' plot(r, main = "points (Spatial)", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_pts)), main = "proximity matrix", axes = FALSE,
#'      box = FALSE)
#' }
#' @export
proximity_matrix <- function(x, distance) UseMethod("proximity_matrix")

#' @rdname proximity_matrix
#' @method proximity_matrix Raster
#' @export
proximity_matrix.Raster <- function(x, distance) {
  assertthat::assert_that(inherits(x, "Raster"), raster::nlayers(x) >= 1,
    assertthat::is.number(distance), assertthat::noNA(distance))
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
  # convert cell centroids to points
  pts <- sf::st_as_sf(as.data.frame(raster::xyFromCell(x, include)),
                      coords = c(1, 2))
  # find cells within the distance
  prx <- sf::st_is_within_distance(pts, dist = distance, sparse = TRUE)
  # convert list format to sparse matrix
  names(prx) <- as.character(seq_len(nrow(pts)))
  prx <- rcpp_list_to_matrix_indices(prx)
  prx$i <- include[prx$i]
  prx$j <- include[prx$j]
  prx <- Matrix::sparseMatrix(i = prx$i, j = prx$j, x = 1,
                              dims = rep(raster::ncell(x), 2))
  Matrix::diag(prx) <- 0
  Matrix::drop0(Matrix::forceSymmetric(prx))
}

#' @rdname proximity_matrix
#' @method proximity_matrix SpatialPolygons
#' @export
proximity_matrix.SpatialPolygons <- function(x, distance) {
  proximity_matrix(sf::st_as_sf(x), distance)
}

#' @rdname proximity_matrix
#' @method proximity_matrix SpatialLines
#' @export
proximity_matrix.SpatialLines <- function(x, distance) {
  proximity_matrix(sf::st_as_sf(x), distance)
}

#' @rdname proximity_matrix
#' @method proximity_matrix SpatialPoints
#' @export
proximity_matrix.SpatialPoints <- function(x, distance) {
  proximity_matrix(sf::st_as_sf(x), distance)
}

#' @rdname proximity_matrix
#' @method proximity_matrix sf
#' @export
proximity_matrix.sf <- function(x, distance) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, "sf"),
    assertthat::is.number(distance), assertthat::noNA(distance))
  # verify that geometry types are supported
  geomc <- geometry_classes(x)
  if (any(grepl("GEOMETRYCOLLECTION", geomc, fixed = TRUE)))
    stop("geometry collection data are not supported")
  # return sparse intersection matrix
  prx <- sf::st_is_within_distance(x, dist = distance, sparse = TRUE)
  names(prx) <- as.character(seq_len(nrow(x)))
  prx <- rcpp_list_to_matrix_indices(prx)
  prx <- Matrix::sparseMatrix(i = prx$i, j = prx$j, x = 1,
                              dims = rep(nrow(x), 2))
  Matrix::diag(prx) <- 0
  Matrix::drop0(Matrix::forceSymmetric(prx))
}

#' @rdname proximity_matrix
#' @method proximity_matrix default
#' @export
proximity_matrix.default <- function(x, distance) {
  stop("data are not stored in a spatial format")
}