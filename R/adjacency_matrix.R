#' @include internal.R
NULL

#' Adjacency matrix
#'
#' Create a matrix showing which planning units are spatially adjacent to
#' each other. Note that this also include planning units that overlap
#' with each other too.
#'
#' @param x [terra::rast()] or [sf::sf()] object representing planning units.
#'
#' @param directions `integer` If `x` is a
#'   [terra::rast()] object, the number of directions
#'   in which cells should be considered adjacent: 4 (rook's case), 8 (queen's
#'   case), 16 (knight and one-cell queen moves), or "bishop" to for cells
#'   with one-cell diagonal moves.
#'
#' @param ... not used.
#'
#' @details Spatial processing is completed using
#'   [sf::st_intersects()] for [sf::sf()] objects,
#'   and [terra::adjacent()] for [terra::rast()] objects.
#'
#' @section Notes:
#'   In earlier versions (< 5.0.0), this function was named as the
#'   `connected_matrix` function. It has been renamed to be consistent
#'   with other spatial association matrix functions.
#'
#' @return A [`dsCMatrix-class`] sparse symmetric matrix.
#'   Each row and column represents a planning unit.
#'   Cells values indicate if different planning units are
#'   adjacent to each other or not (using ones and zeros).
#'   To reduce computational burden, cells among the matrix diagonal are
#'   set to zero. Furthermore, if the argument to `x` is a
#'   [terra::rast()] object, then cells with `NA`
#'   values are set to zero too.
#'
#' @name adjacency_matrix
#'
#' @rdname adjacency_matrix
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_pu_lines <- get_sim_pu_lines()
#'
#' # create adjacency matrix using raster data
#' ## crop raster to 9 cells
#' r <- terra::crop(sim_pu_raster, terra::ext(c(0, 0.3, 0, 0.3)))
#'
#' ## make adjacency matrix
#' am_raster <- adjacency_matrix(r)
#'
#' # create adjacency matrix using polygon data
#' ## subset 9 polygons
#' ply <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
#'
#' ## make adjacency matrix
#' am_ply <- adjacency_matrix(ply)
#'
#' # create adjacency matrix using lines data
#' ## subset 9 lines
#' lns <- sim_pu_lines[c(1:2, 10:12, 20:22), ]
#'
#' ## make adjacency matrix
#' am_lns <- adjacency_matrix(lns)
#'
#' # plot data and the adjacency matrices
#'
#' ## plot raster and adjacency matrix
#' plot(r, main = "raster", axes = FALSE)
#' plot(am_raster, main = "adjacency matrix")
#'
#' ## plot polygons and adjacency matrix
#' plot(ply[, 1], main = "polygons")
#' plot(am_ply, main = "adjacency matrix")
#'
#' ## plot lines and adjacency matrix
#' plot(lns[, 1], main = "lines")
#' plot(am_lns, main = "adjacency matrix")
#' }
#' @export
adjacency_matrix <- function(x, ...) UseMethod("adjacency_matrix")

#' @rdname adjacency_matrix
#' @method adjacency_matrix Raster
#' @export
adjacency_matrix.Raster <- function(x, directions = 4, ...) {
  assertthat::assert_that(inherits(x, "Raster"))
  .Deprecated(msg = raster_pkg_deprecation_notice)
  adjacency_matrix.SpatRaster(x, directions = directions, ...)
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix SpatRaster
#' @export
adjacency_matrix.SpatRaster <- function(x, directions = 4, ...) {
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    terra::nlyr(x) >= 1,
    no_extra_arguments(...),
    assertthat::is.count(directions)
  )
  if (terra::nlyr(x) == 1) {
    # indices of cells with finite values
    include <- terra::cells(is.finite(x), 1)[[1]]
  } else {
    # indices of cells with finite values
    include <- terra::cells(min(is.finite(x)), 1)[[1]]
    # set x to a single raster layer with only values in pixels that are not
    # NA in all layers
    suppressWarnings(x <- terra::setValues(x[[1]], NA_real_))
    x[include] <- 1
  }
  # find the neighboring indices of these cells
  m <- terra::adjacent(x, include, pairs = TRUE, directions = directions)
  m <- m[(m[, 1] %in% include) & (m[, 2] %in% include), , drop = FALSE]
  # coerce to sparse matrix object
  m <- Matrix::sparseMatrix(
    i = m[, 1], j = m[, 2], x = rep(1, nrow(m)),
    dims = rep(terra::ncell(x), 2)
  )
  # return result
  Matrix::drop0(Matrix::forceSymmetric(m))
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix SpatialPolygons
#' @export
adjacency_matrix.SpatialPolygons <- function(x, ...) {
  .Deprecated(msg = sp_pkg_deprecation_notice)
  adjacency_matrix(sf::st_as_sf(x), ...)
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix SpatialLines
#' @export
adjacency_matrix.SpatialLines <- function(x,  ...) {
  .Deprecated(msg = sp_pkg_deprecation_notice)
  adjacency_matrix(sf::st_as_sf(x), ...)
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix SpatialPoints
#' @export
adjacency_matrix.SpatialPoints <- function(x, ...) {
  .Deprecated(msg = sp_pkg_deprecation_notice)
  adjacency_matrix(sf::st_as_sf(x), ...)
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix sf
#' @export
adjacency_matrix.sf <- function(x, ...) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "sf"),
    no_extra_arguments(...)
  )
  # verify that geometry types are supported
  geomc <- geometry_classes(x)
  assertthat::assert_that(
    !any(grepl("POINT", geomc, fixed = TRUE)),
    msg = paste(
      "adjacency_matrix() no longer supports point data,",
      "use proximity_matrix() for point data"
    )
  )
  assertthat::assert_that(
    !any(grepl("GEOMETRYCOLLECTION", geomc, fixed = TRUE)),
    msg = "argument to x contains sf::st_geometrycollection() geometries"
  )
  # return sparse intersection matrix
  int <- sf::st_intersects(x, sparse = TRUE)
  names(int) <- as.character(seq_len(nrow(x)))
  int <- rcpp_list_to_matrix_indices(int)
  int <- Matrix::sparseMatrix(
    i = int$i, j = int$j, x = 1, dims = rep(nrow(x), 2)
  )
  Matrix::diag(int) <- 0
  Matrix::drop0(Matrix::forceSymmetric(int))
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix default
#' @export
adjacency_matrix.default <- function(x, ...) {
  stop("data are not stored in a spatial format")
}
