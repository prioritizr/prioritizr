#' @include internal.R
NULL

#' Adjacency matrix
#'
#' Create a matrix showing which planning units are spatially adjacent to
#' each other.
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
#' @details
#' Spatial processing is completed using
#' [sf::st_intersects()] for [sf::sf()] objects,
#' and [terra::adjacent()] for [terra::rast()] objects.
#' Note that spatially overlapping planning units are considered
#' adjacent.
#'
#' @section Notes:
#'   In earlier versions (< 5.0.0), this function was named as the
#'   `connected_matrix` function. It has been renamed to be consistent
#'   with other spatial association matrix functions.
#'
#' @return A [`Matrix::dsCMatrix-class`] sparse symmetric matrix.
#'   Each row and column represents a planning unit.
#'   Cells values indicate if different planning units are
#'   adjacent to each other or not (using ones and zeros).
#'   To reduce computational burden, cells among the matrix diagonal are
#'   set to zero. Furthermore, if the argument to `x` is a
#'   [terra::rast()] object, then cells with `NA` values are set to
#'   zero too.
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
#' ply <- sim_pu_polygons[c(1:3, 11:13, 20:22), ]
#'
#' ## make adjacency matrix
#' am_ply <- adjacency_matrix(ply)
#'
#' # plot data and the adjacency matrices
#'
#' ## plot raster and adjacency matrix
#' plot(r, main = "raster", axes = FALSE)
#' Matrix::image(am_raster, main = "adjacency matrix")
#'
#' ## plot polygons and adjacency matrix
#' plot(ply[, 1], main = "polygons")
#' Matrix::image(am_ply, main = "adjacency matrix")
#'
#' }
#' @export
adjacency_matrix <- function(x, ...) {
  assert_required(x)
  UseMethod("adjacency_matrix")
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix Raster
#' @export
adjacency_matrix.Raster <- function(x, directions = 4, ...) {
  assert_required(directions)
  assert_dots_empty()
  assert(inherits(x, "Raster"))
  cli_warning(raster_pkg_deprecation_notice)
  adjacency_matrix.SpatRaster(terra::rast(x), directions = directions, ...)
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix SpatRaster
#' @export
adjacency_matrix.SpatRaster <- function(x, directions = 4, ...) {
  assert_required(directions)
  assert_dots_empty()
  assert(
    inherits(x, "SpatRaster"),
    is_numeric_values(x),
    terra::nlyr(x) >= 1,
    assertthat::is.count(directions)
  )
  if (terra::nlyr(x) == 1) {
    # indices of cells with finite values
    include <- terra::cells(is.finite(x), 1)[[1]]
  } else {
    # indices of cells with finite values
    include <- terra::cells(min(is.finite(x)), 1)[[1]]
    # set x to a single raster layer with only values in cells that are not
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
  assert_dots_empty()
  cli_warning(sp_pkg_deprecation_notice)
  adjacency_matrix(sf::st_as_sf(x), ...)
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix SpatialLines
#' @export
adjacency_matrix.SpatialLines <- function(x,  ...) {
  assert_dots_empty()
  cli_warning(sp_pkg_deprecation_notice)
  adjacency_matrix(sf::st_as_sf(x), ...)
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix SpatialPoints
#' @export
adjacency_matrix.SpatialPoints <- function(x, ...) {
  assert_required(x)
  assert_dots_empty()
  cli_warning(sp_pkg_deprecation_notice)
  adjacency_matrix(sf::st_as_sf(x), ...)
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix sf
#' @export
adjacency_matrix.sf <- function(x, ...) {
  # assert valid arguments
  assert_required(x)
  assert_dots_empty()
  assert(inherits(x, "sf"), is_valid_geometries(x))
  # verify that geometry types are supported
  geomc <- st_geometry_classes(x)
  assert(
    !any(grepl("POINT", geomc, fixed = TRUE)),
    msg = paste(
      "This function no longer supports point data,",
      "use {.fn proximity_matrix} for point data."
    )
  )
  # return sparse intersection matrix
  int <- sf::st_intersects(x, sparse = TRUE)
  int <- Matrix::sparseMatrix(
    i = unlist(int, recursive = TRUE, use.names = FALSE),
    j = rep(seq_along(int), lengths(int)),
    x = 1,
    dims = rep(nrow(x), 2)
  )
  Matrix::diag(int) <- 0
  Matrix::drop0(Matrix::forceSymmetric(int))
}

#' @rdname adjacency_matrix
#' @method adjacency_matrix default
#' @export
adjacency_matrix.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "{.arg x} must be a {.cls sf} or {.cls SpatRaster}.",
      "x" = "{.arg x} is a {.cls {class(x)}}."
    )
  )
}
