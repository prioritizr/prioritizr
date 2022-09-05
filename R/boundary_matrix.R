#' @include internal.R
NULL

#' Boundary matrix
#'
#' Generate a matrix describing the amount of shared boundary length
#' between different planning units, and the amount of exposed edge length each
#' planning unit exhibits.
#'
#' @param x [`Raster-class`],
#'   [`SpatialLines-class`],
#'   [`SpatialPolygons-class`],
#'   [sf::sf()] object representing planning units. If `x` is a
#'   [`Raster-class`] object then it must have only one
#'   layer.
#'
#' @param ... not used.
#'
#' @details This function returns a [`dsCMatrix-class`]
#'   symmetric sparse matrix. Cells on the off-diagonal indicate the length of
#'   the shared boundary between two different planning units. Cells on the
#'   diagonal indicate length of a given planning unit's edges that have no
#'   neighbors (e.g., for edges of planning units found along the
#'   coastline). **This function assumes the data are in a coordinate
#'   system where Euclidean distances accurately describe the proximity
#'   between two points on the earth**. Thus spatial data in a
#'   longitude/latitude coordinate system (i.e.,
#'   [WGS84](https://spatialreference.org/ref/epsg/wgs-84/))
#'   should be reprojected to another coordinate system before using this
#'   function. Note that for [`Raster-class`] objects
#'   boundaries are missing for cells that have `NA` values in all cells.
#'
#' @section Notes:
#' In earlier versions, this function had an extra `str_tree` parameter
#' that could be used to leverage STR query trees to speed up processing
#' for planning units in vector format.
#' Although this functionality improved performance, it was not
#' enabled by default because the underlying function
#' (i.e., `rgeos:gUnarySTRtreeQuery()`) was documented as experimental.
#' The `boundary_matrix()` function has since been updated so that it will
#' use STR query trees to speed up processing for planning units in vector
#' format (using [geos::geos_strtree()]).
#'
#' @return [`dsCMatrix-class`] symmetric sparse matrix object.
#'   Each row and column represents a planning unit.
#'   Cells values indicate the shared boundary length between different pairs
#'   of planning units.
#'
#' @name boundary_matrix
#'
#' @rdname boundary_matrix
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_polygons)
#'
#' # subset data to reduce processing time
#' r <- crop(sim_pu_raster, c(0, 0.3, 0, 0.3))
#' ply <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
#' ply2 <- st_as_sf(ply)
#'
#' # create boundary matrix using raster data
#' bm_raster <- boundary_matrix(r)
#'
#' # create boundary matrix using polygon (Spatial) data
#' bm_ply1 <- boundary_matrix(ply)
#'
#' # create boundary matrix using polygon (sf) data
#' bm_ply2 <- boundary_matrix(ply2)
#'
#' # plot raster and boundary matrix
#' \dontrun{
#' par(mfrow = c(1, 2))
#' plot(r, main = "raster", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(bm_raster)), main = "boundary matrix",
#'      axes = FALSE, box = FALSE)
#' }
#' # plot polygons and boundary matrices
#' \dontrun{
#' par(mfrow = c(1, 3))
#' plot(r, main = "polygons (Spatial)", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(bm_ply1)), main = "boundary matrix", axes = FALSE,
#'      box = FALSE)
#' plot(r, main = "polygons (sf)", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(bm_ply2)), main = "boundary matrix", axes = FALSE,
#'      box = FALSE)
#' }
#' @export
boundary_matrix <- function(x, ...) UseMethod("boundary_matrix")

#' @rdname boundary_matrix
#' @method boundary_matrix Raster
#' @export
boundary_matrix.Raster <- function(x, ...) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "Raster"))
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
  ud <- matrix(c(NA, NA, NA, 1, 0, 1, NA, NA, NA), 3, 3)
  lf <- matrix(c(NA, 1, NA, NA, 0, NA, NA, 1, NA), 3, 3)
  b <- rbind(data.frame(raster::adjacent(x, include, pairs = TRUE,
                                         directions = ud),
                        boundary = raster::res(x)[1]),
             data.frame(raster::adjacent(x, include, pairs = TRUE,
                                         directions = lf),
                        boundary = raster::res(x)[2]))
  names(b) <- c("id1", "id2", "boundary")
  b$id1 <- as.integer(b$id1)
  b$id2 <- as.integer(b$id2)
  b <- b[(b$id1 %in% include) & (b$id2 %in% include), , drop = FALSE]
  # coerce to sparse matrix object
  m <- Matrix::forceSymmetric(
    Matrix::sparseMatrix(i = b[[1]], j = b[[2]], x = b[[3]],
                         dims = rep(raster::ncell(x), 2)))
  # if cells don't have four neighbors then set the diagonal to be the total
  # perimeter of the cell minus the boundaries of its neighbors
  Matrix::diag(m)[include] <-
    (sum(raster::res(x)) * 2) - Matrix::colSums(m)[include]
  # return matrix
  as_Matrix(m, "dsCMatrix")
}

#' @rdname boundary_matrix
#' @method boundary_matrix SpatialPolygons
#' @export
boundary_matrix.SpatialPolygons <- function(x, ...) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "SpatialPolygons"))
  # convert to sf format for processing
  boundary_matrix.sf(sf::st_as_sf(x))
}

#' @rdname boundary_matrix
#' @method boundary_matrix SpatialLines
#' @export
boundary_matrix.SpatialLines <- function(x, ...) {
  assertthat::assert_that(inherits(x, "SpatialLines"))
  stop("data represented by lines have no boundaries - ",
    "see ?constraints for alternative constraints")
}

#' @rdname boundary_matrix
#' @method boundary_matrix SpatialPoints
#' @export
boundary_matrix.SpatialPoints <- function(x, ...) {
  assertthat::assert_that(inherits(x, "SpatialPoints"))
  stop("data represented by points have no boundaries - ",
    "see ?constraints alternative constraints")
}

#' @rdname boundary_matrix
#' @method boundary_matrix sf
#' @export
boundary_matrix.sf <- function(x, ...) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, "sf"))
  geomc <- geometry_classes(x)
  if (any(grepl("POINT", geomc, fixed = TRUE)))
    stop("data represented by points have no boundaries - ",
      "see ?constraints alternative constraints")
  if (any(grepl("LINE", geomc, fixed = TRUE)))
    stop("data represented by lines have no boundaries - ",
      "see ?constraints alternative constraints")
  if (any(grepl("GEOMETRYCOLLECTION", geomc, fixed = TRUE)))
    stop("geometry collection data are not supported")
  # generate STR query tree
  g <- geos::as_geos_geometry(x)
  tree <- geos::geos_basic_strtree(g)
  strm <- geos::geos_basic_strtree_query(tree, g)
  geos::geos_basic_strtree_finalized(tree)
  rm(g, tree)
  # convert to matrix
  strm <- Matrix::sparseMatrix(
    i = strm$tree, j = strm$x, x = 1, dims = rep(nrow(x), 2)
  )
  strm <- Matrix::forceSymmetric(strm, uplo = "U")
  # calculate boundary data
  y <- rcpp_boundary_data(
    rcpp_sp_to_polyset(sf::as_Spatial(x)@polygons, "Polygons"),
    strm = strm,
    str_tree = TRUE
  )
  # if any warnings generated, then throw them
  if (length(y$warnings) > 0) {
    vapply(y$warnings, warning, character(1)) # nocov
  }
  # return result
  Matrix::sparseMatrix(
    i = y$data[[1]], j = y$data[[2]], x = y$data[[3]],
    symmetric = TRUE, dims = rep(nrow(x), 2)
  )
}

#' @rdname boundary_matrix
#' @method boundary_matrix default
#' @export
boundary_matrix.default <- function(x, ...) {
  stop("data must be in a spatial format to generate a boundary matrix")
}
