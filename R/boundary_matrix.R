#' @include internal.R
NULL

#' Boundary matrix
#'
#' Generate a boundary matrix describing the shared and exposed edges of
#' planning units.
#'
#' @param x \code{\link[raster]{Raster-class}},
#'   \code{\link[sp]{SpatialLines-class}}, or
#'   \code{\link[sp]{SpatialPolygons-class}} object. If \code{x} is a
#'   \code{\link[raster]{Raster-class}} object then it must have only one
#'   layer.
#'
#' @details This function returns a \code{\link[Matrix]{dsCMatrix-class}}
#'   symmetric sparse matrix. Cells on the off-diagonal indicate the length of
#'   the shared boundary between two different planning units. Cells on the
#'   diagonal indicate length of a given planning unit"s edges that have no
#'   neighbors (e.g. for edges of planning units found along the
#'   coastline). \strong{This function assumes the data are in a coordinate
#'   system where Euclidean distances accurately describe the proximity
#'   between two points on the earth}. Thus spatial data in a longitude/latitude
#'   coordinate system (aka
#'   \href{http://spatialreference.org/ref/epsg/wgs-84/}{WGS84})
#'   should be reprojected to another coordinate system before using this
#'   function. Note that for \code{\link[raster]{Raster-class}} objects
#'   boundaries are missing for cells that have \code{NA} values in all cells.
#'
#' @return \code{\link{Matrix}{dsCMatrix-class}} object.
#'
#' @name boundary_matrix
#'
#' @rdname boundary_matrix
#'
#' @examples
#' ## load data
#' data(sim_pu_raster, sim_pu_polygons)
#'
#' ## create boundary matrix using raster data
#' # crop raster to 9 cells
#' r <- crop(sim_pu_raster, c(0, 0.3, 0, 0.3))
#' # make boundary matrix
#' bm_raster <- boundary_matrix(r)
#'
#' ## create boundary matrix using polygon data
#' # subset 9 polygons
#' ply <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
#' # make boundary matrix
#' bm_ply <- boundary_matrix(ply)
#'
#' # plot raster and connected matrix
#' par(mfrow = c(1, 2))
#' plot(r, main = "raster", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(bm_raster)), main = "boundary matrix",
#'      axes = FALSE, box = FALSE)
#'
#' # plot polygons and connected matrix
#' par(mfrow = c(1, 2))
#' plot(r, main = "polygons", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(bm_ply)), main = "boundary matrix", axes = FALSE,
#'      box = FALSE)
#'
#' @export
boundary_matrix <- function(x) UseMethod("boundary_matrix")

#' @rdname boundary_matrix
#' @method boundary_matrix Raster
#' @export
boundary_matrix.Raster <- function(x) {
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
  b <- b[(b$id1 %in% include) & (b$id2 %in% include), ]
  # coerce to sparse matrix object
  m <- Matrix::forceSymmetric(Matrix::sparseMatrix(i = b[[1]], j = b[[2]],
                                                   x = b[[3]],
                                                   dims = rep(raster::ncell(x),
                                                              2)))
  # if cells don't have four neighbors then set the diagonal to be the total
  # perimeter of the cell minus the boundaries of its neighbors
  Matrix::diag(m)[include] <- (sum(raster::res(x)) * 2) -
                              Matrix::colSums(m)[include]
  # return matrix
  methods::as(m, "dsCMatrix")
}

#' @rdname boundary_matrix
#' @method boundary_matrix SpatialPolygons
#' @export
boundary_matrix.SpatialPolygons <- function(x) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "SpatialPolygons"))
  # calculate boundary data
  y <- rcpp_boundary_data(rcpp_sp_to_polyset(x@polygons, "Polygons"))$data
  # show warnings generated if any
  if (length(y$warnings) > 0)
    vapply(y$warnings, warning, character(1))
  # return result
  Matrix::sparseMatrix(i = y[[1]], j = y[[2]], x = y[[3]], giveCsparse = TRUE,
                       symmetric = TRUE, dims = rep(length(x), 2))
}

#' @rdname boundary_matrix
#' @method boundary_matrix SpatialLines
#' @export
boundary_matrix.SpatialLines <- function(x) {
  assertthat::assert_that(inherits(x, "SpatialLines"))
  stop("data represented by lines have no boundaries - ",
    "see ?constraints for alternative constraints")
}

#' @rdname boundary_matrix
#' @method boundary_matrix SpatialPoints
#' @export
boundary_matrix.SpatialPoints <- function(x) {
  assertthat::assert_that(inherits(x, "SpatialPoints"))
  stop("data represented by points have no boundaries - ",
    "see ?constraints alternative constraints")
}

#' @rdname boundary_matrix
#' @method boundary_matrix data.frame
#' @export
boundary_matrix.data.frame <- function(x) {
  assertthat::assert_that(inherits(x, "data.frame"))
  stop("data stored in a data.frame do not contain spatial information")
}
