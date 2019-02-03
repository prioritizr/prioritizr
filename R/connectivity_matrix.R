#' @include internal.R
NULL

#' Connectivity matrix
#'
#' Create a matrix showing the connectivity between planning units. Connectivity
#' is calculated as the average conductance of two planning units multiplied
#' by the amount of shared boundary between the two planning units. Thus
#' planning units that each have higher a conductance and share a greater
#' boundary are associated with greater connectivity.
#'
#' @param x \code{\link[raster]{Raster-class}} or
#'   \code{\link[sp]{Spatial-class}} object representing planning units. If
#'   \code{x} is a \code{\link[raster]{Raster-class}} object then it must
#'   contain a single band.
#'
#' @param y \code{\link[raster]{Raster-class}} object showing the conductance
#'   of different areas across the study area, or a \code{character} object
#'   denoting a column name in the attribute table of \code{x} that contains
#'   the conductance values. Note that argument to \code{y} can only be a
#'   \code{character} object if the argument to \code{x} is a
#'   \code{\link[sp]{Spatial-class}} object. Additionally, note that if
#'   argument to \code{x} is a \code{\link{Raster-class}} object then
#'   argument to \code{y} must have the same spatial properties as it
#'   (i.e. coordinate system, extent, resolution).
#'
#' @param ... arguments passed to \code{\link{fast_extract}} for extracting
#'   and calculating the conductance for each unit. These arguments
#'   are only used if argument to \code{x} is a \code{link[sp]{Spatial-class}}
#'   object and argument to \code{y} is a \code{\link{Raster-class}}
#'   object.
#'
#' @details This function returns a \code{\link[Matrix]{dsCMatrix-class}}
#'   sparse symmetric matrix. Each row and column represents a planning unit.
#'   Cell values represent the connectivity between two planning units. To
#'   reduce computational burden for \code{\link[raster]{Raster-class}} data,
#'   data are missing for cells with \code{NA} values in the argument to
#'   \code{x}. Furthermore, all cells along the diagonal are missing values
#'   since a planing unit does not share connectivity with itself.
#'
#' @return \code{\link[Matrix]{dsCMatrix-class}} sparse symmetric matrix object.
#'
#' @name connectivity_matrix
#'
#' @rdname connectivity_matrix
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_polygons, sim_pu_lines, sim_pu_points,
#'      sim_features)
#'
#' # create connectivity matrix using raster planning unit data using
#' # the raster cost values to represent conductance
#' ## extract 9 planning units
#' r <- crop(sim_pu_raster, c(0, 0.3, 0, 0.3))
#'
#' ## extact conductance data for the 9 planning units
#' cd <- crop(r, sim_features[[1]])
#'
#' ## make connectivity matrix
#' cm_raster <- connectivity_matrix(r, cd)
#'
#' ## plot data and matrix
#' par(mfrow = c(1,3))
#' plot(r, main = "planning units", axes = FALSE, box = FALSE)
#' plot(cd, main = "conductivity", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_raster)), main = "connectivity", axes = FALSE,
#'      box = FALSE)
#'
#' # create connectivity matrix using polygon planning unit data using
#' # the habitat suitability data for sim_features[[1]] to represent
#' # planning unit conductances
#' ## subset data to 9 polygons
#' ply <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
#'
#' ## make connectivity matrix
#' cm_ply <- connectivity_matrix(ply, sim_features[[1]])
#'
#' ## plot data and matrix
#' par(mfrow = c(1,3))
#' plot(ply, main = "planning units")
#' plot(sim_features[[1]], main = "conductivity", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_ply)), main = "connectivity", axes = FALSE,
#'      box = FALSE)
#'
#' @aliases connectivity_matrix,Spatial,character-method connectivity_matrix,Spatial,Raster-method connectivity_matrix,Raster,Raster-method
#'
#' @export
methods::setGeneric(
  "connectivity_matrix",
  signature = methods::signature("x", "y"),
  function(x, y, ...) standardGeneric("connectivity_matrix"))

#' @name connectivity_matrix
#' @usage \S4method{connectivity_matrix}{Spatial,character}(x, y, ...)
#' @rdname connectivity_matrix
methods::setMethod(
  "connectivity_matrix",
  signature(x = "Spatial", y = "character"),
  function(x, y, ...) {
    # validate that arguments are valid
    assertthat::assert_that(inherits(x, "Spatial"),
      assertthat::is.string(y), "data" %in% methods::slotNames(x),
      assertthat::has_name(x@data, y), is.numeric(x@data[[y]]))
    # generate connectivity data for each pair of connected units
    bd <- matrix_to_triplet_dataframe(boundary_matrix(x))
    bd <- bd[bd[[1]] != bd[[2]], ]
    bd$x <- bd$x * ( (x[[y]][bd$i] + x[[y]][bd$j]) * 0.5)
    bd <- bd[which(bd$x > 0), ]
    # generate connectivity matrix
    Matrix::sparseMatrix(i = bd$i, j = bd$j, x = bd$x, symmetric = TRUE,
                         dims = rep(length(x), 2))
  })

#' @name connectivity_matrix
#' @usage \S4method{connectivity_matrix}{Spatial,Raster}(x, y, ...)
#' @rdname connectivity_matrix
methods::setMethod(
  "connectivity_matrix",
  signature(x = "Spatial", y = "Raster"),
  function(x, y, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "Spatial"), inherits(y, "Raster"),
      raster::nlayers(y) == 1, raster::compareCRS(x@proj4string, y@crs))
    # extract conductance values
    cv <- fast_extract(y, x, ...)
    # generate connectivity data for each pair of connected units
    bd <- matrix_to_triplet_dataframe(boundary_matrix(x))
    bd <- bd[bd[[1]] != bd[[2]], ]
    bd$x <- bd$x * ( (cv[bd$i] + cv[bd$j]) * 0.5)
    bd <- bd[which(bd$x > 0), ]
    # connectivity matrix
    Matrix::sparseMatrix(i = bd$i, j = bd$j, x = bd$x, symmetric = TRUE,
                         dims = rep(length(x), 2))
  })

#' @name connectivity_matrix
#' @usage \S4method{connectivity_matrix}{Raster,Raster}(x, y, ...)
#' @rdname connectivity_matrix
methods::setMethod(
  "connectivity_matrix",
  signature(x = "Raster", y = "Raster"),
  function(x, y, ...) {
    # validate that arguments are valid
    assertthat::assert_that(inherits(x, "Raster"), inherits(y, "Raster"),
      raster::nlayers(x) == 1, raster::nlayers(y) == 1,
      is_comparable_raster(x, y))
    # extract data from first bands in x and y
    x <- x[[1]]
    y <- y[[1]]
    # generate connectivity data for each pair of connected units
    bd <- matrix_to_triplet_dataframe(boundary_matrix(x))
    bd <- bd[bd[[1]] != bd[[2]], ]
    bd$x <- bd$x * ( (y[bd$i] + y[bd$j]) * 0.5)
    bd <- bd[which(bd$x > 0), ]
    # connectivity matrix
    Matrix::sparseMatrix(i = bd$i, j = bd$j, x = bd$x, symmetric = TRUE,
                         dims = rep(raster::ncell(x), 2))
  })
