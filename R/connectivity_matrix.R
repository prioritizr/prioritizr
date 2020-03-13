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
#' @param x \code{\link[raster]{Raster-class}},
#'   \code{\link[sp]{SpatialPolygonsDataFrame-class}},
#'   \code{\link[sp]{SpatialLinesDataFrame-class}},
#'   or \code{\link[sf]{sf}} object
#'   representing planning units.
#'   If \code{x} is a \code{\link[raster]{Raster-class}} object then it must
#'   contain a single layer.
#'
#' @param y \code{\link[raster]{Raster-class}} object showing the conductance
#'   of different areas across the study area, or a \code{character} object
#'   denoting a column name in the attribute table of \code{x} that contains
#'   the conductance values. Note that argument to \code{y} can only be a
#'   \code{character} object if the argument to \code{x} is a
#'   \code{\link[sp]{Spatial-class}} or \code{\link[sf]{sf}} object.
#'   Also, note that if the argument to \code{x} is a
#'   \code{\link{Raster-class}} object then
#'   argument to \code{y} must have the same spatial properties as it
#'   (i.e. coordinate system, extent, resolution).
#'
#' @param ... additional arguments passed to \code{\link{fast_extract}} for
#'   extracting and calculating the conductance values for each planning unit.
#'   These arguments are only used if argument to \code{x} is a
#'   \code{link[sp]{Spatial-class}} or \code{\link[sf]{sf}} object and argument
#'   to \code{y} is a \code{\link{Raster-class}} object.
#'
#' @details Shared boundary calculations are performed using
#'   \code{\link{boundary_matrix}}.
#'
#' @return \code{\link[Matrix]{dsCMatrix-class}} sparse symmetric matrix.
#'   Each row and column represents a planning unit.
#'   Cells values indicate the connectivity between different pairs of planning
#'   units.
#'   To reduce computational burden, cells among the matrix diagonal are
#'   set to zero. Furthermore, if the argument to \code{x} is a
#'   \code{\link[raster]{Raster-class}} object, then cells with \code{NA}
#'   values are set to zero too.
#'
#' @name connectivity_matrix
#'
#' @rdname connectivity_matrix
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_sf, sim_features)
#'
#' # create connectivity matrix using raster planning unit data using
#' # the raster cost values to represent conductance
#' ## extract 9 planning units
#' r <- crop(sim_pu_raster, c(0, 0.3, 0, 0.3))
#'
#' ## extract conductance data for the 9 planning units
#' cd <- crop(r, sim_features[[1]])
#'
#' ## make connectivity matrix
#' cm_raster <- connectivity_matrix(r, cd)
#'
#' ## plot data and matrix
#' \donttest{
#' par(mfrow = c(1,3))
#' plot(r, main = "planning units (raster)", axes = FALSE, box = FALSE)
#' plot(cd, main = "conductivity", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_raster)), main = "connectivity", axes = FALSE,
#'      box = FALSE)
#' }
#' # create connectivity matrix using polygon planning unit data using
#' # the habitat suitability data for sim_features[[1]] to represent
#' # planning unit conductances
#' ## subset data to 9 polygons
#' ply <- sim_pu_sf[c(1:2, 10:12, 20:22), ]
#'
#' ## make connectivity matrix
#' cm_ply <- connectivity_matrix(ply, sim_features[[1]])
#'
#' ## plot data and matrix
#' \donttest{
#' par(mfrow = c(1, 3))
#' plot(ply, main = "planning units (sf)")
#' plot(sim_features[[1]], main = "conductivity", axes = FALSE, box = FALSE)
#' plot(raster(as.matrix(cm_ply)), main = "connectivity", axes = FALSE,
#'      box = FALSE)
#' }
#' @aliases connectivity_matrix,Spatial,character-method connectivity_matrix,Spatial,Raster-method connectivity_matrix,Raster,Raster-method connectivity_matrix,sf,character-method connectivity_matrix,sf,Raster-method
#'
#' @export
methods::setGeneric(
  "connectivity_matrix",
  signature = methods::signature("x", "y"),
  function(x, y, ...) standardGeneric("connectivity_matrix"))

#' @name connectivity_matrix
#' @usage \S4method{connectivity_matrix}{Spatial,Raster}(x, y, ...)
#' @rdname connectivity_matrix
methods::setMethod(
  "connectivity_matrix",
  signature(x = "Spatial", y = "Raster"),
  function(x, y, ...) {
    assertthat::assert_that(inherits(x, "Spatial"))
    connectivity_matrix(sf::st_as_sf(x), y, ...)
  })

#' @name connectivity_matrix
#' @usage \S4method{connectivity_matrix}{Spatial,character}(x, y, ...)
#' @rdname connectivity_matrix
methods::setMethod(
  "connectivity_matrix",
  signature(x = "Spatial", y = "character"),
  function(x, y, ...) {
    assertthat::assert_that(inherits(x, "Spatial"))
    connectivity_matrix(sf::st_as_sf(x), y, ...)
  })

#' @name connectivity_matrix
#' @usage \S4method{connectivity_matrix}{sf,character}(x, y, ...)
#' @rdname connectivity_matrix
methods::setMethod(
  "connectivity_matrix",
  signature(x = "sf", y = "character"),
  function(x, y, ...) {
    # validate that arguments are valid
    assertthat::assert_that(inherits(x, "sf"),
      assertthat::is.string(y),
      assertthat::has_name(x, y))
    assertthat::assert_that(is.numeric(x[[y]]), assertthat::noNA(x[[y]]))
    # generate connectivity data for each pair of connected units
    bd <- matrix_to_triplet_dataframe(boundary_matrix(x))
    bd <- bd[bd[[1]] != bd[[2]], ]
    bd$x <- bd$x * ( (x[[y]][bd$i] + x[[y]][bd$j]) * 0.5)
    bd <- bd[which(bd$x > 0), ]
    # generate connectivity matrix
    Matrix::sparseMatrix(i = bd$i, j = bd$j, x = bd$x, symmetric = TRUE,
                         dims = rep(nrow(x), 2))
  })

#' @name connectivity_matrix
#' @usage \S4method{connectivity_matrix}{sf,Raster}(x, y, ...)
#' @rdname connectivity_matrix
methods::setMethod(
  "connectivity_matrix",
  signature(x = "sf", y = "Raster"),
  function(x, y, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "sf"), inherits(y, "Raster"),
      raster::nlayers(y) == 1)
    assertthat::assert_that(intersecting_extents(x, y),
      raster::compareCRS(as_CRS(sf::st_crs(x)), y@crs))
    # extract conductance values
    cv <- fast_extract(y, x, ...)
    # generate connectivity data for each pair of connected units
    bd <- matrix_to_triplet_dataframe(boundary_matrix(x))
    bd <- bd[bd[[1]] != bd[[2]], ]
    bd$x <- bd$x * ( (cv[bd$i] + cv[bd$j]) * 0.5)
    bd <- bd[which(bd$x > 0), ]
    # connectivity matrix
    Matrix::sparseMatrix(i = bd$i, j = bd$j, x = bd$x, symmetric = TRUE,
                         dims = rep(nrow(x), 2))
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
