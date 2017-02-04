#' @include internal.R Constraint-class.R
NULL

#' Locked out constraints
#'
#' It may be desirable to lock out planning units that are already heavily 
#' developed and therefore have little viable habitat. This function adds
#' constraints to lock out planning units to ensure that they are not
#' prioritised.
#' 
#' @param x \code{\link[raster]{RasterLayer-class}},  
#'   \code{\link[sp]{SpatialPolygons}}, or \code{\link[sp]{SpatialPolygons}} 
#'   object.
#'
#' @details If a \code{Spatial} object is supplied, any planning units that
#'   overlap with argument \code{x} cannot be prioritised. If a
#'   \code{\link[raster]{RasterLayer-class}} is supplied, then the raster must
#'   contain \code{logical} (\code{TRUE} or \code{FALSE}) values. Planning 
#'   units that overlap with \code{TRUE} cells are locked out of the solution.
#'
#' @return \code{\link{Constraint}} object.
#' 
#' @examples
#' problem(sim_pu, sim_features) +
#'   minimum_set_objective() +
#'   relative_targets(0.2) +
#'   locked_out(sim_pu[sim_pu$class == 'urban'])
#'
#' @seealso \code{\link{constraints}} for a list of all available constraints.
#'
#' @export
locked_out_constraint <- function(x) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, 'RasterLayer') || inherits(x, 'Spatial'))
  if (inherits(x, 'RasterLayer'))
    assertthat::assert_that(x@file@datanotation == 'LOG1S')
  # create new constraint object
  Constraint$new(name='locked_out',
                 data=list(locked_out=x),
                 parameters=parameters(binary_parameter('Locked out', 1L)),
                 validate = function(x) {
                  assertthat::assert_that(inherits(x, 'ConservationProblem'))
                  if (inherits(x@cost, 'RasterLayer')) {
                    assertthat::assert_that(raster::compareRaster(data$locked_out,
                                                                x$cost,
                                                                res=TRUE, tol=1e-5,
                                                                stopiffalse=FALSE))
                  } else if (inherits(x@cost, 'Spatial')) {
                    assertthat::assert_that(
                      rgeos::gIntersects(raster::extent(x$cost),
                                         raster::extent(data$locked_out)),
                      raster::compareCRS(x$cost@proj4string, data$locked_out$proj4string))
                  } else {
                    stop('ConservationProblem has cost data of unrecognized class')
                  }
                  invisible(TRUE)
                 },
                 apply = function(x) {
                  assertthat::assert_that(inherits(x, 'OptimizationProblem'))
                  stop('TODO: implement apply method for locked_out_constraint')
                 })
}

