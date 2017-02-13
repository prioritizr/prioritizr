#' @include internal.R Constraint-class.R
NULL

#' Locked in constraints
#'
#' This function adds constraints to lock in planning units
#' to ensure that they are prioritised. For example, it may
#' be desirable to lock in planning units already within 
#' protected areas.
#' 
#' @param x \code{\link[raster]{RasterLayer-class}},  
#'   \code{\link[sp]{SpatialPolygons}}, or \code{\link[sp]{SpatialPolygons}} 
#'   object.
#'
#' @details If a \code{Spatial} object is supplied, any planning units that
#'   overlap with argument \code{x} are locked in. If a
#'   \code{\link[raster]{RasterLayer-class}} is supplied, then the raster must
#'   contain \code{logical} (\code{TRUE} or \code{FALSE}) values. Planning 
#'   units that overlap with \code{TRUE} cells are locked into the solution.
#'
#' @return \code{\link{Constraint}} object.
#'
#' @examples
#' problem(sim_pu_raster, sim_features) +
#'   minimum_set_objective() +
#'   relative_targets(0.2) +
#'   locked_in(sim_locked_in_raster)
#' 
#' @seealso \code{\link{constraints}} for a list of all available constraints.
#'
#' @export
locked_in_constraint <- function(x) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, 'RasterLayer'))
  assertthat::assert_that(x@file@datanotation == 'LOG1S')
  # create new constraint object
  Constraint$new(name='locked_in',
                 data=list(locked_in=x),
                 parameters=parameters(binary_parameter('Locked in', 1L)),
                 validate = function(x) {
                  assertthat::assert_that(inherits(x, 'ConservationProblem'))
                  if (inherits(x@cost, 'RasterLayer')) {
                    assertthat::assert_that(raster::compareRaster(data$locked_in,
                                                                x$cost,
                                                                res=TRUE, tol=1e-5,
                                                                stopiffalse=FALSE))
                  } else if (inherits(x@cost, 'Spatial')) {
                    assertthat::assert_that(
                      rgeos::gIntersects(raster::extent(x$cost),
                                         raster::extent(data$locked_in)),
                      raster::compareCRS(x$cost@proj4string, data$locked_in$proj4string))
                  } else {
                    stop('ConservationProblem has cost data of unrecognized class')
                  }
                  invisible(TRUE)
                 },
                 apply = function(x) {
                  assertthat::assert_that(inherits(x, 'OptimizationProblem'))
                  stop('TODO: implement apply method for locked_in_constraint')
                 })
}

