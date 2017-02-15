#' @include internal.R Constraint-proto.R
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
  assertthat::assert_that(inherits(x, c('integer', 'logical', 'Spatial', 
    'RasterLayer')))
  if (inherits(x, 'Raster'))
    assertthat::assert_that(x@file@datanotation == 'LOG1S', 
      raster::nlayers(x)==1)
  # create new constraint object
  pproto(
    'Constraint',
    Constraint,
    name='locked in',
    data=list(locked_in=x),
    parameters=parameters(binary_parameter('Locked in', 1L)),
    prevalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      if (inherits(x@cost, 'RasterLayer')) {
        assertthat::assert_that(raster::compareRaster(
          data$locked_in, x$cost, res=TRUE, tolerance=1e-5, stopiffalse=FALSE))
      } else if (inherits(x@cost, 'Spatial')) {
        assertthat::assert_that(rgeos::gIntersects(raster::extent(x$cost),
          raster::extent(data$locked_in)),
          raster::compareCRS(x$cost@proj4string, data$locked_in$proj4string))
      } else {
        stop('ConservationProblem has invalid cost data')
      }
      invisible(TRUE)
    },
    synchronize = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      pu_status <- rep(0L, length(x$number_of_features()))
      if (inherits(self$data$locked_in, 'Raster')) {
        stop('TODO: implement locked in constraint for Raster data')
      } else if (inherits(self$data$locked_in, 'Spatial')) {
        stop('TODO: implement locked in constraint for Spatial data')
      } else if (inherits(self$data$locked_in, 'integer')) {
        pu_status[self$data$locked_in] <- 1L
      } else if (inherits(self$data$locked_in, 'logical')) {
        pu_status[self$data$locked_in] <- 1L
      } else {
        stop('Locked in data not recognized')
      }
      self$parameters$add(binary_parameter_array('status', values=pu_status, 
        labels=as.character(seq_along(pu_status))))
      invisible(TRUE)
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(y, 'OptimizationProblem'))
      stop('TODO: implement apply method for locked_in_constraint')
    })
}

