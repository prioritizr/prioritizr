#' @include internal.R Constraint-proto.R
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
#' problem(sim_pu_raster, sim_features) +
#'   minimum_set_objective() +
#'   relative_targets(0.2) +
#'   locked_out(sim_locked_out_raster)
#'
#' @seealso \code{\link{constraints}} for a list of all available constraints.
#'
#' @export
locked_out_constraint <- function(x) {
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
    name='locked out',
    data=list(locked_out=x),
    parameters=parameters(binary_parameter('Locked out', 1L)),
    prevalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      if (inherits(x@cost, 'RasterLayer')) {
        assertthat::assert_that(raster::compareRaster(
          data$locked_out, x$cost, res=TRUE, tolerance=1e-5, stopiffalse=FALSE))
      } else if (inherits(x@cost, 'Spatial')) {
        assertthat::assert_that(rgeos::gIntersects(raster::extent(x$cost),
          raster::extent(data$locked_out)),
          raster::compareCRS(x$cost@proj4string, data$locked_out$proj4string))
      } else {
        stop('ConservationProblem has invalid cost data')
      }
      invisible(TRUE)
    },
    synchronize = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      pu_status <- rep(0L, length(x$number_of_features()))
      if (inherits(self$data$locked_out, 'Raster')) {
        stop('TODO: implement locked out constraint for Raster data')
      } else if (inherits(self$data$locked_out, 'Spatial')) {
        stop('TODO: implement locked out constraint for Spatial data')
      } else if (inherits(self$data$locked_out, 'integer')) {
        pu_status[self$data$locked_out] <- 1L
      } else if (inherits(self$data$locked_out, 'logical')) {
        pu_status[self$data$locked_out] <- 1L
      } else {
        stop('Locked out data not recognized')
      }
      self$parameters$add(binary_parameter_array('status', values=pu_status, 
        labels=as.character(seq_along(pu_status))))
      invisible(TRUE)
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(y, 'OptimizationProblem'))
      stop('TODO: implement apply method for locked_out_constraint')
    })
}

