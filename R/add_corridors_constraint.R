#' @include internal.R Constraint-proto.R
NULL

#' Corridors constraint
#'
#' This function adds corridor constraints using friction rasters that
#' denote the dificulty at which individuals can move across the 
#' landscape. Higher cell values indicate that areas that are harder
#' for indiviuals to move through. Targets are used to denote the 
#' minimum dispersal capability across the solution.
#'
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param targets \code{numeric} targets specifying the minimum flow
#'  permitted throughout the prioritisation. If \code{targets} is a single
#'  number then it is used for each feature. Otherwise, if \code{targets} is 
#'  a \code{vector} then it specifies the target for each feature.
#'
#' @param friction \code{\link[raster]{RasterStack-class}} object denoting 
#'   friction surfaces for each feature. Pixels with missing values are
#'   assumed to be completely impermeable.
#'
#' @return \code{\link{ConservationProblem}} object.
#'
#' @seealso \code{\link{constraints}} for all the available constraints.
#'
#' @examples
#' # create problem with corridors constraint
#' problem(sim_pu_raster, sim_features) %>%
#'   add_minimum_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_corridors_constraint(0.2, sim_locked_in_raster)
#'
#' @export
add_corridors_constraint <- function(x, targets, friction) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, 'RasterStack'), 
    is.numeric(targets), isTRUE(all(is.finite(targets))), 
    isTRUE(all(targets > 0)), length(targets) == raster::nlayers(x),
    isTRUE(all(raster::cellStats(x, 'min') > 0)),
    raster::compareRaster(x$features[[1]], friction[[1]], crs=TRUE, res=TRUE,
      tolerance=1e-5, stopiffalse=FALSE))
  # create parameters
  p <- parameters(binary_parameter('Apply constraint?', 1),
    proportion_parameter_array('Friction targets', targets, x$feature_names()))
  # create new constraint object
  x$add_constraint(pproto(
    'CorridorsConstraint',
    Constraint,
    name='Corridors Constraint',
    parameters=parameters(p),
    data=list(friction=friction),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(y, 'ConservationProblem'))
      stop('TODO: implement apply method for add_corridors_constraint')
    }))
  # return problem
  return(x)
}
