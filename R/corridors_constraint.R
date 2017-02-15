#' @include internal.R Constraint-proto.R
NULL

#' Corridors constraint
#'
#' This function adds corridor constraints using friction rasters that
#' denote the dificulty at which individuals can move across the 
#' landscape. Higher cell values indicate that planning units are 
#' a greater barrier to dispersal. Targets are used to denote the 
#' minimum dispersal capability across the solution.
#' 
#' @param x \code{\link[raster]{RasterStack-class}} object denoting friction
#'  surfaces for each feature.
#' 
#' @param targets \code{\link{Target}} object specifying the minimum flow
#'  permitted throughout the prioritisation.
#'
#' @details TODO
#'
#' @return \code{\link{constraint}} object.
#'
#' @seealso \code{\link{constraints}} for a list of all available constraints.
#'
#' @export
corridors_constraint <- function(x, targets) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, 'RasterStack'), 
    inherits(targets, 'Target'),
    nrow(target$get('targets')) == raster::nlayers(x))
  # create new constraint object
  pproto(
    'Constraint',
    Constraint,
    name='corridors',
    parameters=parameters(target$parameters$parameters[[1]]),
    data=list(friction=x, targets=targets),
    prevalidate=function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'),
      raster::compareRaster(self$data$friction, x$cost, res=TRUE, 
        tolerance=1e-5, stopiffalse=FALSE),
        raster::nlayers(self$data$friction) == x$number_of_features())
      invisible(TRUE)
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(y, 'ConservationProblem'))
      stop('TODO: implement apply method for corridors_constraint')
    })
}
