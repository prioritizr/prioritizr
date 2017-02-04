#' @include internal.R Constraint-class.R
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
#' @return \code{\link{Constraint}} object.
#'
#' @seealso \code{\link{constraints}} for a list of all available constraints.
#'
#' @export
corridors_constraint <- function(x, targets) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, 'RasterStack'),
    inherits(targets, 'Target'),
    nrow(target$get('targets')) == raster::nlayers(x))
  # create new constraint object
  Constraint$new(name='corridors',
                 parameters=parameters(),
                 data=list(friction=x, targets=targets),
                 validate = function(x) {
                  assertthat::assert_that(inherits(x, 'ConservationProblem'),
                                          raster::compareRaster(self$data$friction,
                                                                x$cost,
                                                                res=TRUE, tol=1e-5,
                                                                stopiffalse=FALSE),
                                           raster::nlayers(self$data$friction) == 
                                            raster::nlayers(x$features),
                                           self$data$targets$validate(x))
                  invisible(TRUE)
                 },
                 apply = function(x) {
                  assertthat::assert_that(inherits(x, 'OptimizationProblem'))
                  stop('TODO: implement apply method for corridors_constraint')
                 })
}
