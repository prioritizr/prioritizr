#' @include internal.R Constraint-class.R
NULL

#' Connected constraint
#'
#' This function adds a constraint to ensure that all selected
#' planning units in a prioritisation are connected.
#' 
#' @details TODO
#'
#' @return \code{\link{Constraint}} object.
#' 
#' @seealso \code{\link{constraints}} for a list of all available constraints.
#'
#' @export
connected_constraint <- function() {
  # create new constraint object
  Constraint$new(name='connected',
                 data=list(),
                 parameters=parameters(binary_parameter('Connected', 1L)),
                 validate = function(x) {
                  assertthat::assert_that(inherits('ConservationProblem'))
                  invisible(TRUE)
                 },
                 apply = function(x) {
                  assertthat::assert_that(inherits('OptimizationProblem'))
                  stop('TODO: implement apply methed for connected_constraint')
                 }
  )
}
