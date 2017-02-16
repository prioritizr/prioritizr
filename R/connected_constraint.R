#' @include internal.R Constraint-proto.R
NULL

#' Connected constraint
#'
#' This function adds a constraint to ensure that all selected
#' planning units in a prioritisation are connected.
#' 
#' @details TODO
#'
#' @return \code{\link{constraint}} object.
#' 
#' @seealso \code{\link{constraints}} for a list of all available constraints.
#'
#' @export
connected_constraint <- function() {
  # create new constraint object
  pproto(
    'ConnectedConstraint',
    Constraint, 
    parameters=parameters(binary_parameter('Connected', 1L)),
    prevalidate=function(self, x) {
      assertthat::assert_that(inherits(y, 'ConservationProblem'))
      # check that at least two planning units are touching each other
      stop('TODO: implement prevalidate for connected_constraint')
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits('OptimizationProblem'),
        inherits(y, 'ConservationProblem'))
      stop('TODO: implement apply methed for connected_constraint')
    })
}
