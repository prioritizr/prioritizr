#' @include internal.R Constraint-proto.R
NULL

#' Fragmentation penalty
#'
#' This function adds penalties to reduce fragmentation
#' in a prioritisation by punishing solutions with
#' many exposed edges.
#' 
#' @param x \code{numeric} penalty for fragmentation. This
#'  is equivalent to the boundary length modifier (BLM)
#'  parameter in Marxan.
#'
#' @details TODO
#'
#' @return \code{\link{Constraint}} object.
#' 
#' @seealso \code{\link{constraints}} for a list of all available constraints.
#'
#' @export
fragmentation_constraint <- function(x) {
  # assert valid arguments
  assertthat::assert_that(assertthat::is.scalar(x), isTRUE(x > 1e-5),
    is.finite(x))
  # create new constraint object
  pproto(
    'Constraint'
    Constraint,
    name='fragmentation constraint',
    parameters=parameters(truncated_numeric_parameter('Fragmentation penalty',
      x)),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(x, 'ConservationProblem'))
      stop('TODO: implement apply method for fragmentation_constraint')
    })
}
