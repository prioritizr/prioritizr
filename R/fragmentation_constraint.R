#' @include internal.R Constraint-class.R
NULL

#' Fragmentation constraints
#'
#' This function adds constraints to reduce fragmentation
#' in a prioritisation by penalising solutions with too
#' many exposed edges.
#' 
#' @param x \code{numeric} penalty for fragmentation. This
#'  is equivalent to the boundary length modifier (BLM)
#'  parameter used in Marxan.
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
  assertthat::assert_that(
    is.numeric(x), length(x) == 1, x > 1e-5)
  # create new constraint object
  Constraint$new(name='fragmentation',
                 data=list(x),
                 parameters=parameters(truncated_numeric_parameter('Fragmentation penalty', x)),
                 validate = function(x) {
                   assertthat::assert_that(inherits(x, 'ConservationProblem'))
                   invisible(TRUE)
                 },
                 apply = function(x) {
                  assertthat::assert_that(inherits(x, 'OptimizationProblem'))
                  stop('TODO: implement apply method for fragmentation_constraint')
                 })
}
