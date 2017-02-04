#' @include internal.R generics.R ConservationProblem-class.R OptimizationProblem-class.R
NULL

#' @rdname compile
#' @export
compile.ConservationProblem <- function(x, ...) {
  assertthat::assert_that(inherits(x), 'ConservationProblem')
  # check that fields with no defaults have been set
  if (!inherits(self$objective, 'Objective'))
    stop('problem has no specified objective. See ?objectives')
  if (!inherits(self$targets, 'Target'))
    stop('problem has no specified targets. See ?targets')
  # set defaults for fields if needed
  if (!inherits(self$decision, 'Decision'))
    self$decision <- default_decision()
  # compile problem into basic OptimizationProblem object
  stop('TODO: implement compiler function')
}
 
