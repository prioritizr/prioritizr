#' @include internal.R generics.R ConservationProblem-proto.R
#'   OptimizationProblem-proto.R
NULL

#' @rdname compile
#' @export
compile.ConservationProblem <- function(x, ...) {
  # initialization
  assertthat::assert_that(inherits(x), 'ConservationProblem')
  # check that fields with no defaults have been set
  if (!inherits(x$objective, 'Objective'))
    stop('problem has no specified objective. ',
      'See ?objectives for available objectives.')
  if (!inherits(x$targets, 'Target'))
    stop('problem has no specified targets. ',
      'See ?targets for available targets.')
  # set defaults for fields if needed
  if (!inherits(x$decision, 'Decision'))
    x$decision <- default_decision()
  op <- OptimizationProblem$new()
  # add rij data to optimization problem
  rcpp_add_rij_data(op$ptr, x$data$rij_matrix@i, x$data$rij_matrix@j,
                    x$data$rij_matrix@x, x$data$rij_matrix@dim)
  # add decision types to optimization problem
  x$decision$apply(op)
  # add objective to optimization problem
  x$objective$apply(op, targets=x$targets$output(), costs=x$get_costs())
  # add constraints to optimization problem
  for (i in seq_along(x$constraints$constraints))
    x$constraints$constraints[[i]]$apply(op)
  # return problem object
  op
}

