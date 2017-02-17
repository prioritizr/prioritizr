#' @include internal.R generics.R ConservationProblem-proto.R
#'   OptimizationProblem-proto.R
NULL

#' @rdname compile
#' @export
compile.ConservationProblem <- function(x, ...) {
  # initialization
  assertthat::assert_that(inherits(x, 'ConservationProblem'))
  # replace waivers with defaults
  if (is.Waiver(x$objective))
    x <- add_default_objective(x)
  if (is.Waiver(x$targets))
    x <- add_default_targets(x)
  if (is.Waiver(x$decision))
    x <- add_default_decision(x)
  if (is.Waiver(x$solver))
    x <- add_default_solver(x)
  op <- new_optimization_problem()
  # add rij data to optimization problem
  rcpp_add_rij_data(op$ptr, as_triplet_dataframe(x$data$rij_matrix),
    dim(x$data$rij_matrix))
  # add decision types to optimization problem
  x$decision$apply(op)
  # add objective to optimization problem
  x$objective$apply(op, x)
  # add constraints to optimization problem
  for (i in seq_along(x$constraints$constraints))
    x$constraints$constraints[[i]]$apply(op)
  # return problem object
  op
}

