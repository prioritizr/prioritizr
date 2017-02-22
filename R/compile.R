#' @include internal.R ConservationProblem-proto.R OptimizationProblem-proto.R
NULL

#' Compile a problem
#' 
#' Compile a \code{\link{ConservationProblem}} into an
#' \code{OptimizationProblem}.
#' 
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param ... not used.
#' 
#' @return \code{OptimizationProblem} object.
#'
#' @examples
#' # build minimal conservation problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_minimum_set_objective() %>%
#'   add_relative_targets(0.1)
#'
#' # compile the conservation problem into an optimization problem
#' o <- compile(p)
#'
#' # print the optimization problem
#' print(o)
#'
#' @export
compile <- function(x, ...) UseMethod('compile')

#' @rdname compile
#' @export
compile.ConservationProblem <- function(x, ...) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'))
  # sanity checks
  if (inherits(x$objective, 'MaximumCoverageObjective') & 
      !is.Waiver(x$targets))
    warning('ignoring targets since the maximum coverage objective function ',
      'doesn\'t use targets')
  # replace waivers with defaults
  if (is.Waiver(x$objective))
    x <- add_default_objective(x)
  if (is.Waiver(x$targets) & !inherits(x$objective, 'MaximumCoverageObjective'))
    x <- add_default_targets(x)
  if (is.Waiver(x$decision))
    x <- add_default_decision(x)
  if (is.Waiver(x$solver))
    x <- add_default_solver(x)
  op <- new_optimization_problem()
  # add rij data to optimization problem
  rcpp_add_rij_data(op$ptr, x$get_data('rij_matrix'))
  # add decision types to optimization problem
  x$decision$calculate(x)
  x$decision$apply(op)
  # add objective to optimization problem
  x$objective$calculate(x)
  x$objective$apply(op, x)
  # add constraints to optimization problem
  for (i in seq_along(x$constraints$constraints)) {
    x$constraints$constraints[[i]]$calculate(x)
    x$constraints$constraints[[i]]$apply(op, x)
  }
  # check that planning units have not been locked in and locked out
  pu_ub <- op$ub()[seq_len(x$number_of_planning_units())]
  invalid_pu <- which(op$lb()[seq_len(x$number_of_planning_units())] > pu_ub)
  if (length(invalid_pu)) {
    stop('the following planning units have been locked in and locked out:\n', 
      invalid_pu)
  }
  # check that all planning units have not been locked out
  if (all(pu_ub == 0))
    stop('all planning units are locked out.')
  # return problem object
  op
}
