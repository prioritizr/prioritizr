#' @include internal.R ConservationProblem-proto.R OptimizationProblem-proto.R
NULL

#' Compile a problem
#'
#' Compile a \code{\link{ConservationProblem-class}} into an
#' \code{\link{OptimizationProblem-class}} object.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param compressed_formulation \code{logical} should the conservation problem
#'   compiled into a compressed version of a planning problem?
#'   If \code{TRUE} then the problem is expressed using the compressed
#'   formulation. If \code{FALSE} then the problem is expressed using the
#'   expanded formulation. If \code{NA}, then the compressed is used unless one
#'   of the constraints requires the expanded formulation. This argument
#'   defaults to \code{NA}.
#'
#' @param ... not used.
#'
#' @details \strong{In nearly all cases, the default argument to
#'   \code{formulation} should be used}. The only situation where manually
#'   setting the argument to \code{formulation} is desirable is during testing.
#'   Manually setting the argument to \code{formulation} will at best
#'   have no effect on the problem. At worst, it may result in
#'   an error, a misspecified problem, or unnecessarily long
#'   solve times.
#'
#' @return \code{\link{OptimizationProblem-class}} object.
#'
#' @examples
#' # build minimal conservation problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1)
#'
#' # compile the conservation problem into an optimization problem
#' o <- compile(p)
#'
#' # print the optimization problem
#' print(o)
#'
#' # print the optimization problem
#' print(o)
#'
#' @export
compile <- function(x, ...) UseMethod("compile")

#' @rdname compile
#' @export
compile.ConservationProblem <- function(x, compressed_formulation = NA, ...) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    is.na(compressed_formulation) ||
          assertthat::is.flag(compressed_formulation))
  # sanity checks
  if (inherits(x$objective, "MaximumCoverageObjective") &
      !is.Waiver(x$targets))
    warning("ignoring targets since the maximum coverage objective function ",
            "doesn\"t use targets")
  # replace waivers with defaults
  if (is.Waiver(x$objective))
    x <- add_default_objective(x)
  if (is.Waiver(x$targets) & !inherits(x$objective, "MaximumCoverageObjective"))
    x <- add_default_targets(x)
  if (is.Waiver(x$decisions))
    x <- add_default_decisions(x)
  if (is.Waiver(x$solver))
    x <- add_default_solver(x)
  op <- new_optimization_problem()
  # determine if expanded formulation is required
  if (is.na(compressed_formulation))
    compressed_formulation <- all(vapply(x$constraints$ids(),
      function(i) x$constraints[[i]]$compressed_formulation, logical(1)))
  # add rij data to optimization problem
  rcpp_add_rij_data(op$ptr, x$get_data("rij_matrix"), compressed_formulation)
  # add decision types to optimization problem
  x$decisions$calculate(x)
  x$decisions$apply(op)
  # add objective to optimization problem
  x$objective$calculate(x)
  x$objective$apply(op, x)
  # add penalties to optimization problem
  for (i in x$penalties$ids()) {
    x$penalties[[i]]$calculate(x)
    x$penalties[[i]]$apply(op, x)
  }
  # add constraints to optimization problem
  for (i in x$constraints$ids()) {
    x$constraints[[i]]$calculate(x)
    x$constraints[[i]]$apply(op, x)
  }
  # check that planning units have not been locked in and locked out
  pu_ub <- op$ub()[seq_len(x$number_of_planning_units())]
  invalid_pu <- which(op$lb()[seq_len(x$number_of_planning_units())] > pu_ub)
  if (length(invalid_pu)) {
    stop("the following planning units have been locked in and locked out:\n",
      invalid_pu)
  }
  # check that all planning units have not been locked out
  if (all(pu_ub == 0))
    stop("all planning units are locked out.")
  # return problem object
  op
}
