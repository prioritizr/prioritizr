#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add Maximum Feature Representation Objective
#'
#' Set an objective to find to find the solution that fulfills as many targets
#' as possible while ensuring that the cost of the solution does not exceed
#' budget and that all constraints are met. This objective was inspired by the
#' conservation problem defined in Cabeza and Moilanen (2001).
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param budget \code{numeric} value specifying the maximum expenditure of
#' the prioritization.
#'
#' @details
#' A problem objective is used to specify the overall goal of the
#' conservation planning problem. Please note that \strong{all conservation
#' planning problems formulated in the prioritizr package require the addition
#' of both objectives and targets}. Failing to do so will return a default
#' error message  when solving.
#'
#' The maximum target coverage problem is a hybrid between the minimum set and
#' maximum  cover problems in that it allows for both a budget and targets to
#' be set. This problem finds the set of planning units that meets
#' representation targets for the most species while staying within a fixed
#' budget. If multiple solutions can meet all targets while staying within
#' budget, the cheapest solution is chosen.
#'
#' The maximum target coverage problem can be stated mathematically, for
#' \eqn{n}{n} planning units and \eqn{m}{m} conservation features, as:
#'
#' \deqn{\mathit{Maximize} \space -a\sum_{i=1}^{n} x_i c_i + \sum_{j=1}^{m}y_j
#' \space \mathit{subject \space to} \space
#' \space \sum_{i=1}^{n}x_i c_i \leq B \space \& \space
#' \sum_{j=1}^{n} x_j r_{ij}\geq y_iT_i \space \forall \space i}{Maximize
#' -a\sum^n (xi*ci) + \sum^m (yi) subject to \sum^n (xi*ci) \le B \& \sum^n
#' (xj*rij) \ge yi*Ti for all i}
#'
#' where \eqn{x_i}{xi} is a binary decision variable specifying whether
#' planning unit \eqn{i} has been selected (1) or not (0), \eqn{y_i}{yi} is a
#' binary decision variable specifying whether the target for species \eqn{i}
#' should be met, \eqn{c_i}{ci} is the cost of planning unit \eqn{i},
#' \eqn{r_ij}{rij} is the representation level of feature \eqn{i}{i} in #'
#' planning unit \eqn{j}{j}, \eqn{B}{B} is the budget, and \eqn{T_i}{Ti}
#' is the target for feature \eqn{i}{i}. Finally, the factor \eqn{a}{a} is
#' chosen so that the first term of the objective function is much smaller than #' the second. This ensures that the reserve cost only plays a role in
#' distinguishing between solutions that meet the same number of targets.
#'
#' @seealso \code{\link{objectives}}, \code{\link{constraints}},
#'   \code{\link{problem}}, \code{\link{targets}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_max_features_objective(5000) %>%
#'      add_relative_targets(0.1) %>%
#'      add_binary_decisions()
#'
#' \donttest{
#' # solve problem
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution")
#' }
#'
#' @name add_max_features_objective
NULL

#' @rdname add_max_features_objective
#' @export
add_max_features_objective <- function(x, budget) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(budget))),
                          assertthat::is.scalar(budget),
                          isTRUE(budget > 0.0))
  # make parameter
  p <- numeric_parameter("budget", budget, lower_limit = 0,
                         upper_limit = sum(x$planning_unit_costs()))
  # add objective to problem
  x$add_objective(pproto(
    "MaximumRepresentationObjective",
    Objective,
    name = "Maximum representation objective",
    parameters = parameters(p),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_max_features_objective(x$ptr,
                                                  y$feature_targets(),
                                                  y$planning_unit_costs(),
                                                  self$parameters$get("budget"))
                                                )
    }))
}
