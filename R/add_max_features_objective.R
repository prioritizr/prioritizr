#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add Maximum Feature Representation Objective
#'
#' Set an objective to find the solution that fulfills as many targets
#' as possible while ensuring that the cost of the solution does not exceed
#' a budget.
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
#' The maximum feature representation problem is a hybrid between the minimum
#' set (see \code{\link{add_min_set_objective}}) and maximum cover
#' (see \code{\link{add_max_cover_objective}}) problems in that it allows for
#' both a budget and targets to be set. This problem finds the set of planning
#' units that meets representation targets for as many features as possible
#' while staying within a fixed budget. If multiple solutions can meet all
#' targets while staying within budget, the cheapest solution is chosen.
#'
#' The maximum feature objective for the reserve design problem can be
#' expressed mathematically for a set of planning units (\eqn{I}{I} indexed by
#' \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Maximize} \space \sum_{i = 1}^{I} -s \space c_i +
#' \sum_{j = 1}^{J} y_j w_j \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i r_{ij} >= y_j t_j \forall j \in J \\
#' \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Maximize sum_i^I (-s * ci) + sum_j^J (yj * wj) subject to
#' sum_i^I (xi * rij) >= (yj tj) for all j in J &
#' sum_i^I (xi * ci) <= B}
#'
#' Here, \eqn{x_i}{xi} is the \code{\link{decisions}} variable (e.g.
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning unit
#' \eqn{i}{i}, \eqn{t_j}{tj} is the representation target for feature
#' \eqn{j}{j}, \eqn{y_j}{yj} indicates if the solution has meet
#' the target \eqn{t_j}{tj} for feature \eqn{j}{j}, and \eqn{w_j}{wj} is the
#' weight for feature \eqn{j}{j} (defaults to 1 for all features; see
#' \code{\link{add_feature_weights}} to specify weights). Additionally,
#' \eqn{B}{B} is the budget allocated for the solution, \eqn{c_i}{ci} is the
#' cost of planning unit \eqn{i}{i}, and \eqn{s}{s} is a scaling factor used to
#' shrink the costs so that the problem will return a cheapest solution when
#' there are multiple solutions that represent the same amount of all features
#' within the budget.
#'
#' @seealso \code{\link{objectives}}.
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
#' \donttest{
#' # solve problem
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution", axes = FALSE, box = FALSE)
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
