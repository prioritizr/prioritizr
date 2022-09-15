#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add minimum largest shortfall objective
#'
#' Set the objective of a conservation planning [problem()] to
#' minimize the largest target shortfall while ensuring that
#' the cost of the solution does not exceed a budget. Note that if the
#' target shortfall for a single feature cannot be decreased beyond a certain
#' point (e.g., because all remaining planning units occupied by that feature
#' are too costly or are locked out), then solutions may only use a small
#' proportion of the specified budget.
#'
#' @inheritParams add_max_features_objective
#'
#' @details
#' The minimum largest shortfall objective aims to
#' find the set of planning units that minimize the largest
#' shortfall for any of the representation targets---that is, the fraction of
#' each target that remains unmet---for as many features as possible while
#' staying within a fixed budget. This objective is different from the
#' minimum shortfall objective ([add_min_shortfall_objective()]) because this
#' objective minimizes the largest (maximum) target shortfall,
#' whereas the minimum shortfall objective
#' minimizes the total (weighted sum) of the target shortfalls.
#' Note that this objective function is not compatible with feature weights
#' ([add_feature_weights()]).
#'
#' @section Mathematical formulation:
#' This objective can be expressed mathematically for a set of planning units
#' (\eqn{I}{I} indexed by \eqn{i}{i}) and a set of features (\eqn{J}{J}
#' indexed by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Minimize} \space l \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i r_{ij} + y_j \geq t_j \forall j \in J \\
#' l \geq \frac{y_j}{t_j} \forall j \in J \\
#' \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Minimize l subject to
#' sum_i^I (xi * rij) + yj >= tj for all j in J &
#' l >= (yj / tj) for all j in J &
#' sum_i^I (xi * ci) <= B}
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.,
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning
#' unit \eqn{i}{i}, and \eqn{t_j}{tj} is the representation target for feature
#' \eqn{j}{j}.
#' Additionally, \eqn{y_j}{yj} denotes the target shortfall for
#' the target \eqn{t_j}{tj} for feature \eqn{j}{j}, and
#' \eqn{l}{l} denotes the largest target shortfall.
#' Furthermore, \eqn{B}{B} is the budget allocated for the solution,
#' \eqn{c_i}{ci} is the cost of planning unit \eqn{i}{i}. Note that
#' \eqn{y_j}{yj} and \eqn{s} are continuous variables bounded between zero
#' and infinity.
#'
#' @seealso
#' See [objectives] for an overview of all functions for adding objectives.
#' Also, see [targets] for an overview of all functions for adding targets, and
#' [add_feature_weights()] to specify weights for different features.
#'
#' @family objectives
#'
#' @inherit add_min_set_objective return
#'
#' @examples
#' \dontrun{
#' # load data
#' data(sim_pu_raster, sim_pu_zones_stack, sim_features, sim_features_zones)
#'
#' # create problem with minimum largest shortfall objective
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_largest_shortfall_objective(1800) %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#'
#' # create multi-zone problem with minimum largest shortfall objective,
#' # with 10% representation targets for each feature, and set
#' # a budget such that the total maximum expenditure in all zones
#' # cannot exceed 1800
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_largest_shortfall_objective(1800) %>%
#'       add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(category_layer(s2), main = "solution", axes = FALSE, box = FALSE)
#'
#' # create multi-zone problem with minimum largest shortfall objective,
#' # with 10% representation targets for each feature, and set
#' # separate budgets of 1800 for each management zone
#' p3 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_largest_shortfall_objective(c(1800, 1800, 1800)) %>%
#'       add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_min_largest_shortfall_objective
NULL

#' @rdname add_min_largest_shortfall_objective
#' @export
add_min_largest_shortfall_objective <- function(x, budget) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    is.numeric(budget),
    all(is.finite(budget)),
    all(budget >= 0.0),
    isTRUE(min(budget) > 0),
    length(budget) == 1 ||
      length(budget) == number_of_zones(x))
  # make parameter
  if (length(budget) == 1) {
    p <- numeric_parameter("budget", budget, lower_limit = 0,
                           upper_limit = sum(x$planning_unit_costs(),
                                             na.rm = TRUE))
  } else {
    p <- numeric_parameter_array("budget", budget, x$zone_names(),
                                 lower_limit = 0,
                                 upper_limit = colSums(x$planning_unit_costs(),
                                                       na.rm = TRUE))
  }
  # add objective to problem
  x$add_objective(pproto(
    "MinimumLargestShortfallObjective",
    Objective,
    name = "Minimum largest shortfall objective",
    parameters = parameters(p),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_min_largest_shortfall_objective(
        x$ptr, y$feature_targets(), y$planning_unit_costs(),
        self$parameters$get("budget")[[1]]))
    }))
}
