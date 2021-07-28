#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add minimum shortfall objective
#'
#' Set the objective of a conservation planning [problem()] to
#' minimize the overall shortfall for as many [targets] as possible while
#' ensuring that the cost of the solution does not exceed a budget.
#'
#' @inheritParams add_max_features_objective
#'
#' @details
#' The minimum shortfall objective aims to
#' find the set of planning units that minimize the overall
#' (weighted sum) shortfall for the
#' representation targets---that is, the fraction of each target that
#' remains unmet---for as many features as possible while staying within a
#' fixed budget (inspired by Table 1, equation IV, Arponen *et al.*
#' 2005). Additionally, weights can be used
#' to favor the representation of certain features over other features (see
#' [add_feature_weights()].
#'
#' @section Mathematical formulation:
#' This objective can be expressed mathematically for a set of planning units
#' (\eqn{I}{I} indexed by \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed
#' by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Minimize} \space  \sum_{j = 1}^{J} w_j \frac{y_j}{t_j} \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i r_{ij} + y_j \geq t_j \forall j \in J \\
#' \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Minimize sum_j^J wj * (yj / tj) subject to
#' sum_i^I (xi * rij) + yj >= tj for all j in J &
#' sum_i^I (xi * ci) <= B}
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning
#' unit \eqn{i}{i}, \eqn{t_j}{tj} is the representation target for feature
#' \eqn{j}{j}, \eqn{y_j}{yj} denotes the representation shortfall for
#' the target \eqn{t_j}{tj} for feature \eqn{j}{j}, and \eqn{w_j}{wj} is the
#' weight for feature \eqn{j}{j} (defaults to 1 for all features; see
#' [add_feature_weights()] to specify weights). Additionally,
#' \eqn{B}{B} is the budget allocated for the solution, \eqn{c_i}{ci} is the
#' cost of planning unit \eqn{i}{i}. Note that \eqn{y_j}{yj} is a continuous
#' variable bounded between zero and infinity, and denotes the shortfall
#' for target \eqn{j}{j}.
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
#' @references
#' Arponen A, Heikkinen RK, Thomas CD, and Moilanen A (2005) The value of
#' biodiversity in reserve selection: representation, species weighting, and
#' benefit functions. *Conservation Biology*, 19: 2009--2014.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_zones_stack, sim_features, sim_features_zones)
#'
#' # create problem with minimum shortfall objective
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_shortfall_objective(1800) %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#' \dontrun{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#' }
#'
#' # create multi-zone problem with minimum shortfall objective,
#' # with 10% representation targets for each feature, and set
#' # a budget such that the total maximum expenditure in all zones
#' # cannot exceed 3000
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_shortfall_objective(3000) %>%
#'       add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#' \dontrun{
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(category_layer(s2), main = "solution", axes = FALSE, box = FALSE)
#' }
#' # create multi-zone problem with minimum shortfall objective,
#' # with 10% representation targets for each feature, and set
#' # separate budgets for each management zone
#' p3 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_shortfall_objective(c(3000, 3000, 3000)) %>%
#'       add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#' \dontrun{
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_min_shortfall_objective
NULL

#' @rdname add_min_shortfall_objective
#' @export
add_min_shortfall_objective <- function(x, budget) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
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
    "MinimumShortfallObjective",
    Objective,
    name = "Minimum shortfall objective",
    parameters = parameters(p),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_min_shortfall_objective(
        x$ptr, y$feature_targets(), y$planning_unit_costs(),
        self$parameters$get("budget")[[1]]))
    }))
}
