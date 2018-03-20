#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add maximum feature representation objective
#'
#' Set an objective to find the solution that aims to fulfill as many targets
#' as possible while ensuring that the cost of the solution does not exceed
#' a budget.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param budget \code{numeric} value specifying the maximum expenditure of
#'   the prioritization. For problems with multiple zones, the argument
#'   to \code{budget} can be a single \code{numeric} value to specify a budget
#'   for the entire solution or a \code{numeric} \code{vector} to specify
#'   a budget for each each management zone.
#'
#' @details A problem objective is used to specify the overall goal of the
#'   conservation planning problem. Please note that \strong{all conservation
#'   planning problems formulated in the prioritizr package require the addition
#'   of both objectives and targets}. Failing to do so will return a default
#'   error message  when solving.
#'
#'   The maximum feature representation objective is an enhanced version of the
#'   maximum coverage objective \code{\link{add_max_cover_objective}} because
#'   targets can be used to ensure that a certain amount of each feature is
#'   required in order for them to be adequately represented (similar to the
#'   minimum set objective (see \code{\link{add_min_set_objective}}). This
#'   objective finds the set of planning units that meets representation targets
#'   for as many features as possible while staying within a fixed budget.
#'   Additionally, weights can be used to favor the
#'   representation of certain features over other features (see
#'   \code{\link{add_feature_weights}}). If multiple solutions can meet the same
#'   number of weighted targets while staying within budget, the cheapest
#'   solution is returned.
#'
#'   The maximum feature objective for the reserve design problem can be
#'   expressed mathematically for a set of planning units (\eqn{I}{I} indexed by
#'   \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed by \eqn{j}{j}) as:
#'
#'   \deqn{\mathit{Maximize} \space \sum_{i = 1}^{I} -s \space c_i +
#'   \sum_{j = 1}^{J} y_j w_j \\
#'   \mathit{subject \space to} \\
#'   \sum_{i = 1}^{I} x_i r_{ij} >= y_j t_j \forall j \in J \\
#'   \sum_{i = 1}^{I} x_i c_i \leq B}{
#'   Maximize sum_i^I (-s * ci) + sum_j^J (yj * wj) subject to
#'   sum_i^I (xi * rij) >= (yj tj) for all j in J &
#'   sum_i^I (xi * ci) <= B}
#'
#'   Here, \eqn{x_i}{xi} is the \code{\link{decisions}} variable (e.g.
#'   specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#'   (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning
#'   unit \eqn{i}{i}, \eqn{t_j}{tj} is the representation target for feature
#'   \eqn{j}{j}, \eqn{y_j}{yj} indicates if the solution has meet
#'   the target \eqn{t_j}{tj} for feature \eqn{j}{j}, and \eqn{w_j}{wj} is the
#'   weight for feature \eqn{j}{j} (defaults to 1 for all features; see
#'   \code{\link{add_feature_weights}} to specify weights). Additionally,
#'   \eqn{B}{B} is the budget allocated for the solution, \eqn{c_i}{ci} is the
#'   cost of planning unit \eqn{i}{i}, and \eqn{s}{s} is a scaling factor used
#'   to shrink the costs so that the problem will return a cheapest solution
#'   when there are multiple solutions that represent the same amount of all
#'   features within the budget.
#'
#' @seealso \code{\link{add_feature_weights}}, \code{\link{objectives}}.
#'
#' @inherit add_min_set_objective return
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_zones_stack, sim_features, sim_features_zones)
#'
#' # create problem with maximum features objective
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_max_features_objective(1500) %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#' }
#'
#' # create multi-zone problem with maximum features objective,
#' # with 10 % representation targets for each feature, and set
#' # a budget such that the total maximum expenditure in all zones
#' # cannot exceed 3000
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_max_features_objective(3000) %>%
#'       add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(category_layer(s2), main = "solution", axes = FALSE, box = FALSE)
#' }
#' # create multi-zone problem with maximum features objective,
#' # with 10 % representation targets for each feature, and set
#' # separate budgets for each management zone
#' p3 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_max_features_objective(c(3000, 3000, 3000)) %>%
#'       add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_max_features_objective
NULL

#' @rdname add_max_features_objective
#' @export
add_max_features_objective <- function(x, budget) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          is.numeric(budget),
                          all(is.finite(budget)),
                          all(budget >= 0.0),
                          isTRUE(min(budget) > 0),
                          length(budget) == 1 ||
                            length(budget) == x$number_of_zones())
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
    "MaximumRepresentationObjective",
    Objective,
    name = "Maximum representation objective",
    parameters = parameters(p),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_max_features_objective(
        x$ptr, y$feature_targets(), y$planning_unit_costs(),
        self$parameters$get("budget")[[1]]))
    }))
}
