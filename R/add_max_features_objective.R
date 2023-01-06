#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add maximum feature representation objective
#'
#' Set the objective of a conservation planning [problem()] to
#' fulfill as many targets as possible while ensuring that the cost of the
#' solution does not exceed a budget.
#'
#' @inheritParams add_max_utility_objective
#'
#' @details
#' The maximum feature representation objective is an enhanced version of the
#' maximum coverage objective [add_max_cover_objective()] because
#' targets can be used to ensure that a certain amount of each feature is
#' required in order for them to be adequately represented (similar to the
#' minimum set objective (see [add_min_set_objective()]). This
#' objective finds the set of planning units that meets representation targets
#' for as many features as possible while staying within a fixed budget
#' (inspired by Cabeza and Moilanen 2001). Additionally, weights can be used
#  to favor the representation of certain features over other features (see
#' [add_feature_weights()]). If multiple solutions can meet the same
#' number of weighted targets while staying within budget, the cheapest
#' solution is returned.
#'
#' @section Mathematical formulation:
#' This objective can be expressed mathematically for a set of planning units
#  (\eqn{I}{I} indexed by
#' \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Maximize} \space \sum_{i = 1}^{I} -s \space c_i \space x_i +
#' \sum_{j = 1}^{J} y_j w_j \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i r_{ij} \geq y_j t_j \forall j \in J \\
#' \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Maximize sum_i^I (-s * ci * xi) + sum_j^J (yj * wj) subject to
#' sum_i^I (xi * rij) >= (yj tj) for all j in J &
#' sum_i^I (xi * ci) <= B}
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.,
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning
#' unit \eqn{i}{i}, \eqn{t_j}{tj} is the representation target for feature
#' \eqn{j}{j}, \eqn{y_j}{yj} indicates if the solution has meet
#' the target \eqn{t_j}{tj} for feature \eqn{j}{j}, and \eqn{w_j}{wj} is the
#' weight for feature \eqn{j}{j} (defaults to 1 for all features; see
#' [add_feature_weights()] to specify weights). Additionally,
#' \eqn{B}{B} is the budget allocated for the solution, \eqn{c_i}{ci} is the
#' cost of planning unit \eqn{i}{i}, and \eqn{s}{s} is a scaling factor used
#' to shrink the costs so that the problem will return a cheapest solution
#' when there are multiple solutions that represent the same amount of all
#' features within the budget.
#'
#' @inherit add_min_set_objective return
#'
#' @seealso
#' See [objectives] for an overview of all functions for adding objectives.
#' Also, see [targets] for an overview of all functions for adding targets, and
#' [add_feature_weights()] to specify weights for different features.
#'
#' @family objectives
#'
#' @references
#' Cabeza M and Moilanen A (2001) Design of reserve networks and the
#' persistence of biodiversity. *Trends in Ecology & Evolution*,
#' 16: 242--248.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_pu_zones_raster <- get_sim_zones_pu_raster()
#' sim_features_zones <- get_sim_zones_features()
#'
#' # create problem with maximum features objective
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_max_features_objective(1800) %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # create multi-zone problem with maximum features objective,
#' # with 10% representation targets for each feature, and set
#' # a budget such that the total maximum expenditure in all zones
#' # cannot exceed 3000
#' p2 <-
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_max_features_objective(3000) %>%
#'   add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(category_layer(s2), main = "solution", axes = FALSE)
#'
#' # create multi-zone problem with maximum features objective,
#' # with 10% representation targets for each feature, and set
#' # separate budgets for each management zone
#' p3 <-
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_max_features_objective(c(3000, 3000, 3000)) %>%
#'   add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE)
#' }
#' @name add_max_features_objective
NULL

#' @rdname add_max_features_objective
#' @export
add_max_features_objective <- function(x, budget) {
  # assert arguments are valid
  assertthat::assert_that(
    is_conservation_problem(x),
    is.numeric(budget),
    all_finite(budget),
    all(budget >= 0.0),
    min(budget) > 0
  )
  if (length(budget) > 1) {
    assertthat::assert_that(
      length(budget) == number_of_zones(x),
      msg = paste(
        "argument to budget should contain a single value",
        "or a value for each zone"
      )
    )
  }
  # make parameter
  if (length(budget) == 1) {
    p <- numeric_parameter(
      "budget", budget, lower_limit = 0,
      upper_limit = sum(x$planning_unit_costs(), na.rm = TRUE)
    )
  } else {
    p <- numeric_parameter_array(
      "budget", budget, x$zone_names(), lower_limit = 0,
      upper_limit = colSums(x$planning_unit_costs(), na.rm = TRUE)
    )
  }
  # add objective to problem
  x$add_objective(pproto(
    "MaximumRepresentationObjective",
    Objective,
    name = "Maximum representation objective",
    parameters = parameters(p),
    apply = function(self, x, y) {
      assertthat::assert_that(
        inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem")
      )
      invisible(
        rcpp_apply_max_features_objective(
          x$ptr, y$feature_targets(), y$planning_unit_costs(),
          self$parameters$get("budget")[[1]]
        )
      )
    }
  ))
}
