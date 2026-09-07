#' @include internal.R Objective-class.R
NULL

#' Add maximum number of targets met objective
#'
#' Set the objective of a conservation planning problem to
#' fulfill as many targets as possible, whilst ensuring that the cost of the
#' solution does not exceed a budget. Note that this objective does not
#' value the partial achievement of a given target---only whether or not
#' the target has been met. For this reason, we generally recommend using the
#' the minimum shortfall objective ([add_min_shortfall_objective()]
#' instead for budget-limited scenarios.
#'
#' @inheritParams add_max_wtd_sum_objective
#'
#' @details
#' The maximum number of targets met objective is an enhanced version of the
#' maximum coverage objective [add_max_cover_objective()] because
#' targets can be used to ensure that a certain amount of each feature is
#' required in order for them to be adequately represented (similar to the
#' minimum set objective (see [add_min_set_objective()]). This
#' objective finds the set of planning units that meets representation targets
#' for as many features as possible while staying within a fixed budget
#' (inspired by Cabeza and Moilanen 2001). Additionally, weights can be used
#' to favor the representation of certain features over other features (see
#' [add_feature_weights()]). If multiple solutions can meet the same
#' number of weighted targets while staying within budget, the cheapest
#' solution is returned.
#'
#' @section Mathematical formulation:
#' This objective can be expressed mathematically for a set of planning units
#'  (\eqn{I}{I} indexed by
#' \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Maximize} \space \sum_{j = 1}^{J} y_j w_j \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i r_{ij} \geq y_j t_j \forall j \in J \\
#' \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Maximize sum_j^J (yj * wj) subject to
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
#' \eqn{B}{B} is the budget allocated for the solution, and \eqn{c_i}{ci} is the
#' cost of planning unit \eqn{i}{i}.
#'
#' @section Notes:
#' In previous versions (< 9.0.0), this function was called the
#' `add_max_features_objective()` and has since been renamed to
#' provide greater clarity. Additionally, it previously had extra
#' terms to help minimize the solution cost. Although these terms
#' have since been removed to reduce solve time,
#' this behavior can still be achieved by
#' building a multi-objective optimization problem and specifying the
#' first problem based on this objective function and the second
#' problem based on minimizing penalties (via [add_min_penalties_objective()])
#' with penalties set according to cost values
#' (via [add_linear_penalties()]).
#'
#' @inherit add_max_wtd_sum_objective return
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
#' @examplesIf prioritizr::do_run_example()
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create problem with maximum number of targets met objective
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_max_n_targets_met_objective(1800) %>%
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
#' # create multi-zone problem with maximum number of targets met objective,
#' # 10% representation targets for each feature, and set
#' # a budget such that the total maximum expenditure in all zones
#' # cannot exceed 3000
#' p2 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_max_n_targets_met_objective(3000) %>%
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
#' # create multi-zone problem with maximum number of targets met objective,
#' # 10% representation targets for each feature, and set
#' # separate budgets for each management zone
#' p3 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_max_n_targets_met_objective(c(3000, 3000, 3000)) %>%
#'   add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE)
#'
#' @name add_max_n_targets_met_objective
NULL

#' @rdname add_max_n_targets_met_objective
#' @export
add_max_n_targets_met_objective <- function(x, budget) {
  # assert arguments are valid
  assert_required(x)
  assert_required(budget)
  assert(is_conservation_problem(x))
  if (!is.null(budget)) {
    assert(
      is.numeric(budget),
      all_finite(budget),
      all_positive(budget),
      is_budget_length(x, budget)
    )
  }
  # add objective to problem
  x$add_objective(
    R6::R6Class(
      "MaximumNTargetsMetObjective",
      inherit = Objective,
      public = list(
        name = "maximum number targets met objective",
        has_weights = TRUE,
        has_targets = TRUE,
        data = list(budget = budget),
        apply = function(x, y, weights) {
          # assert valid arguments
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            is.numeric(weights),
            .internal = TRUE
          )
          # if needed, replace budget with NA value
          b <- self$get_data("budget")
          if (is.null(b)) b <- NA_real_
          # apply objective
          invisible(
            rcpp_apply_max_n_targets_met_objective(
              x$ptr,
              y$feature_targets(),
              y$planning_unit_costs(),
              b,
              weights
            )
          )
        }
      )
    )$new()
  )
}
