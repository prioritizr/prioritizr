#' @include internal.R Objective-class.R
NULL

#' Add maximum utility objective
#'
#' Set the objective of a conservation planning problem to
#' secure as much of the features as possible without exceeding a budget.
#' This objective does not use targets, and feature
#' weights should be used instead to increase the representation of certain
#' features by a solution.
#' Note that this objective does not aim to maximize as much of each feature as
#' possible, and so often results in solutions that are heavily biased towards
#' just a few features.
#'
#' @param x [problem()] object.
#'
#' @param budget `numeric` value specifying the maximum expenditure of
#'   the prioritization. For problems with multiple zones, the argument
#'   to `budget` can be (i) a single `numeric` value to specify a single budget
#'   for the entire solution or (ii) a `numeric` vector to specify
#'   a separate budget for each management zone.
#'
#' @details
#' The maximum utility objective seeks to maximize the overall level of
#' representation across a suite of conservation features, while keeping cost
#' within a fixed budget.
#' Additionally, weights can be used to favor the
#' representation of certain features over other features (see
#' [add_feature_weights()]). It is essentially calculated as a weighted
#' sum of the feature data inside the selected planning units.
#'
#' @section Mathematical formulation:
#' This objective can be expressed mathematically for a set of planning units
#' (\eqn{I}{I} indexed by \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed
#' by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Maximize} \space \sum_{i = 1}^{I} -s \space c_i \space x_i +
#' \sum_{j = 1}^{J} a_j w_j \\
#' \mathit{subject \space to} \\ a_j = \sum_{i = 1}^{I} x_i r_{ij} \space
#' \forall j \in J \\ \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Maximize sum_i^I (-s * ci * xi) + sum_j^J (aj * wj) subject to
#' aj = sum_i^I (xi * rij) for all j in J & sum_i^I (xi * ci) <= B}
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.,
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning
#' unit \eqn{i}{i}, \eqn{A_j}{Aj} is the amount of feature \eqn{j}{j}
#' represented in in the solution, and \eqn{w_j}{wj} is the weight for
#' feature \eqn{j}{j} (defaults to 1 for all features; see
#' [add_feature_weights()]
#' to specify weights). Additionally, \eqn{B}{B} is the budget allocated for
#' the solution, \eqn{c_i}{ci} is the cost of planning unit \eqn{i}{i}, and
#' \eqn{s}{s} is a scaling factor used to shrink the costs so that the problem
#' will return a cheapest solution when there are multiple solutions that
#' represent the same amount of all features within the budget.
#'
#' @section Notes:
#' In early versions (< 3.0.0.0), this function was named as
#' the `add_max_cover_objective` function. It was renamed to avoid
#' confusion with existing terminology.
#'
#' @inherit add_max_features_objective return
#'
#' @seealso
#' See [objectives] for an overview of all functions for adding objectives.
#' Also, see [add_feature_weights()] to specify weights for different features.
#'
#' @family objectives
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create problem with maximum utility objective
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_max_utility_objective(5000) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # create multi-zone problem with maximum utility objective that
#' # has a single budget for all zones
#' p2 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_max_utility_objective(5000) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(category_layer(s2), main = "solution", axes = FALSE)
#'
#' # create multi-zone problem with maximum utility objective that
#' # has separate budgets for each zone
#' p3 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_max_utility_objective(c(1000, 2000, 3000)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE)
#' }
#' @name add_max_utility_objective
NULL

#' @rdname add_max_utility_objective
#' @export
add_max_utility_objective <- function(x, budget) {
  # assert argument is valid
  assert_required(x)
  assert_required(budget)
  assert(
    is_conservation_problem(x),
    is.numeric(budget),
    all_finite(budget),
    all_positive(budget),
    is_budget_length(x, budget)
  )
  # add objective to problem
  x$add_objective(
    R6::R6Class(
      "MaximumUtilityObjective",
      inherit = Objective,
      public = list(
        name = "maximum utility objective",
        data = list(budget = budget),
        apply = function(x, y) {
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          invisible(
            rcpp_apply_max_utility_objective(
              x$ptr,
              unname(y$feature_abundances_in_planning_units()),
              y$planning_unit_costs(),
              self$get_data("budget")
            )
          )
        }
      )
    )$new()
  )
}
