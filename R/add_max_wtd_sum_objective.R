#' @include internal.R Objective-class.R
NULL

#' Add maximum weighted sum objective
#'
#' Set the objective of a conservation planning problem to
#' maximize the weighted sum of the features represented by the solution
#' as much as possible without exceeding a budget.
#' This objective does not use targets, and feature
#' weights should be used instead to increase the representation of particular
#' features by a solution.
#' Note that this objective does not account for complementarity and so often
#' fails to produce solutions that represent a variety of different features
#' (Kirkpatrick 1983).
#' Although this objective can be valid when considering certain types
#' of features (e.g., ecosystem services), we caution that it is not
#' suitable for features that pertain to species distribution or ecosystem
#' classification data.
#' In general, we **strongly advise** against using this objective because
#' -- except under very specific conditions --
#' it has "repeatedly been shown to identify priorities that are biologically
#' ineffective and economically inefficient" (Brown *et al.* 2015).
#'
#' @inheritParams add_max_cover_objective
#'
#' @details
#' The maximum weighted sum objective seeks to maximize the overall level of
#' representation across a suite of conservation features, while keeping cost
#' within a fixed budget.
#' Additionally, weights can be used to favor the
#' representation of particular features over other features (see
#' [add_feature_weights()]). It involves calculating scores
#' for each planning unit based on the feature data and weights,
#' and then selecting the combination of planning units that
#' would maximize the sum of these scores.
#' Please note that such scoring systems have considerable limitations
#' and -- except in rare cases -- are not suitable for modern systematic
#' conservation planning (Game *et al.* 2006).
#' We emphasize that this objective should not be used simply because you
#' do not have the time, data, or expertise to set meaningful targets.
#' Indeed, this objective should only be used if you have an expert-level
#' understanding of the limitations of this objective and are confident that
#' such limitations will not present issues for your conservation planning
#' exercise.
#'
#' @section Mathematical formulation:
#' This objective can be expressed mathematically for a set of planning units
#' (\eqn{I}{I} indexed by \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed
#' by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Maximize} \space \sum_{j = 1}^{J} a_j w_j \\
#' \mathit{subject \space to} \\ a_j = \sum_{i = 1}^{I} x_i r_{ij} \space
#' \forall j \in J \\ \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Maximize sum_j^J (aj * wj) subject to
#' aj = sum_i^I (xi * rij) for all j in J & sum_i^I (xi * ci) <= B}
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.,
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning
#' unit \eqn{i}{i}, \eqn{a_j}{aj} is the amount of feature \eqn{j}{j}
#' represented in in the solution, and \eqn{w_j}{wj} is the weight for
#' feature \eqn{j}{j} (defaults to 1 for all features; see
#' [add_feature_weights()]
#' to specify weights). Additionally, \eqn{B}{B} is the budget allocated for
#' the solution, and \eqn{c_i}{ci} is the cost of planning unit \eqn{i}{i}.
#'
#' @section Notes:
#' In early versions (< 9.0.0.0), this function was named as
#' the `add_max_cover_objective()` and the `add_max_utility_objective()`
#' function. It has since been renamed for clarity.
#' Additionally, in previous versions (< 9.0.0), this function had extra
#' terms to help minimize the solution cost. Although these terms
#' have since been removed to reduce solve time,
#' this behavior can still be achieved by
#' building a multi-objective optimization problem and specifying the
#' first problem based on this objective function and the second
#' problem based on minimizing penalties (via [add_min_penalties_objective()])
#' with penalties set according to cost values
#' (via [add_linear_penalties()]).
#'
#' @inherit add_max_cover_objective return
#'
#' @seealso
#' See [objectives] for an overview of all functions for adding objectives.
#' Also, see [add_feature_weights()] to specify weights for different features.
#'
#' @family objectives
#'
#' @references
#' Brown CJ, Bode M, Venter O, Barnes MD, McGowan J, Runge CA, Watson JEM,
#' and Possingham HP (2015) Effective conservation requires clear objectives and
#' prioritizing actions, not places or species.
#' *Proceedings of the National Academy of Sciences* 112: E4342.
#'
#' Game ET, Kareiva P, and Possingham HP (2013) Six common mistakes in
#' conservation priority setting. *Conservation Biology*, 27: 480--485.
#'
#' Kirkpatrick JB (1983) An iterative method for establishing priorities for
#' the selection of nature reserves: An example from Tasmania.
#' *Biological Conservation*, 25: 127--134.
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
#'   add_max_wtd_sum_objective(5000) %>%
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
#'   add_max_wtd_sum_objective(5000) %>%
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
#'   add_max_wtd_sum_objective(c(1000, 2000, 3000)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE)
#' }
#' @name add_max_wtd_sum_objective
NULL

#' @rdname add_max_wtd_sum_objective
#' @export
add_max_wtd_sum_objective <- function(x, budget) {
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
  # display message about using the function
  cli::cli_inform(
    message = c(
      "i" = paste(
        "{.fn add_max_wtd_sum_objective} has severe limitations",
        " - use with caution."
      )
    )
  )
  # add objective to problem
  x$add_objective(
    R6::R6Class(
      "MaximumWtdSumObjective",
      inherit = Objective,
      public = list(
        name = "maximum weighted sum objective",
        has_weights = TRUE,
        has_targets = FALSE,
        data = list(budget = budget),
        apply = function(x, y, weights) {
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            is.numeric(weights),
            .internal = TRUE
          )
          invisible(
            rcpp_apply_max_wtd_sum_objective(
              x$ptr,
              unname(y$feature_positive_abundances_in_planning_units()),
              y$has_negative_feature_data(),
              y$planning_unit_costs(),
              self$get_data("budget"),
              weights
            )
          )
        }
      )
    )$new()
  )
}
