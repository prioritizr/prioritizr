#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add maximum coverage objective
#'
#' Set the objective of a conservation planning [problem()] to
#' represent at least one instance of as many features as possible within a
#' given budget. This objective does not use targets, and feature
#' weights should be used instead to increase the representation of certain
#' features by a solution.
#'
#' @param x [problem()] object.
#'
#' @param budget `numeric` value specifying the maximum expenditure of
#'   the prioritization. For problems with multiple zones, the argument
#'   to `budget` can be a single `numeric` value to specify a budget
#'   for the entire solution or a `numeric` vector to specify
#'   a budget for each each management zone.
#'
#' @details
#' The maximum coverage objective seeks to find the set of planning units that
#' maximizes the number of represented features, while keeping cost within a
#' fixed budget. Here, features are treated as being represented if
#' the reserve system contains at least a single instance of a feature
#' (i.e., an amount greater than 1). This formulation has often been
#' used in conservation planning problems dealing with binary biodiversity
#' data that indicate the presence/absence of suitable habitat
#' (e.g., Church & Velle 1974). Additionally, weights can be used to favor the
#' representation of certain features over other features (see
#' [add_feature_weights()]). Check out the
#' [add_max_features_objective()] for a more
#' generalized formulation which can accommodate user-specified representation
#' targets.
#'
#' @section Mathematical formulation:
#' This objective is based on the maximum coverage reserve
#' selection problem (Church & Velle 1974; Church *et al.* 1996).
#' The maximum coverage objective for the reserve design problem can be
#' expressed mathematically for a set of planning units (\eqn{I}{I} indexed by
#' \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Maximize} \space \sum_{i = 1}^{I} -s \space c_i \space x_i +
#' \sum_{j = 1}^{J} y_j w_j \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i r_{ij} \geq y_j \times 1 \forall j \in J \\
#' \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Maximize sum_i^I (-s * ci * xi) + sum_j^J (yj * wj) subject to
#' sum_i^I (xi * rij) >= (yj * 1) for all j in J &
#' sum_i^I (xi * ci) <= B}
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.,
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning
#' unit \eqn{i}{i}, \eqn{y_j}{yj} indicates if the solution has meet
#' the target \eqn{t_j}{tj} for feature \eqn{j}{j}, and \eqn{w_j}{wj} is the
#' weight for feature \eqn{j}{j} (defaults to 1 for all features; see
#' [add_feature_weights()] to specify weights). Additionally,
#' \eqn{B}{B} is the budget allocated for the solution, \eqn{c_i}{ci} is the
#' cost of planning unit \eqn{i}{i}, and \eqn{s}{s} is a scaling factor used
#' to shrink the costs so that the problem will return a cheapest solution
#' when there are multiple solutions that represent the same amount of all
#' features within the budget.
#'
#' @section Notes:
#' In early versions (< 3.0.0.0), the mathematical formulation
#' underpinning this function was very different. Specifically,
#' as described above, the function now follows the formulations outlined in
#' Church *et al.* (1996). The old formulation is now provided by the
#' [add_max_utility_objective()] function.
#'
#' @references
#' Church RL and Velle CR (1974) The maximum covering location problem.
#' *Regional Science*, 32: 101--118.
#'
#' Church RL, Stoms DM, and Davis FW (1996) Reserve selection as a maximum
#' covering location problem. *Biological Conservation*, 76: 105--112.
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
#' sim_pu_zones_raster <- get_sim_zones_pu_raster()
#' sim_features <- get_sim_features()
#' sim_features_zones <- get_sim_zones_features()
#'
#' # threshold the feature data to generate binary biodiversity data
#' sim_binary_features <- sim_features
#' thresholds <- terra::global(
#'   sim_features, fun = quantile, probs = 0.5, na.rm = TRUE
#' )
#' for (i in seq_len(terra::nlyr(sim_features))) {
#'   sim_binary_features[[i]] <- terra::as.int(
#'     sim_features[[i]] > thresholds[[1]][[i]]
#'   )
#' }
#'
#' # create problem with maximum utility objective
#' p1 <-
#'   problem(sim_pu_raster, sim_binary_features) %>%
#'   add_max_cover_objective(500) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # threshold the multi-zone feature data to generate binary biodiversity data
#' sim_binary_features_zones <- sim_features_zones
#' for (z in number_of_zones(sim_features_zones)) {
#'   thresholds <- terra::global(
#'     sim_features_zones[[z]], fun = quantile, probs = 0.5, na.rm = TRUE
#'   )
#'   for (i in seq_len(number_of_features(sim_features_zones))) {
#'     sim_binary_features_zones[[z]][[i]] <- terra::as.int(
#'       sim_features_zones[[z]][[i]] > thresholds[[1]][[i]]
#'     )
#'   }
#' }
#'
#' # create multi-zone problem with maximum utility objective that
#' # has a single budget for all zones
#' p2 <-
#'   problem(sim_pu_zones_raster, sim_binary_features_zones) %>%
#'   add_max_cover_objective(800) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
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
#'   problem(sim_pu_zones_raster, sim_binary_features_zones) %>%
#'   add_max_cover_objective(c(400, 400, 400)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE)
#' }
#' @name add_max_cover_objective
NULL

#' @rdname add_max_cover_objective
#' @export
add_max_cover_objective <- function(x, budget) {
  # assert argument is valid
  rlang::check_required(x)
  rlang::check_required(budget)
  assert(
    is_conservation_problem(x),
    is.numeric(budget),
    all_finite(budget),
    all_positive(budget),
    min(budget) > 0
  )
  budget_msg <- ifelse(
    number_of_zones(x) == 1,
    "{.arg budget} must be a single numeric value.",
    paste(
      "{.arg budget} must have a single numeric value,",
      "or a value for each zone in {.arg x}."
    )
  )
  assert(
    length(budget) %in% c(1, number_of_zones(x)),
    msg = budget_msg
  )
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
    "MaximumCoverageObjective",
    Objective,
    name = "Maximum coverage objective",
    parameters = parameters(p),
    apply = function(self, x, y) {
      assert(
        inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"),
        .internal = TRUE
      )
      invisible(
        rcpp_apply_max_cover_objective(
          x$ptr, y$planning_unit_costs(), self$parameters$get("budget")[[1]]
        )
      )
    }
  ))
}
