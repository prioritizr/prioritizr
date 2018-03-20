#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add maximum coverage objective
#'
#' Set an objective to find the solution aims to represent one instance of
#' as many features as possible within a given budget. This type of objective
#' does not require the addition of targets. The mathematical formulation
#' underpinning this function is different from versions prior to 3.0.0.0;
#' see Details section for more information.
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
#'   of objectives}. Failing to do so will return a default error message
#'   when solving.
#'
#'   The maximum coverage problem seeks to find the set of planning units that
#'   maximizes the number of represented features, while keeping cost within a
#'   fixed budget. Here, features are treated as being represented if
#'   the reserve system contains at least a single instance of a feature
#'   (i.e. an amount greater than 1). This problem formulation has been
#'   used in conservation planning problems dealing with binary biodiversity
#'   data that indicate the presence/absence of suitable habitat
#'   (e.g. Church & Velle 1974). Check out the
#'   \code{\link{add_max_features_objective}} for a more
#'   generalized formulation which can accommodate user-specified representation
#'   targets.
#'
#'   This formulation is based on the historical maximum coverage reserve
#'   selection formulation (Church & Velle 1974; Church \emph{et al.} 1996).
#'   The maximum coverage objective for the reserve design problem can be
#'   expressed mathematically for a set of planning units (\eqn{I}{I} indexed by
#'   \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed by \eqn{j}{j}) as:
#'
#'   \deqn{\mathit{Maximize} \space \sum_{i = 1}^{I} -s \space c_i +
#'   \sum_{j = 1}^{J} y_j w_j \\
#'   \mathit{subject \space to} \\
#'   \sum_{i = 1}^{I} x_i r_{ij} >= y_j \times 1 \forall j \in J \\
#'   \sum_{i = 1}^{I} x_i c_i \leq B}{
#'   Maximize sum_i^I (-s * ci) + sum_j^J (yj * wj) subject to
#'   sum_i^I (xi * rij) >= (yj * 1) for all j in J &
#'   sum_i^I (xi * ci) <= B}
#'
#'   Here, \eqn{x_i}{xi} is the \code{\link{decisions}} variable (e.g.
#'   specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#'   (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning
#'   unit \eqn{i}{i}, \eqn{y_j}{yj} indicates if the solution has meet
#'   the target \eqn{t_j}{tj} for feature \eqn{j}{j}, and \eqn{w_j}{wj} is the
#'   weight for feature \eqn{j}{j} (defaults to 1 for all features; see
#'   \code{\link{add_feature_weights}} to specify weights). Additionally,
#'   \eqn{B}{B} is the budget allocated for the solution, \eqn{c_i}{ci} is the
#'   cost of planning unit \eqn{i}{i}, and \eqn{s}{s} is a scaling factor used
#'   to shrink the costs so that the problem will return a cheapest solution
#'   when there are multiple solutions that represent the same amount of all
#'   features within the budget.
#'
#'   Note that this formulation is functionally equivalent to the
#'   \code{\link{add_max_features_objective}} function with absolute targets
#'   set to 1. Please note that in versions prior to 3.0.0.0, this objective
#'   function implemented a different mathematical formulation. To
#    use the formulation implemented in versions prior to 3.0.0.0, please see
#'   the \code{\link{add_max_utility_objective}} function.
#'
#' @references
#' Church RL and Velle CR (1974) The maximum covering location problem.
#' \emph{Regional Science}, 32: 101--118.
#'
#' Church RL, Stoms DM, and Davis FW (1996) Reserve selection as a maximum
#' covering location problem. \emph{Biological Conservation}, 76: 105--112.
#'
#' @seealso \code{\link{add_feature_weights}}, \code{\link{objectives}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_zones_stack, sim_features, sim_features_zones)
#'
#' # threshold the feature data to generate binary biodiversity data
#' sim_binary_features <- sim_features
#' thresholds <- raster::quantile(sim_features, probs = 0.95, names = FALSE,
#'                                na.rm = TRUE)
#' for (i in seq_len(raster::nlayers(sim_features)))
#'   sim_binary_features[[i]] <- as.numeric(raster::values(sim_features[[i]]) >
#'                                          thresholds[[i]])
#'
#' # create problem with maximum utility objective
#' p1 <- problem(sim_pu_raster, sim_binary_features) %>%
#'       add_max_cover_objective(500) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#' }
#'
#' # threshold the multi-zone feature data to generate binary biodiversity data
#' sim_binary_features_zones <- sim_features_zones
#' for (z in n_zone(sim_features_zones)) {
#'   thresholds <- raster::quantile(sim_features_zones[[z]], probs = 0.95,
#'                                  names = FALSE, na.rm = TRUE)
#'   for (i in seq_len(n_feature(sim_features_zones))) {
#'     sim_binary_features_zones[[z]][[i]] <- as.numeric(
#'       raster::values(sim_features_zones[[z]][[i]]) > thresholds[[i]])
#'   }
#' }
#'
#' # create multi-zone problem with maximum utility objective that
#' # has a single budget for all zones
#' p2 <- problem(sim_pu_zones_stack, sim_binary_features_zones) %>%
#'       add_max_cover_objective(800) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(category_layer(s2), main = "solution", axes = FALSE, box = FALSE)
#' }
#'
#' # create multi-zone problem with maximum utility objective that
#' # has separate budgets for each zone
#' p3 <- problem(sim_pu_zones_stack, sim_binary_features_zones) %>%
#'       add_max_cover_objective(c(400, 400, 400)) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_max_cover_objective
NULL

#' @rdname add_max_cover_objective
#' @export
add_max_cover_objective <- function(x, budget) {
  # assert argument is valid
  # assert argument is valid
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
    "MaximumCoverageObjective",
    Objective,
    name = "Maximum coverage objective",
    parameters = parameters(p),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_max_cover_objective(x$ptr,
                y$planning_unit_costs(), self$parameters$get("budget")[[1]]))
    }))
}
