#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add Maximum Utility Objective
#'
#' Set an objective to find the solution that secures as much of each feature
#' as possible without exceeding the budget. This type of objective does not
#' require the addition of targets.
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
#' of objectives}. Failing to do so will return a default error message
#' when solving.
#'
#' The maximum utility problem seeks to find the set of planning units that
#' maximizes the overall level of representation across a suite of conservation
#' features, while keeping cost within a fixed budget. This problem is roughly
#' the opposite of what the conservation planning software Marxan does. In
#' versions prior to 3.0.0.0, this objective function was implemented in the
#' \code{\link{add_max_cover_objective}} but has been renamed as
#' \code{\link{add_max_utility_objective}} to avoid confusion with historical
#' formulations of the maximum coverage problem.
#'
#' The maximum utility objective for the reserve design problem can be
#' expressed mathematically for a set of planning units (\eqn{I}{I} indexed by
#' \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Maximize} \space \sum_{i = 1}^{I} s \space c_i +
#' \sum_{j = 1}^{J} a_j w_j \\
#' \mathit{subject \space to} \\ a_j = \sum_{i = 1}^{I} x_i r_{ij} \space
#' \forall j \in J \\ \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Maximize sum_i^I (s * ci) + sum_j^J (aj * wj) subject to
#' aj = sum_i^I (xi * rij) for all j in J & sum_i^I (xi * ci) <= B}
#'
#' Here, \eqn{x_i}{xi} is the \code{\link{decisions}} variable (e.g.
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning unit
#' \eqn{i}{i}, \eqn{A_j}{Aj} is the amount of feature \eqn{j}{j} represented in
#' in the solution, and \eqn{w_j}{wj} is the weight for feature \eqn{j}{j}
#' (defaults to 1 for all features; see \code{\link{add_feature_weights}}
#' to specify weights). Additionally, \eqn{B}{B} is the budget allocated for
#' the solution, \eqn{c_i}{ci} is the cost of planning unit \eqn{i}{i}, and
#' \eqn{s}{s} is a scaling factor used to shrink the costs so that the problem
#' will return a cheapest solution when there are multiple solutions that
#' represent the same amount of all features within the budget.
#'
#' @seealso \code{\link{add_feature_weights}}, \code{\link{objectives}}.
#'
#' @examples
#' # not implemented
#' # # load data
#' # data(sim_pu_raster, sim_features)
#' #
#' # # create problem
#' # p <- problem(sim_pu_raster, sim_features) %>%
#' #      add_max_utility_objective(5000) %>%
#' #      add_binary_decisions()
#' # \donttest{
#' # # solve problem
#' # s <- solve(p)
#' #
#' # # plot solution
#' # plot(s, main = "solution", axes = FALSE, box = FALSE)
#' # }
#'
#' @name add_max_utility_objective
NULL

#' @rdname add_max_utility_objective
#' @export
add_max_utility_objective <- function(x, budget) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(budget))),
                          assertthat::is.scalar(budget),
                          isTRUE(budget > 0.0))
  # make parameter
  p <- numeric_parameter("budget", budget, lower_limit = 0,
                         upper_limit = sum(x$planning_unit_costs()))
  # add objective to problem
  x$add_objective(pproto(
    "MaximumUtilityObjective",
    Objective,
    name = "Maximum utility objective",
    parameters = parameters(p),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_max_utility_objective(x$ptr,
                  unname(y$feature_abundances_in_planning_units()),
                         y$planning_unit_costs(),
                         self$parameters$get("budget")))
    }))
}
