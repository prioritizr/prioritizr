#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add Maximum Coverage Objective
#'
#' Set an objective to find the solution that secures as much of each feature as possible
#' without exceeding the budget. This type of objective does not require the addition of targets.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param budget \code{numeric} value specifying the maximum expenditure of
#' the prioritization.
#'
#' @details
#' A problem objective is used to specify the overall goal of the
#' conservation planning problem. Please note that \strong{all conservation planning 
#' problems formulated in the prioritizr package require the addition of
#' objectives}. Failing to do so will return a default error message 
#' when solving.
#' 
#' The maximum coverage problem seeks to find the set of planning units that 
#' maximizes the overall level of representation across a suite of conservation 
#' features, while keeping cost within a fixed budget. This problem is roughly 
#' the opposite of what the conservation planning software Marxan does. 
#' 
#' The maximum coverage problem can be stated mathematically, for \emph{n} 
#' planning units and \emph{m} conservation features, as:
#' 
#' \deqn{\text{Maximize} \sum_{i=1}^{m} \sum_{j=1}^{n} x_j r_{ij} + \text{ subject to } 
#' \sum_{i=1}^{n}x_i c_i \leq B}{Maximize \sum^m \sum^n (xj*rij) subject to \sum^n 
#' (xi*ci) \le B}
#' 
#' where \eqn{x_i}{xi} is a binary decision variable specifying whether planning unit 
#' \emph{i} has been selected (1) or not (0), \eqn{c_i}{ci} is the cost of planning unit 
#' \emph{i}, \eqn{r_ij}{rij} is the representation level of feature \emph{i} in planning 
#' unit \emph{j}, and \emph{B} is the budget.
#'
#' @seealso \code{\link{objectives}}, \code{\link{constraints}}, \code{\link{problem}},
#'   \code{\link{targets}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features, sim_phylogeny)
#'
#' # create base problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'  add_relative_targets(0.1)
#'
#  # create problem with added minimum set objective
#' p1 <- p %>% add_min_set_objective()
#' # create problem with added maximum coverage objective
#' # note that this objective does not use targets
#' p2 <- p %>% add_max_cover_objective(5000)
#'
#' # create problem with added maximum feature representation objective
#' p3 <- p %>% add_max_features_objective(5000)
#'
#' # create problem with added maximum phylogenetic representation objective
#' p4 <- p %>% add_max_phylo_objective(5000, sim_phylogeny)
#'
#' \donttest{
#' # solve problems
#' s <- stack(solve(p1), solve(p2), solve(p3), solve(p4))
#'
#' # plot solutions
#' plot(s, main=c("minimum set", "maximum coverage", "maximum representation",
#'                "phylogenetic representation"))
#' }
#'
#' @name add_max_cover_objective
NULL

#' @rdname add_max_cover_objective
#' @export
add_max_cover_objective <- function(x, budget) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(budget))), assertthat::is.scalar(budget),
                          isTRUE(budget > 0.0))
  # make parameter
  p <- numeric_parameter("budget", budget, lower_limit = 0,
                         upper_limit = sum(x$planning_unit_costs()))
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
                                               unname(y$feature_abundances_in_planning_units()),
                                               y$planning_unit_costs(), self$parameters$get("budget")))
    }))
}
