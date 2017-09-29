#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add Minimum Set Objective
#'
#' Set an objective to find the solution that fulfills all the targets and
#' constraints for the smallest cost. This objective is similar to that used in
#' Marxan.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @details
#' A problem objective is used to specify the overall goal of the
#' conservation planning problem. Please note that \strong{all conservation
#' planning problems formulated in the prioritizr package require the addition
#' of both objectives and targets}. Failing to do so will return a default
#' error message when solving.
#'
#' In the context of systematic reserve design, the minimum set objective
#' seeks to find the set of planning units that minimizes the overall cost of
#' a reserve network, while meeting a set of representation targets for the
#' conservation features. This objective is equivalent to a simplified Marxan
#' reserve design problem, with the Boundary Length Modifier (BLM) set to zero.
#'
#' The reserve design problem for the minimum set objective can be expressed
#' mathematically for \emph{n} planning units as:
#'
#' \deqn{\mathit{Minimize} \space \sum_{i=1}^{n}x_i c_i \space
#' \mathit{subject \space to} \space
#' \sum_{j=1}^{n}x_j r_{ij}\geq T_i \space \forall \space i}{Minimize
#' \sum^n(xi*ci) subject to \sum^n(j*rij) \ge Ti for all i}
#'
#' where \eqn{x_i}{xi} is a binary decision variable specifying whether planning
#' unit \eqn{i}{i} has been selected (1) or not (0), \eqn{c_i}{ci} is the cost
#' of planning unit \eqn{i}{i}, \eqn{r_ij}{rij} is the representation level of
#' feature \eqn{i}{i} in planning unit \emph{j}{j}, and \eqn{T_i}{Ti} is the
#' target for feature \eqn{i}{i}. The first term is the objective function and
#' the second is the set of constraints. In words this says find the set of
#' planning units that meets #' all the representation targets while minimizing
#' the overall cost.
#'
#' @seealso \code{\link{objectives}}, \code{\link{constraints}},
#'   \code{\link{problem}}, \code{\link{targets}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_min_set_objective() %>%
#'      add_relative_targets(0.1) %>%
#'      add_binary_decisions()
#'
#' \donttest{
#' # solve problem
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution")
#' }
#' @name add_min_set_objective
NULL

#' @rdname add_min_set_objective
#' @export
add_min_set_objective <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # add objective to problem
  x$add_objective(pproto(
    "MinimumSetObjective",
    Objective,
    name = "Minimum set objective",
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_min_set_objective(x$ptr, y$feature_targets(),
                                             y$planning_unit_costs()))
    }))
}
