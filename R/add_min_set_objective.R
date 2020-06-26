#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add minimum set objective
#'
#' Set the objective of a conservation planning [problem()] to
#' minimize the cost of the solution whilst ensuring that all targets are met.
#' This objective is similar to that used in
#' *Marxan* and is detailed in Rodrigues *et al.* (2000).
#'
#' @param x [ConservationProblem-class] object.
#'
#' @details A problem objective is used to specify the overall goal of the
#'   conservation planning problem. Please note that all conservation
#'   planning problems formulated in the \pkg{prioritizr} package require the
#'   addition of objectives---failing to do so will return an error
#'   message when attempting to solve problem.
#'
#'   In the context of systematic reserve design, the minimum set objective
#'   seeks to find the set of planning units that minimizes the overall cost of
#'   a reserve network, while meeting a set of representation targets for the
#'   conservation features. This objective is equivalent to a simplified
#'   *Marxan* reserve design problem with the Boundary Length Modifier
#'   (BLM) set to zero.
#'
#'   The minimum set objective for the reserve design problem can be expressed
#'   mathematically for a set of planning units (\eqn{I}{I} indexed by
#'   \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed by \eqn{j}{j}) as:
#'
#'   \deqn{\mathit{Minimize} \space \sum_{i = 1}^{I} x_i c_i \\
#'   \mathit{subject \space to} \\
#'   \sum_{i = 1}^{I} x_i r_{ij} \geq T_j \space \forall \space j \in J}{
#'   Minimize sum_i^I (xi * ci) subject to sum_i^I (xi * rij) >= Tj for all
#'   j in J}
#'
#'   Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.
#'   specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#'   (0)), \eqn{c_i}{ci} is the cost of planning unit \eqn{i}{i},
#'   \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning unit
#'   \eqn{i}{i}, and \eqn{T_j}{Tj} is the target for feature \eqn{j}{j}. The
#'   first term is the objective function and the second is the set of
#'   constraints. In words this says find the set of planning units that meets
#'   all the representation targets while minimizing the overall cost.
#'
#' @references
#' Rodrigues AS, Cerdeira OJ, and Gaston KJ (2000) Flexibility,
#' efficiency, and accountability: adapting reserve selection algorithms to
#' more complex conservation problems. *Ecography*, 23: 565--574.
#'
#' @seealso [objectives], [targets].
#'
#' @return [ConservationProblem-class] object with the objective
#'   added to it.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#'
#' # create minimal problem with minimum set objective
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'      add_min_set_objective() %>%
#'      add_relative_targets(0.1) %>%
#'      add_binary_decisions()
#' \donttest{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#' }
#'
#' # create multi-zone problem with minimum set objective
#' targets_matrix <- matrix(rpois(15, 1), nrow = 5, ncol = 3)
#'
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(targets_matrix) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(category_layer(s2), main = "solution", axes = FALSE, box = FALSE)
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
