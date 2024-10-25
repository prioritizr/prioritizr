#' @include internal.R Objective-class.R
NULL

#' Add minimum penalties objective
#'
#' Set the objective of a conservation planning problem to
#' only minimize the penalties added to the problem, whilst ensuring that all
#' [targets] are met and the cost of the solution does not exceed a budget.
#' This objective is useful when using a hierarchical approach for
#' multi-objective optimization.
#'
#' @inheritParams add_min_shortfall_objective
#'
#' @details
#' The minimum penalty objective is designed to be used with problems
#' that have penalties (see [penalties] for details). It can be used
#' to generate solutions that focus entirely on minimizing the penalties,
#' whilst ensuring that certain constraints are met.
#' This is is useful when performing multi-objective optimization using a
#' hierarchical approach (see examples below). Although previous versions of the
#' package recommended using the minimum set objective (i.e.,
#' [add_min_set_objective()]) with zero costs and linear constraints for this
#' purpose, the minimum penalty objective provides a dedicated objective
#' for performing hierarchical multi-objective optimization.
#'
#' @section Mathematical formulation:
#' This objective can be expressed
#' mathematically for a set of planning units (\eqn{I}{I} indexed by
#' \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Minimize} \space 0 \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i r_{ij} \geq T_j \space \forall \space j \in J \\
#' \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Minimize sum_i^I (xi * ci) subject to
#' sum_i^I (xi * rij) >= Tj for all j in J &
#' sum_i^I (xi * ci) <= B}
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.,
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{c_i}{ci} is the cost of planning unit \eqn{i}{i},
#' \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning unit
#' \eqn{i}{i}, and \eqn{T_j}{Tj} is the target for feature \eqn{j}{j}.
#'
#' @family objectives
#'
#' @inherit add_min_shortfall_objective return
#'
#' @seealso
#' See [objectives] for an overview of all functions for adding objectives.
#' Also see [targets] for an overview of all functions for adding targets.
#' Additionally, see [penalties] for an overview of all functions for adding
#' penalties.
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create initial problem with minimum set objective
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve initial problem
#' s1 <- solve(p1)
#'
#' # plot initial  solution
#' plot(s1, main = "initial solution", axes = FALSE)
#'
#' # calculate total cost of initial solution
#' c1 <- eval_cost_summary(s1)
#'
#' # since the solution is spatially fragmented, we will now use
#' # a hierarchical multi-objective optimization approach to reduce
#' # spatial fragmentation
#'
#' # calculate budget for new prioritization based on cost of initial solution,
#' # this budget will specify that we are willing to accept a 10%
#' # increase in the total cost of the solution to minimize fragmentation
#' b <- c1$cost[[1]] * 1.1
#'
#' # create problem with minimum penalty objective using the budget and
#' # boundary penalties to reduce spatial fragmentation
#' p2 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_penalties_objective(budget = b) %>%
#'   add_boundary_penalties() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve initial problem
#' ss <- solve(p2)
#'
#' # plot initial  solution
#' plot(s2, main = "solution", axes = FALSE)
#' }
#' @name add_min_penalties_objective
NULL

#' @rdname add_min_penalties_objective
#' @export
add_min_penalties_objective <- function(x, budget) {
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
      "MinimumPenaltiesObjective",
      inherit = Objective,
      public = list(
        name = "minimum penalties objective",
        data = list(budget = budget),
        apply = function(x, y) {
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          invisible(
            rcpp_apply_min_penalties_objective(
              x$ptr,
              y$feature_targets(),
              y$planning_unit_costs(),
              self$get_data("budget")
            )
          )
        }
      )
    )$new()
  )
}
