#' @include internal.R Objective-class.R
NULL

#' Add minimum penalties objective
#'
#' Set the objective of a conservation planning problem to
#' minimize the penalties added to the problem.
#' Targets can optionally be specified to ensure that the solution
#' must meet all the [targets].
#' Budgets can also optionally be specified to ensure that the solution
#' does not exceed a budgetary threshold.
#' This objective is designed to be used with multi-objective optimization.
#'
#' @inheritParams add_max_cover_objective
#'
#' @param budget `numeric` value specifying the maximum expenditure of
#'   the prioritization. For problems with multiple zones, the argument
#'   to `budget` can be a single `numeric` value to specify a budget
#'   for the entire solution or a `numeric` vector to specify
#'   a budget for each each management zone. Defaults to `NULL`
#'   such that no maximum expenditure is specified.
#'
#' @details
#' The minimum penalty objective is designed to be used with problems
#' that have penalties (see [penalties] for details). It can be used
#' to generate solutions that focus entirely on minimizing the penalties,
#' whilst (optionally) ensuring that certain constraints are met.
#' This is is useful when performing multi-objective optimization
#' (see examples below).
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
#' \eqn{i}{i}, and \eqn{T_j}{Tj} is the target for feature \eqn{j}{j}. Since
#' the objective is to minimize zero, this function does not actually provide
#' any criteria to compare competing solutions. As such, when used in
#' conjunction with a penalty function (see [penalties]), only the penalty
#' (e.g., [add_boundary_penalties()]) will be used to compare competing
#' solutions during optimization.
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
#' # here we will show how the min penalties objective can be used
#' # to generate a solution that accounts for spatial fragmentation
#' # (via boundary penalties) using multi-objective optimization techniques
#'
#' # create initial problem
#' # note that this does not consider boundary penalties
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.3) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "initial solution", axes = FALSE)
#'
#' # create a multi-objective problem that contains the
#' # initial problem as well as an additional problem that is
#' # focused entirely on minimizing spatial fragmentation.
#' # additionally, this multi-objective problem will use the
#' # hierarchical approach for optimization and we will
#' # consider three rel_tol values to generate multiple solutions
#' # that represent different levels of trade-off between total cost
#' # and spatial fragmentation. note that we use a small penalty value
#' # in add_boundary_penalties() to avoid scaling issues and this
#' # has no influence on the trade-offs between cost and spatial fragmentation.
#' rel_tol <- c(0, 0.05, 0.1, 0.2)
#' mp <-
#'   multi_problem(
#'     obj1 = p1,
#'     obj2 =
#'       problem(sim_pu_raster, sim_features) %>%
#'      add_min_penalties_objective() %>%
#'      add_boundary_penalties(penalty = 0.1) %>%
#'      add_binary_decisions()
#'   ) %>%
#'   add_hier_approach(rel_tol = matrix(rel_tol, ncol = 1)) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # generate multi-objective solutions
#' s2 <- solve(mp)
#'
#' # plot multi-objective solutions
#' plot(terra::rast(s2), main = paste("rel_tol =", rel_tol), axes = FALSE)
#' }
#' @name add_min_penalties_objective
NULL

#' @rdname add_min_penalties_objective
#' @export
add_min_penalties_objective <- function(x, budget = NULL) {
  # assert argument is valid
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
  } else {
    budget <- NA_real_
  }
  # add objective to problem
  x$add_objective(
    R6::R6Class(
      "MinimumPenaltiesObjective",
      inherit = Objective,
      public = list(
        name = "minimum penalties objective",
        has_weights = FALSE,
        has_targets = NA,
        data = list(budget = budget),
        apply = function(x, y, weights) {
          # note that weights are not used
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # prepare targets
          if (is.Waiver(y$targets)) {
            targ <- tibble::tibble(
              feature = integer(0),
              zone = list(),
              sense = character(0),
              value = numeric(0)
            )
          } else {
            targ <- y$feature_targets()
          }
          invisible(
            rcpp_apply_min_penalties_objective(
              x$ptr,
              targ,
              y$planning_unit_costs(),
              self$get_data("budget")
            )
          )
        }
      )
    )$new()
  )
}
