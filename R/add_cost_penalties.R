#' @include internal.R Penalty-class.R
NULL

#' Add cost penalties
#'
#' Add penalties to a conservation planning problem to penalize
#' solutions that select planning units with higher cost values.
#' These penalties assume a linear trade-off between the cost and the primary
#' objective of the conservation planning problem (e.g.,
#' number of targets met for [add_max_n_targets_met_objective()].
#'
#' @inheritParams add_boundary_penalties
#'
#' @param penalty `numeric` value denoting the importance of not selecting
#' planning units with high cost values.
#' Higher `penalty` values can be used to obtain solutions that
#' are strongly averse to selecting places with high cost
#' values, and smaller `penalty` values can be used to obtain solutions
#' that only avoid places with especially high cost.
#' Note that negative
#' `penalty` values can be used to obtain solutions that prefer places
#' with high cost values. Additionally, if has `x` has multiple zones,
#' then `penalty` must have a value for each zone.
#'
#' @details
#' This function penalizes solutions that have higher values according
#' to the sum of the cost values associated with each planning unit,
#' weighted by status of each planning unit in the solution.
#' Note that this function provided as a convenient alternative for
#' adding linear penalties (per [add_linear_penalties()]) to a [problem()].
#'
#' @section Mathematical formulation:
#' The cost penalties are implemented using the following
#' equations.
#' Let \eqn{I} denote the set of planning units
#' (indexed by \eqn{i}), \eqn{Z} the set of management zones (indexed by
#' \eqn{z}), and \eqn{X_{iz}}{Xiz} the decision variable for allocating
#' planning unit \eqn{i} to zone \eqn{z} (e.g., with binary
#' values indicating if each planning unit is allocated or not). Also, let
#' \eqn{P_z} represent the penalty scaling value for zones
#' \eqn{z \in Z}{z in Z} (per `penalty`), and
#' \eqn{D_{iz}}{Diz} represent the cost data for allocating planning unit
#' \eqn{i \in I}{i in I} to zones \eqn{z \in Z}{z in Z}
#' (per `data` in matrix format).
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{z}^{Z} P_z \times D_{iz} \times X_{iz}
#' }{
#' sum_i^I sum_z^Z (Pz * Diz * Xiz)
#' }
#'
#' Note that when the problem objective is to maximize some measure of
#' benefit and not minimize some measure of cost, the term \eqn{P_z} is
#' replaced with \eqn{-P_z}.
#'
#' @inherit add_linear_penalties return seealso
#'
#' @family penalties
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' sim_complex_pu_raster <- get_sim_complex_pu_raster()
#' sim_complex_features <- get_sim_complex_features()
#'
#' # create layer with 1s for all planning units
#' sim_ones_complex_raster <- (sim_complex_pu_raster * 0) + 1
#'
#' # here we will formulate a multi-objective optimization problem
#' # that (i) minimizes the largest target shortfall for feature representation,
#' # (ii) minimizes the overall target shortfalls for feature representation,
#' # and (iii) minimizes the cost of the solution. since the
#' # first objective is to minimize the largest shortfall and the second
#' # objective is to minimize overall target shortfalls,
#' # this helps balance shortfalls among all features and better
#' # achieve complementarity. additionally, we will specify that
#' # (approximately) 30% of the study area should be selected (i.e., by
#' # specifying a budget for the upper threshold and a linear constraint for
#' # the lower threshold on the number of selected planning units).
#'
#' # calculate budget based on 30% of the number of planning units
#' budget <-
#'   0.3 * terra::global(sim_ones_complex_raster, "sum", na.rm = TRUE)[[1]]
#'
#' # build multi-objective conservation planning problem
#' mp <-
#'   multi_problem(
#'     obj1 =
#'       problem(sim_ones_complex_raster, sim_complex_features) %>%
#'       add_min_largest_shortfall_objective(budget = budget) %>%
#'       add_auto_targets("jung") %>%
#'       # note that this constraint only needs to be specified once
#'       add_cost_constraints(sense = ">=", budget = budget * 0.9) %>%
#'       add_binary_decisions(),
#'     obj2 =
#'       problem(sim_ones_complex_raster, sim_complex_features) %>%
#'       add_min_shortfall_objective(budget = budget) %>%
#'       # note that we use the same targets for both obj1 and obj2
#'       add_auto_targets("jung") %>%
#'       add_binary_decisions(),
#'     obj3 =
#'       problem(sim_complex_pu_raster, sim_complex_pu_raster) %>%
#'       add_min_penalties_objective() %>%
#'       # note a value of 1 is here because only the costs minimized
#'       add_cost_penalties(1) %>%
#'       add_binary_decisions()
#'   ) %>%
#'   add_default_solver(gap = 0.01, verbose = FALSE)
#'
#' # to explore trade-offs between how well the feature targets
#' # are met and cost, we will generate a matrix of relative tolerance values
#' # for the hierarchical approach. note that the first column of this
#' # matrix will have only zeros to help promote balanced
#' # shortfalls across different features, and the second column
#' # will have non-zeros because we are interested in trade-offs between
#' # overall feature shortfalls and cost
#' rel_tol_matrix <- matrix(0, ncol = 2, nrow = 10)
#' rel_tol_matrix[, 2] <- seq(0, 0.5, length.out = nrow(rel_tol_matrix))
#'
#' # display matrix
#' print(rel_tol_matrix)
#'
#' # add hierarchical approach to multi-objective problem
#' mp <-
#'   mp %>%
#'   add_hier_approach(rel_tol = rel_tol_matrix)
#'
#' # generate solutions
#' ms <- solve(mp)
#'
#' # plot the solutions
#' plot(terra::rast(ms), axes = FALSE)
#'
#' # extract objective values for the solutions
#' obj_matrix <- attributes(ms)$objective
#'
#' # preview the objective values
#' head(obj_matrix)
#'
#' # plot the objectives values to visualize trade-offs
#' # (note that smaller values are better for both objectives)
#' plot(
#'   obj_matrix[, 2:3],
#'   main = "Trade-offs between objectives",
#'   xlab = "Species representation (overall shortfall)",
#'   ylab = "Solution cost"
#' )
#' }
#' @export
add_cost_penalties <- function(x, penalty) {
  # assert valid arguments
  assert(
    is_conservation_problem(x),
    is.numeric(penalty),
    assertthat::noNA(penalty),
    number_of_zones(x) == length(penalty)
  )
  # add penalties
  x$add_penalty(
    R6::R6Class(
      "CostPenalty",
      inherit = Penalty,
      public = list(
        name = "cost penalties",
        data = list(penalty = penalty),
        apply = function(x, y) {
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # extract parameters
          p <- self$get_data("penalty")
          if (min(abs(p)) > 1e-50) {
            # apply penalties
            rcpp_apply_linear_penalties(
              x$ptr, p,
              as_Matrix(y$planning_unit_costs(), "dgCMatrix")
            )
          }
          invisible(TRUE)
        }
      )
    )$new()
  )
}
