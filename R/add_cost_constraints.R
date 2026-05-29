#' @include internal.R
NULL

#' Add cost constraints
#'
#' Add constraints to a conservation planning problem to ensure
#' that the cost of selected planning units meets certain criteria.
#'
#' @inheritParams add_max_cover_objective
#'
#' @param sense `character` value specifying the constraint sense.
#' Acceptable values are: `">="`, `"<="`, or `"="`. If `x` has multiple zones,
#' then `sense` can be
#' (i) a single `character` value to specify a constraint sense
#' for the entire solution or (ii) a `character` vector to specify
#' a different constraint sense for each zone (separately) in the solution.
#' Note that `sense` and `budget` must have the same number of values.
#'
#' @details
#' This function adds constraints constraints that can be used to
#' ensure that the cost of solutions meet certain criteria
#' (see Examples section below for details).
#' For example, these constraints can be used to specify
#' both minimum and maximum budget thresholds.
#' Note that this function provided as a convenient alternative for
#' adding linear constraints (per [add_linear_constraints()]) to a [problem()].
#'
#' @section Mathematical formulation:
#' The cost constraints are implemented using the following
#' equation.
#' Let \eqn{I} denote the set of planning units
#' (indexed by \eqn{i}), \eqn{Z} the set of management zones (indexed by
#' \eqn{z}), and \eqn{X_{iz}}{Xiz} the decision variable for allocating
#' planning unit \eqn{i} to zone \eqn{z} (e.g., with binary
#' values indicating if each planning unit is allocated or not). Also, let
#' \eqn{D_{iz}}{Diz} denote the costs associated with
#' planning units \eqn{i \in I}{i in I} for zones \eqn{z \in Z}{z in Z}
#' (per `data`, if supplied as a `matrix` object),
#' \eqn{\theta} denote the constraint sense
#' (per `sense`), and \eqn{B}{B} denote the budget
#' threshold (per `budget`).
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{z}^{Z} (D_{iz} \times X_{iz}) \space \theta \space t
#' }{
#' sum_i^I sum (Diz * Xiz) \theta B
#' }
#'
#' @inherit add_manual_locked_constraints return seealso
#'
#' @family constraints
#'
#' @inherit add_cost_penalties examples
#'
#' @export
add_cost_constraints <- function(x, budget, sense) {
  # assert valid arguments
  assert(
    is_conservation_problem(x),
    is.numeric(budget),
    assertthat::noNA(budget),
    is.character(sense),
    assertthat::noNA(sense),
    all_match_of(sense, c("<=", "=", ">=")),
    is_match_of(length(budget), c(1, number_of_zones(x))),
    is_match_of(length(sense), c(1, number_of_zones(x))),
    length(budget) == length(sense)
  )
  # add penalties
  x$add_constraint(
    R6::R6Class(
      "CostConstraint",
      inherit = Constraint,
      public = list(
        name = "cost constraints",
        data = list(budget = budget, sense = sense),
        apply = function(x, y) {
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # prepare cost data by removing NA values
          d <- as_Matrix(y$planning_unit_costs(), "dgCMatrix")
          d@x[!is.finite(d@x)] <- 0
          # apply constraints
          rcpp_apply_cost_constraints(
            x$ptr,
            self$get_data("budget"),
            self$get_data("sense"),
            d
          )
          # return success
          invisible(TRUE)
        }
      )
    )$new()
  )
}
