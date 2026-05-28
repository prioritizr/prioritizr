#' @include internal.R
NULL

#' Add cost constraints
#'
#' Add constraints to a conservation planning problem to ensure
#' that the cost of selected planning units meets certain criteria.
#'
#' @inheritParams add_linear_constraints
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
#' The linear constraints are implemented using the following
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
#' (per `sense`), and \eqn{t} denote the constraint
#' threshold (per `threshold`).
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{z}^{Z} (D_{iz} \times X_{iz}) \space \theta \space t
#' }{
#' sum_i^I sum (Diz * Xiz) \theta t
#' }
#'
#' @inherit add_manual_locked_constraints return seealso
#'
#' @family constraints
#'
#' @inherit add_cost_penalties examples
#'
#' @export
add_cost_constraints <- function(x, threshold, sense) {
  # assert valid arguments
  assert(
    is_conservation_problem(x),
    assertthat::is.number(threshold),
    assertthat::noNA(threshold),
    assertthat::is.string(sense),
    assertthat::noNA(sense),
    is_match_of(sense, c("<=", "=", ">="))
  )
  # add penalties
  x$add_constraint(
    R6::R6Class(
      "CostConstraint",
      inherit = Constraint,
      public = list(
        name = "cost constraints",
        data = list(threshold = threshold, sense = sense),
        apply = function(x, y) {
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # apply constraints
          rcpp_apply_linear_constraints(
            x$ptr,
            self$get_data("threshold"),
            self$get_data("sense"),
            as_Matrix(y$planning_unit_costs(), "dgCMatrix")
          )
          # return success
          invisible(TRUE)
        }
      )
    )$new()
  )
}
