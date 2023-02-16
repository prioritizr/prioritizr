#' @include internal.R
NULL

#' @export
if (!methods::isClass("Solver")) methods::setOldClass("Solver")
NULL

#' Solver class
#'
#' @description
#' This class is used to represent solvers for optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Solver-class
#'
#' @family classes
Solver <- R6::R6Class(
  "Solver",
  inherit = ConservationModifier,
  public = list(

    #' @description
    #' Run the solver to generate a solution.
    #' @return `list` of solutions.
    run = function() {
      cli::cli_abort("No defined $run method.", .internal = TRUE)
    },

    #' @description
    #' Perform computations that need to be completed before applying
    #' the object.
    #' @param x [optimization_problem()] object.
    #' @param ... Additional arguments.
    #' @return Invisible `TRUE`.
    calculate = function(...) {
      cli::cli_abort("No defined $calculate method.", .internal = TRUE)
    },

    #' @description
    #' Set the upper bound for a decision variable.
    #' @param index `integer` value indicating the index of the decision
    #' variable.
    #' @param value `numeric` new bound value.
    #' @return Invisible `TRUE`.
    set_variable_ub = function(index, value) {
      cli::cli_abort("No defined $set_variable_ub method.", .internal = TRUE)
    },

    #' @description
    #' Set the lower bound for a decision variable.
    #' @param index `integer` value indicating the index of the decision
    #' variable.
    #' @param value `numeric` new bound value.
    #' @return Invisible `TRUE`.
    set_variable_lb = function(index, value) {
      cli::cli_abort("No defined $set_variable_lb method.", .internal = TRUE)
    },

    #' @description
    #' Solve an optimization problem.
    #' @param x [optimization_problem()] object.
    #' @param ... Additional arguments passed to the `calculate()` method.
    #' @return Invisible `TRUE`.
    solve = function(x, ...) {
      # build optimization problem
      self$calculate(x, ...)
      # run solver and get solution
      out <- self$run()
      # clear internal store to reduce memory consumption
      self$internal <- list()
      # return output
      out
    }
  )
)
