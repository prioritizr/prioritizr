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
#'
#' @export
Solver <- R6::R6Class(
  "Solver",
  inherit = ConservationModifier,
  public = list(

    #' @description
    #' Run the solver to generate a solution.
    #' @return `list` of solutions.
    run = function() {
      # nocov start
      cli::cli_abort("No defined $run method.", .internal = TRUE)
      # nocov end
    },

    #' @description
    #' Perform computations that need to be completed before applying
    #' the object.
    #' @param x [optimization_problem()] object.
    #' @param ... Additional arguments.
    #' @return Invisible `TRUE`.
    calculate = function(...) {
      # nocov start
      cli::cli_abort("No defined $calculate method.", .internal = TRUE)
      # nocov end
    },

    #' @description
    #' Set the upper bound for a decision variable.
    #' @param index `integer` value indicating the index of the decision
    #' variable.
    #' @param value `numeric` new bound value.
    #' @details Note that this method should only be run after `$calculate()`.
    #' It can be used to overwrite values after ingesting an
    #' [optimization_problem()] object.
    #' It is designed to be used in [portfolios] and [importance] functions.
    #' @return Invisible `TRUE`.
    set_variable_ub = function(index, value) {
      # nocov start
      cli::cli_abort("No defined $set_variable_ub method.", .internal = TRUE)
      # nocov end
    },

    #' @description
    #' Set the lower bound for a decision variable.
    #' @param index `integer` value indicating the index of the decision
    #' variable.
    #' @param value `numeric` new bound value.
    #' @details Note that this method should only be run after `$calculate()`.
    #' It can be used to overwrite values after ingesting an
    #' [optimization_problem()] object.
    #' It is designed to be used in [portfolios] and [importance] functions.
    #' @return Invisible `TRUE`.
    set_variable_lb = function(index, value) {
      # nocov start
      cli::cli_abort("No defined $set_variable_lb method.", .internal = TRUE)
      # nocov end
    },

    #' @description
    #' Set the right-hand-side coefficient bound for a constraint.
    #' @param index `integer` value indicating the index of the decision
    #' variable.
    #' @param value `numeric` new value.
    #' @details Note that this method should only be run after `$calculate()`.
    #' It can be used to overwrite values after ingesting an
    #' [optimization_problem()] object.
    #' It is designed to be used in [portfolios] and [importance] functions.
    #' @return Invisible `TRUE`.
    set_constraint_rhs = function(index, value) {
      # nocov start
      cli::cli_abort("No defined $set_constraint_rhs method.", .internal = TRUE)
      # nocov end
    },

    #' @description
    #' Set the starting solution.
    #' @param value `numeric` new value.
    #' @details Note that this method should only be run after `$calculate()`.
    #' It can be used to overwrite values after ingesting an
    #' [optimization_problem()] object.
    #' It is designed to be used in [portfolios] and [importance] functions.
    #' @return Invisible `TRUE`.
    set_start_solution = function(value) {
      # nocov start
      cli::cli_abort("No defined $set_start_solution method.", .internal = TRUE)
      # nocov end
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
