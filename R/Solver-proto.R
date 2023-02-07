#' @include internal.R pproto.R
NULL

#' @export
if (!methods::isClass("Solver")) methods::setOldClass("Solver")
NULL

#' Solver prototype
#'
#' This prototype is used to generate objects that represent methods for solving
#' optimization problems. **This class represents a recipe to
#' create solver and and is only recommended for use by expert users. To
#' customize the method used to solve optimization problems, please see the
#' help page on [solvers]**.
#'
#' @section Fields:
#'
#' \describe{
#'
#' \item{$name}{`character` name of solver.}
#'
#' \item{$data}{`list` of data.}
#'
#' \item{$internal}{`list` of internal data.}
#'
#' }
#'
#' @section Usage:
#'
#' `x$print()`
#'
#' `x$show()`
#'
#' `x$repr()`
#'
#' `x$get_data(name)`
#'
#' `x$set_data(name, value)`
#'
#' `x$get_internal(name)`
#'
#' `x$set_internal(name, value)`
#'
#' `x$set_variable_ub(index, value)`
#'
#' `x$set_variable_lb(index, value)`
#'
#' `x$calculate(op)`
#'
#' `x$run()`
#'
#' `x$solve(op)`
#'
#' @section Arguments:
#' \describe{
#'
#' \item{x}{[`Solver-class`] object.}
#'
#' \item{op}{[`OptimizationProblem-class`] object.}
#'
#' }
#'
#' @section Details:
#'
#' \describe{
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{repr}{`character` representation of object.}
#'
#' \item{get_data}{return an object stored in the `data` field with
#'   the corresponding `name`. If the object is not present in the
#'   `data` field, a `waiver` object is returned.}
#'
#' \item{set_data}{store an object stored in the `data` field with
#'   the corresponding name. If an object with that name already
#'   exists then the object is overwritten.}
#'
#' \item{get_internal}{return an object stored in the `internal` field with
#'   the corresponding `name`. If the object is not present in the
#'   `internal` field, a `waiver` object is returned.}
#'
#' \item{set_internal}{store an object stored in the `internal` field with
#'   the corresponding name. If an object with that name already
#'   exists then the object is overwritten.}
#'
#' \item{set_variable_ub}{set the upper bounds on decision variables in
#'   a pre-calculated optimization problem stored in the solver.}
#'
#' \item{set_variable_lb}{set the lower bounds on decision variables in
#'   a pre-calculated optimization problem stored in the solver.}
#
#' \item{calculate}{ingest a general purpose
#'   [`OptimizationProblem-class`] object and convert it to the
#'   correct format for the solver.}

#' \item{run}{run the solver and output a solution}
#'
#' \item{solve}{solve an [`OptimizationProblem-class`] using this
#'   object.}
#'
#' }
#'
#' @name Solver-class
#'
#' @aliases Solver
NULL

#' @export
Solver <- pproto(
  "Solver",
  name = character(0),
  data = list(),
  internal = list(),
  print = function(self) {
    cli::cli_text(self$repr())
  },
  show = function(self) {
    self$print()
  },
  repr = function(self, compact = TRUE) {
    repr_data_list(self$name, self$data, compact = compact)
  },
  calculate = function(...) {
    cli::cli_abort("No defined $calculate method.", .internal = TRUE)
  },
  run = function(...) {
    cli::cli_abort("No defined $run method.", .internal = TRUE)
  },
  solve = function(self, x, ...) {
    # build optimization problem
    self$calculate(x, ...)
    # run solver and get solution
    out <- self$run()
    # clear internal store to reduce memory consumption
    self$internal <- list()
    # return output
    out
  },
  get_data = function(self, x) {
    self$data[[x]] %||% new_waiver()
  },
  set_data = function(self, x, value) {
    self$data[[x]] <- value
    invisible()
  },
  get_internal = function(self, x) {
    self$internal[[x]] %||% new_waiver()
  },
  set_internal = function(self, x, value) {
    self$internal[[x]] <- value
    invisible()
  }
)
