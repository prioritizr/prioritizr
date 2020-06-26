#' @include internal.R pproto.R parameters.R
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
#' \item{$data}{`list` object optimization problem data.}
#'
#' \item{$parameters}{`Parameters` object with parameters used to customize
#'   the the solver.}
#'
#' \item{$solve}{`function` used to solve a
#'   [OptimizationProblem-class] object.}
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
#' \item{x}{[Solver-class] object.}
#'
#' \item{op}{[OptimizationProblem-class] object.}
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
#' \item{set_variable_ub}{set the upper bounds on decision variables in
#'   a pre-calculated optimization problem stored in the solver.}
#'
#' \item{set_variable_lb}{set the lower bounds on decision variables in
#'   a pre-calculated optimization problem stored in the solver.}
#
#' \item{calculate}{ingest a general purpose
#'   [OptimizationProblem-class] object and convert it to the
#'   correct format for the solver.}

#' \item{run}{run the solver and output a solution}
#'
#' \item{solve}{solve an [OptimizationProblem-class] using this
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
  calculate = function(...) stop("solver is missing a calculate method"),
  run = function(...) stop("solver is missing a run method"),
  solve = function(self, x, ...) {
    # build optimization problem
    self$calculate(x, ...)
    # run solver and get solution
    out <- self$run()
    # clear data store to reduce memory consumption
    self$data <- list()
    # return output
    out
  },
  parameters = parameters(),
  print = function(self) {
    message(self$repr())
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    paste(self$name, self$parameters$repr())
  },
  get_parameter = function(self, x) {
    self$parameters$get(x)
  },
  set_parameter = function(self, x, value) {
    self$parameters$set(x, value)
  },
  get_data = function(self, x) {
    if (!x %in% names(self$data))
      return(new_waiver())
    return(self$data[[x]])
  },
  set_data = function(self, x, value) {
    self$data[[x]] <- value
    invisible()
  },
  render_parameter = function(self, x) {
    self$parameters$render(x)
  },
  render_all_parameters = function(self) {
    shiny::div(class = "Solver", self$parameters$render_all())
  })
