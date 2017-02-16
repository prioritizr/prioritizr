#' @include internal.R pproto.R parameters.R 
NULL

#' @export
methods::setOldClass('Solver')

#' Solver prototype
#'
#' This prototype is used to generate objects that represent methods for solving
#' optimization problems. \strong{This class represents a recipe to 
#' create solver and and is only recomended for use by expert users. To 
#' customize the method used to solve optimization problems, please see the
#' help page on \code{\link{solvers}}}.
#'
#' @section Fields:
#' \itemize{
#' \item{$name}{\code{character} name of solver.}
#' \item{$parameters}{\code{Parameters} object with parameters used to customize
#'   the the solver.}
#' \item{$solve}{\code{function} used to solve a
#'   \code{\link{OptimizationProblem}} object.}
#' }
#'
#' @section Usage:
#' \code{x$print()}
#' \code{x$show()}
#' \code{x$repr()}
#' \code{x$solve(op)}
#' 
#' @section Arguments:
#' \describe{
#' \item{x}{\code{\link{Solver}} object.}
#' \item{op}{\code{\link{OptimizationProblem}} object.}
#' }
#'
#' @section Details:
#' \describe{
#' \item{print}{print the object.}
#' \item{show}{show the object.}
#' \item{repr}{\code{character} representation of object.}
#' \item{solve}{solve a \link{OptimizationProblem} using this object.}
#' }
#'
#' @name Solver
NULL

#' @export
Solver <- pproto(
  'Solver',
  name = character(0),
  solve = function(...) {stop('solver is missing a solve method')},
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
  get_paramter = function(self, x) {
    self$parameters$get(x)
  },
  set_parameter = function(self, value) {
    self$parameters$set(x, value)
  },
  render_parameter = function(self, x) {
    self$parameters$render(x)
  },
  render_all_parameters = function(self) {
    shiny::div(class='Solver',
                self$parameters$render_all())
  })
