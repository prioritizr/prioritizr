#' @include internal.R Parameters-class.R
NULL

#' @export
methods::setOldClass('Solver')

#' Solver class
#'
#' This class is used to generate objects that represent methods for solving
#' optimization problems. \strong{This class represents a recipe to 
#' create solver and and is only recomended for use by expert users. To 
#' customize the method used to solve optimization problems, please see the
#' help page on \code{\link{solvers}}}.
#'
#' @section Fields:
#' \itemize{
#' \item{$name}{\code{character} name of solver.}
#' \item{$f}{\code{function} used to solve a \code{\link{OptimizationProblem}}
#'    object.}
#' \item{$parameters}{\code{Parameters} object with parameters used to customize the
#'    the solver.}
#' }
#'
#' @section Usage:
#' \preformatted{x <- Solver$new(name, f, parameters)}
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
Solver <- R6::R6Class('Solver',
  public=list(
    name = NULL,
    f = NULL,
    parameters = NULL,
    initialize = function(name, f, parameters) {
      assertthat::assert_that(is.string(name),
                              inherits(f, 'function'),
                              inherits(parameters, 'Parameters'))
      self$name <- name
      self$f <- f
      self$parameters <- parameters
    }, 
    print = function() {
  message(
    'Solver object',
'\n  name:     ',x$name, 
'\n  options:  ', repr(x$parameters))
    }, 
    show = function() {
      self$print()
    },
    repr = function() {
      paste0(self$name, ' [', repr(x$parameters), ']')
    },
    solve = function(x) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'))
      self$f(x)
    },
    get_paramter = function(x) {
      self$parameters$get(x)
    },
    set_parameter = function(x, value) {
      self$parameters$set(x, value)
    },
    render_parameter = function(x) {
      self$parameters$render(x)
    },
    render_all_parameters = function() {
      shiny::div(class='Solver',
                 self$parameters$render_all())
    }
  )
)
