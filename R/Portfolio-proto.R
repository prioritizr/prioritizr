#' @include internal.R pproto.R parameters.R
NULL

#' @export
if (!methods::isClass("Portfolio")) methods::setOldClass("Portfolio")
NULL

#' Portfolio prototype
#'
#' This prototype is used to represent methods for generating portfolios of
#' optimization problems. \strong{This class represents a recipe to
#' create portfolio generating method and is only recommended for use by expert
#' users. To customize the method used to generate portfolios, please see the
#' help page on \code{\link{portfolios}}}.
#' @section Fields:
#'
#' \describe{
#'
#' \item{$name}{`character` name of portfolio method.}
#'
#' \item{$parameters}{`Parameters` object with parameters used to customize
#'   the the portfolio.}
#'
#' \item{$run}{`function` used to generate a portfolio.}
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
#' `x$run(op, sol)`
#'
#' @section Arguments:
#' \describe{
#'
#' \item{x}{\code{\link{Solver-class}} object.}
#'
#' \item{op}{\code{\link{OptimizationProblem-class}} object.}
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
#' \item{run}{solve an \code{\link{OptimizationProblem-class}} object using this
#'   object and a \code{\link{Solver-class}} object.}
#'
#' }
#'
#' @name Portfolio-class
#'
#' @aliases Portfolio
NULL

#' @export
Portfolio <- pproto(
  "Portfolio",
  name = character(0),
  run = function(...) stop("portfolio is missing a run method"),
  parameters = parameters(),
  print = function(self) {
    message(self$repr())
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    if (self$parameters$length() > 0)
      return(paste(self$name, self$parameters$repr()))
    return(self$name)
  },
  get_parameter = function(self, x) {
    self$parameters$get(x)
  },
  set_parameter = function(self, x, value) {
    self$parameters$set(x, value)
  },
  render_parameter = function(self, x) {
    self$parameters$render(x)
  },
  render_all_parameters = function(self) {
    shiny::div(class = "Portfolio",
                self$parameters$render_all())
  })
