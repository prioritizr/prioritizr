#' @include internal.R pproto.R parameters.R
NULL

#' @export
methods::setOldClass("Portfolio")

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
#' \item{$name}{\code{character} name of portfolio method.}
#'
#' \item{$parameters}{\code{Parameters} object with parameters used to customize
#'   the the portfolio.}
#'
#' \item{$run}{\code{function} used to generate a portfolio.}
#' }
#'
#' @section Usage:
#'
#' \code{x$print()}
#'
#' \code{x$show()}
#'
#' \code{x$repr()}
#'
#' \code{x$run(op, sol)}
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
#' \item{repr}{\code{character} representation of object.}
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
    shiny::div(class = "Portfolio",
                self$parameters$render_all())
  })
