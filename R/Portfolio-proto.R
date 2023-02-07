#' @include internal.R pproto.R
NULL

#' @export
if (!methods::isClass("Portfolio")) methods::setOldClass("Portfolio")
NULL

#' Portfolio prototype
#'
#' This prototype is used to represent methods for generating portfolios of
#' optimization problems. **This class represents a recipe to
#' create portfolio generating method and is only recommended for use by expert
#' users. To customize the method used to generate portfolios, please see the
#' help page on [portfolios]**.
#' @section Fields:
#'
#' \describe{
#'
#' \item{$name}{`character` name of portfolio method.}
#'
#' \item{$data}{`list` object with data.}
#'
#' \item{$internal}{`list` object with internal data.}
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
#' `x$run(op, sol)`
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
#' \item{run}{solve an [`OptimizationProblem-class`] object using this
#'   object and a [`Solver-class`] object.}
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
  data = list(),
  internal = list(),
  print = function(self) {
    message(self$repr())
  },
  show = function(self) {
    self$print()
  },
  repr = function(self, compact = TRUE) {
    repr_data_list(self$name, self$data, compact = compact)
  },
  run = function(...) {
    cli::cli_abort("Portfolio is missing a run method.", .internal = TRUE)
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
