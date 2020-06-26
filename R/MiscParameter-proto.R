#' @include internal.R pproto.R Parameter-proto.R
NULL

#' @export
if (!methods::isClass("MiscParameter")) methods::setOldClass("MiscParameter")
NULL

#' Miscellaneous parameter prototype
#'
#' This prototype is used to represent a parameter that can be any object.
#' **Only experts should interact directly with this prototype.**
#'
#' @section Fields:
#' \describe{
#'
#' \item{$id}{`character` identifier for parameter.}
#'
#' \item{$name}{`character` name of parameter.}
#'
#' \item{$value}{[tibble::tibble()] object.}
#'
#' \item{$validator}{`list` object containing a `function` that
#'   is used to validate changes to the parameter.}
#'
#' \item{$widget}{`list` object containing a `function` used to
#'   construct a *shiny* interface for modifying values.}
#' }
#'
#' @section Usage:
#'
#' `x$print()`
#'
#' `x$show()`
#'
#' `x$validate(x)`
#'
#' `x$get()`
#'
#' `x$set(x)`
#'
#' `x$reset()`
#'
#' `x$render(...)`
#'
#' @section Arguments:
#'
#' \describe{
#'
#' \item{x}{object used to set a new parameter value.}
#'
#' \item{...}{arguments passed to `$widget`.}
#'
#'  }
#'
#' @section Details:
#' \describe{
#'
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{validate}{check if a proposed new parameter is valid.}
#'
#' \item{get}{extract the parameter value.}
#'
#' \item{set}{update the parameter value.}
#'
#' \item{reset}{update the parameter value to be the default value.}
#'
#' \item{render}{create a [shiny::shiny()] widget to modify
#'               parameter values.}
#'
#' }
#'
#' @name MiscParameter-class
#'
#' @seealso [Parameter-class].
#'
#' @aliases MiscParameter
NULL

#' @export
MiscParameter <- pproto(
  "MiscParameter",
  Parameter,
  repr = function(self) {
    self$name
  },
  print = function(self) {
    self$repr()
  },
  validate = function(self, x) {
    self$validator[[1]](x)
  },
  get = function(self) {
    self$value
  },
  set = function(self, x) {
    check_that(self$validate(x))
    self$value <- x
  },
  render = function(self, ...) {
    self$widget[[1]](self$id, self$value)
  })
