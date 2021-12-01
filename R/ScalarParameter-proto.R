#' @include internal.R pproto.R Parameter-proto.R
NULL

#' @export
if (!methods::isClass("ScalarParameter")) methods::setOldClass("ScalarParameter")
NULL

#' Scalar parameter prototype
#'
#' This prototype is used to represent a parameter has a single value.
#' **Only experts should interact directly with this prototype.**
#'
#' @section Fields:
#' \describe{
#'
#' \item{$id}{`character` identifier for parameter.}
#'
#' \item{$name}{`character` name of parameter.}
#'
#' \item{$value}{`numeric` scalar value.}
#'
#' \item{$default}{`numeric` scalar default value.}
#'
#' \item{$class}{`character` name of the class that `$value` should
#'   inherit from (e.g., `integer`).}
#'
#' \item{$lower_limit}{`numeric` scalar value that is the minimum value
#'   that `$value` is permitted to be.}
#'
#' \item{$upper_limit}{`numeric` scalar value that is the maximum value
#'   that `$value` is permitted to be.}
#'
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
#' @section Arguments:
#'
#' \describe{
#'
#' \item{x}{object used to set a new parameter value.}
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
#' \item{validate}{check if a proposed new set of parameters are valid.}
#'
#' \item{get}{extract the parameter value.}
#'
#' \item{set}{update the parameter value.}
#'
#' \item{reset}{update the parameter value to be the default value.}
#'
#' }
#'
#' @name ScalarParameter-class
#'
#' @seealso [`Parameter-class`], [`ArrayParameter-class`].
#'
#' @aliases ScalarParameter
NULL

#' @export
ScalarParameter <- pproto(
  "ScalarParameter",
  Parameter,
  upper_limit = numeric(0),
  lower_limit = numeric(0),
  repr = function(self) {
    paste0(self$name, " (", self$value, ")")
  },
  validate = function(self, x) {
    invisible(assertthat::see_if(
      inherits(x, self$class),
      isTRUE(x >= self$lower_limit),
      isTRUE(x <= self$upper_limit),
      is.finite(x)
    ))
  },
  get = function(self) {
    self$value
  },
  set = function(self, x) {
    check_that(self$validate(x))
    self$value <- x
  })
