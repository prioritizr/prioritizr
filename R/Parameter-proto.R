#' @include internal.R pproto.R

#' @export
if (!methods::isClass("Parameter")) methods::setOldClass("Parameter")
NULL

#' Parameter class
#'
#' This class is used to represent a parameter that has multiple values. Each
#' value has a different label to differentiate values. **Only experts
#' should interact directly with this class.**
#'
#' @section Fields:
#' \describe{
#'
#' \item{$id}{[`Id`] identifier for parameter.}
#'
#' \item{$name}{`character` name of parameter.}
#'
#' \item{$value}{`numeric` vector of values.}
#'
#' \item{$default}{`numeric` vector of default values.}
#'
#' \item{$class}{`character` name of the class that the values inherit
#'   from (e.g., `"integer"`.}
#'
#' \item{$lower_limit}{`numeric` vector specifying the minimum
#'   permitted value for each element in `$value`.}
#'
#' \item{$upper_limit}{`numeric` vector specifying the maximum
#'   permitted value for each element in `$value`.}
#'
#' }
#'
#' @section Usage:
#'
#' `x$print()`
#'
#' `x$show()`
#'
#' `x$reset()`
#'
#' @section Details:
#' \describe{
#' \item{print}{print the object.}
#' \item{show}{show the object.}
#' \item{reset}{change the parameter values to be the default values.}
#' }
#'
#' @seealso [`ScalarParameter-class`].
#'
#' @name Parameter-class
#'
#' @aliases Parameter
NULL

#' @export
Parameter <- pproto(
  "Parameter",
  id = structure(list(), class = "ID"),
  name = character(0),
  value = numeric(0),
  default = numeric(0),
  class = character(0),
  print = function(self) {
    message(self$repr())
  },
  show = function(self) {
    self$print()
  },
  reset = function(self) {
    self$value <- self$default
  })
