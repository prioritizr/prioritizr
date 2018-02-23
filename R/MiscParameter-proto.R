#' @include internal.R pproto.R Parameter-proto.R
NULL

#' @export
methods::setOldClass("MiscParameter")

#' Miscellaneous Parameter prototype
#'
#' This prototype is used to represent a parameter that can be any object.
#' \strong{Only experts should interact directly with this prototype.}
#'
#' @section Fields:
#' \describe{
#'
#' \item{$id}{\code{character} identifier for parameter.}
#'
#' \item{$name}{\code{character} name of parameter.}
#'
#' \item{$value}{\code{\link[tibble]{tibble}} object.}
#'
#' \item{$validator}{\code{list} object containing a \code{function} that
#'   is used to validate changes to the parameter.}
#'
#' \item{$widget}{\code{list} object containing a \code{function} used to
#'   construct a \emph{shiny} interface for modifying values.}
#' }
#'
#' @section Usage:
#'
#' \code{x$print()}
#'
#' \code{x$show()}
#'
#' \code{x$validate(x)}
#'
#' \code{x$get()}
#'
#' \code{x$set(x)}
#'
#' \code{x$reset()}
#'
#' \code{x$render(...)}
#'
#' @section Arguments:
#'
#' \describe{
#'
#' \item{x}{object used to set a new parameter value.}
#'
#' \item{...}{arguments passed to \code{$widget}.}
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
#' \item{render}{create a \code{\link[shiny]{shiny}} widget to modify
#'               parameter values.}
#'
#' }
#'
#' @name MiscParameter-class
#'
#' @seealso \code{\link{Parameter-class}}.
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
    self$widget[[1]](self$value)
  })
