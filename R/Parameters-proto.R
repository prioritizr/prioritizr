#' @include internal.R pproto.R
NULL

#' @export
if (!methods::isClass("Parameters")) methods::setOldClass("Parameters")
NULL

#' Parameters class
#'
#' This class represents a collection of [`Parameter-class`] objects.
#' It provides methods for accessing and the parameters
#' stored inside it.
#'
#' @section Fields:
#' \describe{
#'
#' \item{$parameters}{`list` object containing
#'   [`Parameter-class`] objects.}
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
#' `x$names()`
#'
#' `x$ids()`
#'
#' `x$length()`
#'
#' `x$get(id)`
#'
#' `x$set(id, value)`
#'
#' `x$add(p)`
#'
#' @section Arguments:
#' \describe{
#' \item{id}{[`Id`] object.}
#'
#' \item{p}{[`Parameter-class`] object.}
#'
#' \item{value}{any object.}
#'
#' }
#'
#' @section Details:
#'
#' \describe{
#'
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{repr}{`character` representation of object.}
#'
#' \item{names}{return `character` names of parameters.}
#'
#' \item{ids}{return `character` parameter unique identifiers.}
#'
#' \item{length}{return `integer` number of parameters in object.}
#
#' \item{get}{retrieve the value of a parameter in the object
#'    using an `Id` object.}
#'
#' \item{set}{change the value of a parameter in the object
#'    to a new object.}
#'
#' }
#'
#' @name Parameters-class
#'
#' @aliases Parameters
NULL

#' @export
Parameters <- pproto(
  "Parameters",
  print = function(self) {
    message(self$repr())
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    if (self$length() > 0)
      return(paste0("[", paste(sort(vapply(self$ids(),
                                           function(x) self[[x]]$repr(),
                                           character(1))),
                                collapse = ", "), "]"))
    return("[]")
  },
  ids = function(self) {
    o <- self$ls()
    o[!vapply(o, function(x) inherits(self[[x]], "function"), logical(1))]
  },
  find = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x) || is.id(x))
      if (inherits(x, "Id")) {
        return(x)
    } else {
      n <- self$ids()
      x <- match(x, vapply(n, function(j) self[[j]]$name, character(1)))
      if (!is.finite(x))
        stop("parameter with matching name not found")
      if (base::length(x) > 1)
        stop("multiple parameters with the same name")
      return(n[x])
    }
  },
  length = function(self) {
    base::length(self$ids())
  },
  names = function(self) {
    vapply(self$ids(), function(x) self[[x]]$name, character(1))
  },
  get = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x) || is.id(x))
    self[[self$find(x)]]$get()
  },
  set = function(self, x, value) {
    assertthat::assert_that(assertthat::is.string(x) || is.id(x))
    self[[self$find(x)]]$set(value)
    invisible()
  },
  add = function(self, x) {
    assertthat::assert_that(inherits(x, "Parameter"))
    self[[as.character(x$id)]] <- x
    invisible()
  },
  reset = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x) || is.id(x))
    self[[self$find(x)]]$reset()
    invisible()
  },
  reset_all = function(self) {
    for (i in self$ids())
      self[[i]]$reset()
    invisible()
  })
