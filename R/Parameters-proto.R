#' @include internal.R pproto.R
NULL

#' @export
methods::setOldClass("Parameters")

#' Parameters class
#'
#' This class represents a collection of \code{\link{Parameter-class}} objects.
#' It provides methods for accessing, updating, and rendering the parameters
#' stored inside it.
#'
#' @section Fields:
#' \describe{
#'
#' \item{$parameters}{\code{list} object containing
#'   \code{\link{Parameter-class}} objects.}
#'
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
#' \code{x$names()}
#'
#' \code{x$ids()}
#'
#' \code{x$length()}
#'
#' \code{x$get(id)}
#'
#' \code{x$set(id, value)}
#'
#' \code{x$add(p)}
#'
#' \code{x$render(id)}
#'
#' \code{x$render_all()}
#'
#' @section Arguments:
#' \describe{
#' \item{id}{\code{\link{Id}} object.}
#'
#' \item{p}{\code{\link{Parameter-class}} object.}
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
#' \item{repr}{\code{character} representation of object.}
#'
#' \item{names}{return \code{character} names of parameters.}
#'
#' \item{ids}{return \code{character} parameter unique identifiers.}
#'
#' \item{length}{return \code{integer} number of parameters in object.}
#
#' \item{get}{retrieve the value of a parameter in the object
#'    using an \code{Id} object.}
#'
#' \item{set}{change the value of a parameter in the object
#'    to a new object.}
#'
#' \item{render}{generate a \emph{shiny} widget to modify the
#'   the value of a parameter (specified by argument \code{Id}).}
#'
#' \item{render_all}{generate a \code{\link[shiny]{div}}
#'   containing all the parameters" widgets.}
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
  render = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x) || is.id(x))
    self[[self$find(x)]]$render()
  },
  render_all = function(self) {
    do.call(shiny::div,
        append(list(class = "Parameters"),
                lapply(self$ids(),
                       function(x) self[[x]]$render())))
  },
  reset_all = function(self) {
    for (i in self$ids())
      self[[i]]$reset()
    invisible()
  })
