#' @include internal.R pproto.R
NULL

#' @export
methods::setOldClass('Parameters')

#' Parameters class
#'
#' This class represents a collection of \code{\link{ArrayParameter}}, and
#' \code{\link{ScalarParameter}} objects.
#'
#' @section Fields:
#' \itemize{
#' \item{$parameters}{\code{list} object containing
#'  \code{\link{ArrayParameter}}, and \code{\link{ScalarParameter}} objects.}
#' }
#'
#' @section Usage:
#' \code{x$print()}
#' \code{x$show()}
#' \code{x$repr()}
#' \code{x$names()}
#'
#' \code{x$get(id)}
#' \code{x$set(id, value)}
#' \code{x$add(p)}
#' \code{x$render(id)}
#' \code{x$render_all()}
#'
#' @section Arguments:
#' \describe{
#' \item{id}{\code{id} object.}
#' \item{p}{\code{\link{Parameter}} object.}
#' \item{value}{any object.}
#' }
#'
#' \describe{
#' \item{print}{print the object.}
#' \item{show}{show the object.}
#' \item{repr}{\code{character} representation of object.}
#' \item{names}{return \code{character} names of parameters.}
#'
#' \item{get}{retrieve the value of a parameter in the object
#'    using an \code{id} object.}
#' \item{set}{change the value of a parameter in the object 
#'    to a new object.}
#' \item{render}{generate a \emph{shiny} widget to modify the
#'   the value of a parameter (specified by argument \code{id}).}
#' \item{render_all}{generate a \emph{shiny} \code{div}
#'   containing all the parameters' widgets.}
#' }
#'
#' @name Parameters
NULL

#' @export
Parameters <- pproto(
  'Parameters',
  parameters = list(),
  print = function(self) {
    message(self$repr())
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    if (self$length()>0)
      return(paste0('[',paste(sapply(
        self$parameters, 
        function(x) {x$repr()}), collapse=', '), ']'))
    return('[]')
  },
  find = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x) || is.id(x))
    if (inherits(x, 'id')) {
      i <- match(x, sapply(self$parameters, function(x) x$id))
      if (!is.finite(i))
        stop('parameter with matching id not found')
      if (base::length(i) > 1)
        stop('multiple parameters with the same id')
    } else {
      i <- match(x, sapply(self$parameters, function(x) x$name))
      if (!is.finite(i))
        stop('parameter with matching name not found')
      if (base::length(i) > 1)
        stop('multiple parameters with the same name')
    }
    i
  },  
  length = function(self) {
    base::length(self$parameters)
  },
  names = function(self) {
    sapply(self$parameters, function(x) x$name)
  },
  get = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x) || is.id(x))
    self$parameters[[self$find(x)]]$get()
  },
  set = function(self, x, value) {
    assertthat::assert_that(assertthat::is.string(x) || is.id(x))
    self$parameters[[self$find(x)]]$set(value)
    invisible()
  },
  add = function(self, x) {
    assertthat::assert_that(inherits(x, 'Parameter'))
    self$parameters <- append(self$parameters, list(x))
    invisible()
  },
  reset = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x) || is.id(x))
    self$parameters[[self$find(x)]]$reset()
    invisible()
  },
  render = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x) || is.id(x))
    self$parameters[[self$find(x)]]$render()
  },
  render_all = function(self) {
    do.call(shiny::div, 
        append(list(class='Parameters'), 
                lapply(self$parameters, function(x) {x$render()})))
  },
  reset_all = function(self) {
    for (i in seq_along(self$parameters))
      self$parameters[[i]]$reset()
    invisible()
  })


