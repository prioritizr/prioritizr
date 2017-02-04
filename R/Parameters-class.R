#' @include internal.R ScalarParameter-class.R ArrayParameter-class.R 
NULL

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
#' \preformatted{x <- Parameters$new(...)}
#' \code{x$print()}
#' \code{x$show()}
#' \code{x$repr()}
#' \code{x$names()}
#'
#' \code{x$get(id)}
#' \code{x$set(id, value)}
#' \code{x$render(id)}
#' \code{x$render_all()}
#'
#' @section Arguments:
#' \describe{
#' \item{id}{\code{id} object.}
#' \item{value}{any object.}
#' }
#'
#' @section Details:
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
Parameters <- R6::R6Class('Parameters',
  public=list(
    parameters = NULL,
    initialize = function(...) {
      p <- list(...)
      assertthat::assert_that(
        all(sapply(p, inherits, 'ArrayParameter') | 
            sapply(p, inherits, 'ScalarParameter')))
      self$parameters <- p
    },
    print = function() {
      message('Parameters object (',length(self$parameters),')')
    },
    show = function() {
      self$print()
    },
    repr = function() {
      if (length(self$parameters)>0)
        return(paste(sapply(self$parameters, 
                            function(x) {x$repr()}), collapse=', '))
      return('(defaults)')
      
    },
    names = function() {
      sapply(self$parameters, function(x) x$name)
    },
    get = function(x) {
      assertthat::assert_that(assertthat::is.string(x) || is.id(x))
      self$parameters[[private$find(x)]]$get()
    },
    set = function(x, value) {
      assertthat::assert_that(assertthat::is.string(x) || is.id(x))
      self$parameters[[private$find(x)]]$set(value)
    },
    reset = function(x) {
      assertthat::assert_that(assertthat::is.string(x) || is.id(x))
      self$parameters[[private$find(x)]]$reset()
    },
    render = function(x) {
      assertthat::assert_that(assertthat::is.string(x) || is.id(x))
      self$parameters[[private$find(x)]]$render()
    },
    render_all = function() {
      do.call(shiny::div, 
          append(list(class='Parameters'), 
                 lapply(self$parameters, function(x) {x$render()})))
    },
    reset_all = function() {
      for (i in seq_along(self$parameters))
        self$parameters[[i]]$reset()
      invisible()
    }
  ),
  private = list(
    find = function(x) {
      assertthat::assert_that(assertthat::is.string(x) || is.id(x))
      if (inherits(x, 'id')) {
        i <- match(x, sapply(self$parameters, function(x) x$id))
        if (!is.finite(i))
          stop('parameter with matching id not found')
        if (length(i) > 1)
          stop('multiple parameters with the same id')
      } else {
        i <- match(x, sapply(self$parameters, function(x) x$name))
        if (!is.finite(i))
          stop('parameter with matching name not found')
        if (length(i) > 1)
          stop('multiple parameters with the same name')
      }
      i
    }  
  )
)

