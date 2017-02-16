#' @include internal.R pproto.R

#' @export
methods::setOldClass('Parameter')

#' Parameter class
#'
#' This class is used to represent a parameter has multiple values. Each value 
#' is has a label to differentiate values. \strong{Only experts should interact
#' directly with this class.}
#'
#' @section Fields: 
#' \itemize{
#' \item{$id}{\code{character} identifier for parameter.}
#' \item{$name}{\code{character} name of parameter.}
#' \item{$value}{\code{numeric} \code{vector} of values.}
#' \item{$default}{\code{numeric} \code{vector} of default values.}
#' \item{$class}{\code{character} class of values.}
#' \item{$lower_limit}{\code{numeric} \code{vector} specifying the minimum
#'   permitted values.}
#' \item{$upper_limit}{\code{numeric} \code{vector} specifying the maximum
#'   permitted values.}
#' \item{$widget}{\code{function} used to construct a
#'                \code{\link[shiny]{shiny}} interface for modifying values.}
#' } 
#'
#' @section Usage: 
#' \code{x$print()}
#' \code{x$show()}
#' \code{x$reset()}
#'
#' @section Details:
#' \describe{
#' \item{print}{print the object.}
#' \item{show}{show the object.}
#' \item{reset}{update the parameter values to be the default values.}
#' }
#'
#' @seealso \code{\link{ScalarParameter}}.
#'
#' @name Parameter
NULL

#' @export
Parameter <- pproto(
    'Parameter',
    id = structure(NULL, class='I'),
    name = character(0),
    value = numeric(0),
    default = numeric(0),
    class = character(0),
    upper_limit = numeric(0),
    lower_limit = numeric(0),
    widget = function(...) {stop('no widget defined')},
    print = function(self) {
      message(self$repr())
    },
    show = function(self) {
      self$print()
    },
    reset = function(self) {
      self$value <- self$default
    })
