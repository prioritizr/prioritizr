#' @include internal.R
NULL

#' @export
methods::setOldClass('ScalarParameter')

#' Scalar Parameter class
#'
#' This class is used to represent a parameter has a single value.
#' \strong{Only experts should interact directly with this class.}
#'
#' @section Fields: 
#' \itemize{
#' \item{$id}{\code{character} identifier for parameter.}
#' \item{$name}{\code{character} name of parameter.}
#' \item{$value}{\code{vector} of values.}
#' \item{$default}{\code{vector} of default values.}
#' \item{$class}{\code{character} class of values.}
#' \item{$range}{\code{vector} specifying minimum and maximum values.}
#' \item{$widget}{\code{function} used to construct a
#'                \code{\link[shiny]{shiny}} interface for modifying values.}
#' } 
#'
#' @section Usage: 
#' \preformatted{x <- ScalarParameter$new(name, value, default, class, range, widget)}
#' \code{x$print()}
#' \code{x$show()}
#' \code{x$validate(x)}
#' \code{x$get()}
#' \code{x$set(x)}
#' \code{x$reset()}
#' \code{x$render(...)}
#'
#' @section Arguments: 
#' \describe{
#' \item{x}{object used to set a new parameter value.}
#' \item{...}{arguments passed to function in \code{widget} field.}
#'  }
#'
#' @section Details:
#' \describe{
#' \item{print}{print the object.}
#' \item{show}{show the object.}
#' \item{validate}{check if a proposed new set of parameters are valid.}
#' \item{get}{extract the parameter value.}
#' \item{set}{update the parameter value.}
#' \item{reset}{update the parameter value to be the default value.}
#' \item{render}{create a \code{\link[shiny]{shiny}} widget to modify 
#'               parameter values.}
#' }
#'
#' @name ScalarParameter
#' @seealso \code{\link{ArrayParameter}}.
NULL

#' @export
ScalarParameter <- R6::R6Class('ScalarParameter', 
  public = list(
    id = NULL,
    name = NULL,
    value = NULL,
    default = NULL,
    class = NULL,
    range = NULL,
    widget = NULL,
    initialize = function(name, value, default, class, range, widget) {
      assertthat::assert_that(
        assertthat::is.string(name),
        inherits(widget, 'function'),
        inherits(value, class),
        inherits(default, class),
        inherits(range, class),
        length(range)==2,
        range[1] <= range[2],
        default >= range[1], default <= range[2],
        value >= range[1], value <= range[2],
        is.finite(value), is.finite(default),
        all(is.finite(range)))
      self$id <- new_id()
      self$name <- name
      self$default <- default
      self$value <- value
      self$class <- class
      self$range <- range
      self$widget <- widget
    },
    print = function() {
      message('ScalarParameter object\n',
              '  name:    ',self$name,'\n',
              '  value:   ',self$value,'\n',
              '  default: ',self$default,'\n',
              '  class:   ',self$class,'\n',
              '  range:   ',paste(self$range, collapse=', '),' (min, max)')
    },
    show = function() {
      self$print()
    },
    repr = function() {
      paste0(self$name, ' (', self$value, ')')
    },
    validate = function(x) {
      invisible(assertthat::see_if(
        inherits(x, self$class),
        isTRUE(x >= self$range[1]),
        isTRUE(x <= self$range[2]),
        is.finite(x)
      ))
    },
    get = function() {
      self$value
    },
    set = function(x) {
      if(!self$validate(x))
        stop('invalid input')
      self$value <- x
    },
    reset = function() {
      self$value <- self$default
    },
    render = function(...) {
      # get all possible arguments
      args <- list(inputId=self$id, label=self$name, value=self$value,
                   min=self$range[1], max=self$range[1])
      # subset to include only valid arguments
      args <- args[intersect(names(args),
                             names(as.list(args(self$widget))))]
      do.call(self$widget, append(args, list(...)))
    }
  )
)

