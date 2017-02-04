#' @include internal.R

#' @export
methods::setOldClass('ArrayParameter')

#' Array Parameter class
#'
#' This class is used to represent a parameter has multiple values. Each value 
#' is has a label to differentiate values. \strong{Only experts should interact
#' directly with this class.}
#'
#' @section Fields: 
#' \itemize{
#' \item{$id}{\code{character} identifier for parameter.}
#' \item{$name}{\code{character} name of parameter.}
#' \item{$value}{\code{vector} of values.}
#' \item{$label}{\code{character} names for each value.}
#' \item{$default}{\code{vector} of default values.}
#' \item{$length}{\code{integer} number of values.}
#' \item{$class}{\code{character} class of values.}
#' \item{$range}{\code{vector} specifying minimum and maximum values.}
#' \item{$widget}{\code{function} used to construct a
#'                \code{\link[shiny]{shiny}} interface for modifying values.}
#' } 
#'
#' @section Usage: 
#' \preformatted{x <- ArrayParameter$new(name, value, default, length, 
#'                                       class, range, widget)}
#' \code{x$print()}
#' \code{x$show()}
#' \code{x$repr()}
#' \code{x$validate(tbl)}
#' \code{x$get()}
#' \code{x$set(tbl)}
#' \code{x$reset()}
#' \code{x$render(...)}
#'
#' @section Arguments: 
#' \describe{
#' \item{tbl}{\code{\link{data.frame}} containing new parameter values with 
#'            row names indicating the labels and a column called 'values' 
#'            containing the new parameter values.}
#' \item{...}{arguments passed to function in \code{widget} field.}
#' }
#'
#' @section Details:
#' \describe{
#' \item{print}{print the object.}
#' \item{show}{show the object.}
#' \item{repr}{\code{character} representation of object.}
#' \item{validate}{check if a proposed new set of parameters are valid.}
#' \item{get}{return a \code{\link[base]{data.frame}} containing the
#'   parameter values.}
#' \item{set}{update the parameter values using a
#'   \code{\link[base]{data.frame}}.}
#' \item{reset}{update the parameter values to be the default values.}
#' \item{render}{create a \code{\link[shiny]{shiny}} widget to modify 
#'   parameter values.}
#' }
#'
#' @seealso \code{\link{ScalarParameter}}.
#'
#' @name ArrayParameter
NULL

#' @export
ArrayParameter <- R6::R6Class('ArrayParameter', 
  public = list(
    id = NULL,
    name = NULL,
    value = NULL,
    label = NULL,
    default = NULL,
    length = NULL,
    class = NULL,
    range = NULL,
    widget = NULL,
    initialize = function(name, value, label, default, class, range, length, widget) {
      assertthat::assert_that(
        assertthat::is.string(name),
        inherits(widget, 'function'),
        inherits(value, class),
        inherits(default, class),
        inherits(range, class),
        inherits(length, 'integer'),
        inherits(default, class),
        length(value)==length,
        length(label)==length,
        length(default)==length,
        length(range)==2, range[1] <= range[2],
        all(default >= range[1]), all(default <= range[2]),
        all(value >= range[1]), all(value <= range[2]),
        all(is.finite(value)), sum(is.na(default))==0,
        all(is.finite(range)))
      self$id <- new_id()
      self$name <- name
      self$default <- default
      self$value <- value
      self$class <- class
      self$range <- range
      self$widget <- widget
      self$length <- length
      self$label <- label
    },
    print = function() {
      message('ArrayParameter object\n',
              '  name:    ',self$name,'\n',
              '  value:   ',min(self$value), ', ', max(self$value) ,' (min, max)\n',
              '  default: ',min(self$default), ', ', max(self$default),' (min, max)\n',
              '  class:   ',self$class,'\n',
              '  range:   ',paste(self$range, collapse=', '),' (min, max)')
    },
    show = function() {
      self$print()
    },
    repr = function() {
      paste0(self$name, ' (min: ', min(self$value), ', max: ', max(self$value), ')')
    },
    validate = function(x) {
      assertthat:::assert_that(inherits(x, 'data.frame'))
      invisible(assertthat::see_if(identical(names(x),'value'),
                         ncol(x)==1,
                         nrow(x)==self$length,
                         inherits(x[[1]], self$class),
                         setequal(self$label, rownames(x)),
                         sum(!is.finite(x[[1]]))==0,
                         isTRUE(all(x[[1]] >= self$range[1])),
                         isTRUE(all(x[[1]] <= self$range[2]))))
    },
    reset = function() {
      self$value <- self$default
    },
    set = function(x) {
      if (!self$validate(x))
        stop('invalid input')
      self$value <- x[[1]][match(rownames(x), self$label)]
    },
    get = function() {
      structure(list(value=self$value),
                .Names = 'value',
                row.names = self$label,
                class = "data.frame")
    },
    render = function(...) {
      do.call(self$widget, list(outputId=self$id))
    }
  )
)
