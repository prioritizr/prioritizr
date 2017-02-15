#' @include internal.R Parameter-proto.R
NULL

#' @export
methods::setOldClass('ScalarParameter')

#' Scalar Parameter prototype
#'
#' This prototype is used to represent a parameter has a single value.
#' \strong{Only experts should interact directly with this prototype.}
#'
#' @section Fields: 
#' \itemize{
#' \item{$id}{\code{character} identifier for parameter.}
#' \item{$name}{\code{character} name of parameter.}
#' \item{$value}{\code{numeric} scalar value.}
#' \item{$default}{\code{numeric} scalar default value.}
#' \item{$class}{\code{character} class of value.}
#' \item{$lower_limit}{\code{numeric} scalar value minimum permitted value.}
#' \item{$upper_limit}{\code{numeric} scalar value maximum permitted value.}
#' \item{$widget}{\code{function} used to construct a
#'                \code{\link[shiny]{shiny}} interface for modifying values.}
#' } 
#'
#' @section Usage: 
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
#'
#' @seealso \code{\link{Parameter}}, \code{\link{ArrayParameter}}.
NULL

#' @export
ScalarParameter <- pproto(
  NULL,
  Parameter,
  repr = function(self) {
    paste0(self$name, ' (', self$value, ')')
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
    check(self$validate(x))
    self$value <- x
  },
  render = function(self, ...) {
    # get all possible arguments
    args <- list(inputId=self$id, label=self$name, value=self$value,
                  min=self$lower_limit, max=self$upper_limit)
    # subset to include only valid arguments
    args <- args[intersect(names(args),
                            names(as.list(args(self$widget))))]
    do.call(self$widget, append(args, list(...)))
  })


