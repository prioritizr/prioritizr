#' @include internal.R ArrayParameter-class.R ScalarParameter-class.R Parameters-class.R
NULL

#' Scalar parameters
#'
#' These functions are used to create parameters that consist of a single
#' number. Parameters have a name, a value, a defined range of acceptable values,
#' a default value, a class, and a \code{\link[shiny]{shiny}} widget for 
#' modifying them. If values are supplied to a parameter that are unacceptable
#' then an error is thrown.
#'
#' @param x \code{character} name of parameter.
#' @param value \code{integer} or \code{double} value depending on the
#'                parameter.
#' 
#' @details Below is a list of parameter generating functions and a brief 
#'          description of each.
#' \describe{
#' \item{proportion_parameter}{A parameter that is a double bounded between
#'                             zero and one.)}
#' \item{integer_parameter}{A parameter that is an integer with no bounds 
#'                          (except those imposed by the host system.))}
#' \item{numeric_parameter}{A parameter that is a double with no bounds 
#'                          (except those imposed by the host system.))}
#' \item{truncated_numeric_parameter}{A parameter that is a double that has
#'                          a lower bound of zero.}
#' \item{truncated_integer_parameter}{A parameter that is an integer that has
#'                          a lower bound of zero.}
#' \item{binary_parameter}{A parameter that is restricted to integer values of
#'                          zero or one.}
#' }
#'
#' @return \code{\link{ScalarParameter}} object.
#' @name scalar_parameters
NULL

#' @rdname scalar_parameters
proportion_parameter <- function(x, value) {
  ScalarParameter$new(name=x, value=value, default=value, class='numeric',
                range=c(0.0, 1.0), widget=shiny::sliderInput)
} 

#' @rdname scalar_parameters
integer_parameter <- function(x, value) {
  ScalarParameter$new(name=x, value=value, default=value, class='integer',
                range=as.integer(c(-.Machine$integer.max, .Machine$integer.max)),
                widget=shiny::numericInput)
} 

#' @rdname scalar_parameters
numeric_parameter <- function(x, value) {
  ScalarParameter$new(name=x, value=value, default=value, class='numeric',
                range=c(.Machine$double.xmin, .Machine$double.xmax),
                shiny::numericInput)
}

#' @rdname scalar_parameters
truncated_numeric_parameter <- function(x, value) {
  ScalarParameter$new(name=x, value=value, default=value, class='numeric',
                range=c(0.0, .Machine$double.xmax),
                shiny::numericInput)
}

#' @rdname scalar_parameters
truncated_integer_parameter <- function(x, value) {
  ScalarParameter$new(name=x, value=value, default=value, class='integer',
                range=as.integer(c(0L, .Machine$integer.max)),
                shiny::numericInput)
}

#' @rdname scalar_parameters
binary_parameter <- function(x, value) {
  ScalarParameter$new(name=x, value=value, default=value, class='integer',
                  range=c(0L, 1L), widget=shiny::checkboxInput)
}

#' Array parameters
#'
#' These functions are used to create parameters that consist of multiple
#' numbers. Parameters have a name, multiple values, a label for each 
#' value, defalt values, a defined range of acceptable values, a class, 
#' and a function for generating a \code{\link[shiny]{shiny}} widget to
#' modify them. If values are supplied to a parameter that are unacceptable,
#' then an error is thrown.
#'
#' @param x \code{character} name of parameter.
#'
#' @param value \code{vector} of values.
#'
#' @param label \code{character} vector of values to label values with.
#' 
#' @details Below is a list of parameter generating functions and a brief 
#'    description of each.
#' \describe{
#'   \item{proportion_parameter_array}{array of \code{numeric} values that 
#'     between zero and one.}
#'   \item{truncated_numeric_parameter_array}{array of \code{numeric} values
#'     that are bounded at zero.}
#' }
#'
#' @return \code{\link{ArrayParameter}} object.
#'
#' @name array_parameters
NULL

#' @rdname array_parameters
proportion_parameter_array <- function(x, value, label) {
  ArrayParameter$new(name = x, value=value, label=label, class='numeric',
                     range=c(0.0, 1.0), default=value, length=length(value),
                     widget=rhandsontable::rHandsontableOutput)
}

#' @rdname array_parameters
truncated_numeric_parameter_array <- function(x, value, label) {
  ArrayParameter$new(name = x, value=value, label=label, class='numeric',
                     range=c(0.0, .Machine$double.xmax), default=value,
                     length=length(value), widget=rhandsontable::rHandsontableOutput)
}

#' Parameters
#'
#' Create a collection of parameter objects.
#' 
#' ... \code{\link{ArrayParameter}} and/or 
#'   \code{\link{ScalarParameter}}) objects to store in collection.
#'
#' @return \code{\link{Parameters}} object.
#'
#' @seealso \code{\link{array_parameters}}, code{\link{scalar_parameters}}.
#'
#' @export
parameters <- function(...) Parameters$new(...)

