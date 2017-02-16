#' @include internal.R ArrayParameter-proto.R ScalarParameter-proto.R Parameters-proto.R
NULL

#' Scalar parameters
#'
#' These functions are used to create parameters that consist of a single
#' number. Parameters have a name, a value, a defined range of acceptable
#' values, a default value, a class, and a \code{\link[shiny]{shiny}} widget for 
#' modifying them. If values are supplied to a parameter that are unacceptable
#' then an error is thrown.
#'
#' @param name \code{character} name of parameter.
#' 
#' @param value \code{integer} or \code{double} value depending on the
#'    parameter.
#' 
#' @details Below is a list of parameter generating functions and a brief 
#'   description of each.
#'
#' \describe{
#'
#' \item{proportion_parameter}{A parameter that is a \code{double} and bounded
#'   between zero and one.}
#'
#' \item{integer_parameter}{A parameter that is a \code{integer}.}
#'
#' \item{numeric_parameter}{A parameter that is a \code{double}.}
#'
#' \item{binary_parameter}{A parameter that is restricted to \code{integer}
#'   values of zero or one.}
#' }
#'
#' @return \code{\link{ScalarParameter}} object.
#'
#' @name scalar_parameters
NULL

#' @rdname scalar_parameters
proportion_parameter <- function(name, value) {
  assertthat::assert_that(assertthat::is.string(name), is.finite(value),
    assertthat::is.scalar(value), isTRUE(value>=0), isTRUE(value<=1))
  pproto('ProportionParameter', ScalarParameter, id=new_id(), name=name, 
    value=as.double(value), default=as.double(value), class='numeric',
    lower_limit=0.0, upper_limit=1.0, widget='shiny::sliderInput')
}

#' @rdname scalar_parameters
integer_parameter <- function(name, value, 
                              lower_limit=as.integer(-.Machine$integer.max),
                              upper_limit=as.integer(.Machine$integer.max)) {
  assertthat::assert_that(assertthat::is.string(name), is.finite(value),
    assertthat::is.count(abs(value)))
  pproto('IntegerParameter', ScalarParameter, id=new_id(), name=name,
    value=as.integer(value), default=as.integer(value), class='integer',
    lower_limit=lower_limit, upper_limit=upper_limit,
    widget='shiny::numericInput')
} 

#' @rdname scalar_parameters
numeric_parameter <- function(name, value,
                              lower_limit=.Machine$double.xmin,
                              upper_limit=.Machine$double.xmax) {
  assertthat::assert_that(assertthat::is.string(name),
    assertthat::is.scalar(value), is.finite(value))
  pproto('NumericParameter', ScalarParameter, id=new_id(), name=name,
    value=as.double(value), default=as.double(value), class='numeric',
    lower_limit=lower_limit, upper_limit=upper_limit,
    widget='shiny::numericInput')
}

#' @rdname scalar_parameters
binary_parameter <- function(name, value) {
  assertthat::assert_that(assertthat::is.string(name),
    isTRUE(value<=1), isTRUE(value>=0), is.finite(value),
    assertthat::is.count(value))
  pproto('BinaryParameter', ScalarParameter, id=new_id(), name=name,
    value=as.integer(value), default=as.integer(value), class='integer',
    lower_limit=0L, upper_limit=1L, widget='shiny::checkboxInput')
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
#' @param name \code{character} name of parameter.
#'
#' @param value \code{vector} of values.
#'
#' @param label \code{character} \code{vector} of labels for each value.
#' 
#' @details Below is a list of parameter generating functions and a brief 
#'   description of each.
#'
#' \describe{
#'
#'   \item{proportion_parameter_array}{array of \code{numeric} values that 
#'     between zero and one.}
#'
#'   \item{numeric_parameter_array}{array of \code{numeric} values.}
#'
#'   \item{binary_parameter_array}{array of \code{integer} values
#'     that can only be zero or one.}
#' }
#'
#' @return \code{\link{ArrayParameter}} object.
#'
#' @name array_parameters
NULL

#' @rdname array_parameters
proportion_parameter_array <- function(name, value, label) {
  assertthat::assert_that(assertthat::is.string(name),
    inherits(value, 'numeric'),
    isTRUE(all(value >= 0)), isTRUE(all(value <= 1)), assertthat::noNA(value),
    all(is.finite(value)), inherits(label, 'character'),
    assertthat::noNA(label), length(value) == length(label))
  pproto('ProportionParameterArray', ArrayParameter, id=new_id(),
    name = name, value=as.double(value),
    label=label, class='numeric', default=as.double(value), 
    lower_limit=rep(0.0, length(value)), upper_limit=rep(1.0, length(value)),
    length=length(value), widget='rhandsontable::rHandsontableOutput')
}

#' @rdname array_parameters
numeric_parameter_array <- function(name, value, label,
                                    lower_limit=rep(.Machine$double.xmin,
                                      length(value)),
                                    upper_limit=rep(.Machine$double.xmax,
                                      length(value))) {
  assertthat::assert_that(assertthat::is.string(name),
    inherits(value, 'numeric'),assertthat::noNA(value), all(is.finite(value)),
    inherits(label, 'character'), assertthat::noNA(label),
    length(value) == length(label))
  pproto('NumericParameterArray', ArrayParameter, id=new_id(),
    name = name, value=as.double(value),
    label=label, class='numeric', lower_limit=lower_limit, 
    upper_limit=upper_limit, default=as.double(value), length=length(value),
    widget='rhandsontable::rHandsontableOutput')
}

#' @rdname array_parameters
binary_parameter_array <- function(name, value, label) {
  assertthat::assert_that(assertthat::is.string(name),
    inherits(value, 'numeric'), isTRUE(all(value==round(value))),
    isTRUE(all(value >= 0)),isTRUE(all(value <= 1)), 
    assertthat::noNA(value), all(is.finite(value)),
    inherits(label, 'character'), assertthat::noNA(label),
    length(value) == length(label))
  pproto('BinaryParameterArray', ArrayParameter, id=new_id(),
    name = name, value=as.integer(value),
    label=label, class='integer', lower_limit=rep(0L, length(value)), 
    upper_limit=rep(1L, length(value)),
    default=as.integer(value), length=length(value),
    widget='rhandsontable::rHandsontableOutput')
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
parameters <- function(...) {
  args <- list(...)
  assertthat::assert_that(isTRUE(all(sapply(args, inherits, 'Parameter'))))
  pproto(NULL, Parameters, parameters=args)
} 
