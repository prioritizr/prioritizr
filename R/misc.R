#' @include internal.R ScalarParameter-proto.R ArrayParameter-proto.R Solver-proto.R ConservationProblem-proto.R OptimizationProblem-proto.R Id.R
NULL

#' Show
#'
#' Show an object.
#'
#' @param object An object.
#' 
#' @return An object.
#' @name show
NULL

#' @name show
#' @rdname show
methods::setMethod('show', 'ConservationModifier',
                   function(object) object$show())

#' @name show
#' @rdname show
methods::setMethod('show', 'ConservationProblem',
                   function(object) object$show())

#' @name show
#' @rdname show
methods::setMethod('show', 'Id',
                   function(object) print(object))

#' @name show
#' @rdname show
methods::setMethod('show', 'OptimizationProblem',
                   function(object) object$show())

#' @name show
#' @rdname show
methods::setMethod('show', 'Parameter',
                   function(object) object$show())

#' @name show
#' @rdname show
methods::setMethod('show', 'Solver', function(object) object$show())

#' Print
#'
#' Print an object.
#'
#' @param x An object.
#'
#' @param ... not used.
#' 
#' @return An object.
#'
#' @name print
NULL

#' @rdname print
#' @export
print.ConservationModifier <- function(x, ...) x$print()

#' @rdname print
#' @export
print.ConservationProblem <- function(x, ...) x$print()

#' @rdname print
#' @export
print.id <- function(x, ...) message('id: ', x)

#' @rdname print
#' @export
print.OptimizationProblem <- function(x, ...) x$print()

#' @rdname print
#' @export
print.ScalarParameter <- function(x, ...) x$print()

#' @rdname print
#' @export
print.ArrayParameter <- function(x, ...) x$print()

#' @rdname print
#' @export
print.Solver <- function(x, ...) x$print()

#' Coerce object to another object
#'
#' Coerce an object.
#'
#' @param x Object.
#'
#' @param ... unused arguments.
#'
#' @return An Object.
#' 
#' @name as
NULL

#' @rdname as
#' @export
as.list.Parameters <- function(x, ...)
  structure(lapply(x$parameters, function(x) x$value),
            .Names=sapply(x$parameters, function(x) x$name),
            id=sapply(x$parameters, function(x) x$id))

#' Is it?
#'
#' Test if an object inherits from a class.
#'
#' @param x Object.
#'
#' @return \code{logical} indicatig if it inherits from the class.
#' 
#' @name is
NULL
