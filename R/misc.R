#' @include internal.R
NULL

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
#' @method as.list Parameters
#' @export
as.list.Parameters <- function(x, ...)
  structure(lapply(x$ids(), function(i) x[[i]]$value),
            .Names = x$names(), id = x$ids())

#' @rdname as
#' @method as.list Zones
#' @export
as.list.Zones <- function(x, ...) {
  attributes(x) <- NULL
  class(x) <- "list"
  x
}

#' Is it?
#'
#' Test if an object inherits from a class.
#'
#' @param x Object.
#'
#' @return \code{logical} indicating if it inherits from the class.
#'
#' @name is
NULL
