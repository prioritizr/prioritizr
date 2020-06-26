#' @include internal.R
NULL

#' @export
if (!methods::isClass("Id")) methods::setOldClass("Id")
NULL

#' Identifier
#'
#' Generate a new unique identifier.
#'
#' @details Identifiers are made using the \code{\link[uuid]{UUIDgenerate}}.
#'
#' @return `Id` object.
#'
#' @seealso \code{\link[uuid]{UUIDgenerate}}.
#'
#' @examples
#' # create new id
#' i <- new_id()
#'
#' # print id
#' print(i)
#'
#' # convert to character
#' as.character(i)
#'
#' # check if it is an Id object
#' is.Id(i)
#'
#' @aliases Id
#'
#' @export
new_id <- function() {
  x <- uuid::UUIDgenerate()
  class(x) <- c("Id", class(x))
  x
}

#' @rdname as
#' @export
as.Id <- function(x, ...) UseMethod("as.Id")

#' @rdname as
#' @export
as.Id.character <- function(x, ...) {
  class(x) <- c("Id", class(x))
  x
}

#' @rdname is
#' @export
is.Id <- function(x) inherits(x, "Id")

#' @rdname is
#' @export
is.Waiver <- function(x) inherits(x, "Waiver")
