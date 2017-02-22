#' @include internal.R
#' @export
methods::setOldClass('Id')

#' Identifier
#'
#' Generate a new unique identifier.
#'
#' @details Identifiers are made using the \code{\link[uuid]{UUIDgenerate}}.
#'
#' @return \code{Id} object.
#' 
#' @seealso \code{\link[uuid]{UUIDgenerate}}.
#'
#' @aliases Id
#'
#' @export
new_id <- function() {
  x <- uuid::UUIDgenerate()
  class(x) <- c('Id', class(x))
  x
}
 
#' @rdname as
#' @export
as.id <- function(x, ...) UseMethod('as.Id')

#' @rdname as
#' @export
as.Id.character <- function(x, ...) {
  class(x) <- c('Id', class(x))
  x
}

#' @rdname is
#' @export
is.Id <- function(x) inherits(x, 'Id')

#' @rdname is
#' @export
is.Waiver <- function(x) inherits(x, 'Waiver')


