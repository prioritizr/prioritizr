#' @import raster
#' @import sp
#' @useDynLib prioritizr
NULL


#' @export
methods::setOldClass('waiver')

#' Waiver
#' 
#' Create a "waiver" object. 
#' 
#' @details This object is used to represent that the user has not manually 
#' specified a setting, and so defaults should be used. By explictly 
#' using a \code{waiver()}, this means that \code{NULL} objects can be a 
#' valid setting. The use of a "waiver"  object was inspired by that in the
#' \emph{ggplot2} package.
#' 
#' @return \code{object} of class "waiver".
#'
#' @noRd
wavier <- structure(NULL, class = "waiver")

#' @export
methods::setOldClass('id')

#' Identifier
#'
#' Generate a new unique identifier.
#'
#' @return \code{id} object.
#' 
#' @aliases id
#'
#' @export
new_id <- function() {
  x <- uuid::UUIDgenerate()
  class(x) <- c('id', class(x))
  x
}
