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
#' @return An object.
#'
#' @name as
NULL

#' @rdname as
#' @method as.list Parameters
#' @export
as.list.Parameters <- function(x, ...) {
  structure(
    lapply(x$ids(), function(i) x[[i]]$value),
    .Names = x$names(),
    id = x$ids()
  )
}

#' @rdname as
#' @method as.list Zones
#' @export
as.list.Zones <- function(x, ...) {
  attributes(x) <- NULL
  class(x) <- "list"
  x
}


#' @rdname as
#' @export
as.ZonesSpatRaster <- function(x) UseMethod("as.ZonesSpatRaster")

#' @rdname as
#' @method as.ZonesSpatRaster ZonesRaster
#' @export
as.ZonesSpatRaster.ZonesRaster <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- terra::rast(x[[i]])
  }
  class(x) <- c("ZonesSpatRaster", "Zones")
  x
}
