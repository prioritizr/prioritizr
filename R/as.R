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
#' @method as.list Zones
#' @export
as.list.Zones <- function(x, ...) {
  attributes(x) <- NULL
  class(x) <- "list"
  x
}

as.ZonesSpatRaster <- function(x) UseMethod("as.ZonesSpatRaster")

as.ZonesSpatRaster.ZonesRaster <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- terra::rast(x[[i]])
  }
  class(x) <- c("ZonesSpatRaster", "Zones")
  x
}

.S3method("as.ZonesSpatRaster", "ZonesRaster", as.ZonesSpatRaster.ZonesRaster)

as.ZonesRaster <- function(x) UseMethod("as.ZonesRaster")

as.ZonesRaster.ZonesSpatRaster <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- raster::stack(x[[i]])
  }
  class(x) <- c("ZonesRaster", "Zones")
  x
}

.S3method("as.ZonesRaster", "ZonesSpatRaster", as.ZonesRaster.ZonesSpatRaster)
