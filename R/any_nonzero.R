#' @include internal.R
NULL

#' Any non-zero?
#'
#' Check if an object has any non-zero values?
#'
#' @param object.
#'
#' @return A `logical` value.
#'
#' @noRd
any_nonzero <- function(x) UseMethod("any_nonzero")

assertthat::on_failure(any_nonzero) <- function(call, env) {
  paste0("{.arg ", deparse(call$x), "} must not have only zero values.")
}

any_nonzero.default <- function(x) {
  stop("{.arg x} is not a recognized class.")
}

.S3method("any_nonzero", "default", any_nonzero.default)

any_nonzero.numeric <- function(x) {
  if (all(!is.finite(x))) return(TRUE)
  isTRUE(max(abs(x), na.rm = TRUE) > 1e-6)
}

.S3method("any_nonzero", "numeric", any_nonzero.numeric)

any_nonzero.Matrix <- function(x) {
  any_nonzero(x@x)
}

.S3method("any_nonzero", "Matrix", any_nonzero.Matrix)

any_nonzero.matrix <- function(x) {
  any_nonzero(c(x))
}

.S3method("any_nonzero", "matrix", any_nonzero.matrix)

any_nonzero.Raster <- function(x) {
  any_nonzero(terra::rast(x))
}

.S3method("any_nonzero", "Raster", any_nonzero.Raster)

any_nonzero.SpatRaster <- function(x) {
  x <- terra::global(abs(x), "max", na.rm = TRUE)[[1]]
  if (all(!is.finite(x))) return(TRUE)
  all(x > 1e-6, na.rm = TRUE)
}

.S3method("any_nonzero", "SpatRaster", any_nonzero.SpatRaster)

any_nonzero.ZonesRaster <- function(x) {
  any_nonzero(terra::rast(raster::stack(raster::as.list(x))))
}

.S3method("any_nonzero", "ZonesRaster", any_nonzero.ZonesRaster)

any_nonzero.ZonesSpatRaster <- function(x) {
  any_nonzero(terra::rast(terra::as.list(x)))
}

.S3method("any_nonzero", "ZonesSpatRaster", any_nonzero.ZonesSpatRaster)

any_nonzero.data.frame <- function(x) {
  all(vapply(x, any_nonzero, logical(1)))
}

.S3method("any_nonzero", "data.frame", any_nonzero.data.frame)

any_nonzero.Spatial <- function(x) {
  all(vapply(x@data, any_nonzero, logical(1)))
}

.S3method("any_nonzero", "Spatial", any_nonzero.Spatial)

any_nonzero.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), any_nonzero, logical(1)))
}

.S3method("any_nonzero", "sf", any_nonzero.sf)
