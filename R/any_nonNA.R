#' @include internal.R
NULL

#' Any non-NA?
#'
#' Check if an object has any non-missing (`NA`) values?
#'
#' @param object.
#'
#' @param names `character` vector of column names.
#'
#' @return A `logical` value.
#'
#' @noRd
any_nonNA <- function(x) UseMethod("any_nonNA")

assertthat::on_failure(any_nonNA) <- function(call, env) {
  paste(deparse(call$x), "only has missing (NA) values")
}

any_nonNA.default <- function(x, what) {
  stop("argument to x is not a recognized class")
}

.S3method("any_nonNA", "default", any_nonNA.default)

any_nonNA.numeric <- function(x) {
  any(!is.na(x))
}

.S3method("any_nonNA", "numeric", any_nonNA.numeric)

any_nonNA.logical <- function(x) {
  any(!is.na(x))
}

.S3method("any_nonNA", "logical", any_nonNA.logical)

any_nonNA.character <- function(x) {
  any(!is.na(x))
}

.S3method("any_nonNA", "character", any_nonNA.character)

any_nonNA.factor <- function(x) {
  any(!is.na(x))
}

.S3method("any_nonNA", "factor", any_nonNA.factor)

any_nonNA.Matrix <- function(x) {
  any(!is.na(x@x))
}

.S3method("any_nonNA", "Matrix", any_nonNA.Matrix)

any_nonNA.matrix <- function(x) {
  any(!is.na(c(x)))
}

.S3method("any_nonNA", "matrix", any_nonNA.matrix)

any_nonNA.data.frame <- function(x) {
  all(vapply(x, any_nonNA, logical(1)))
}

.S3method("any_nonNA", "data.frame", any_nonNA.data.frame)

any_nonNA.Spatial <- function(x) {
  all(vapply(x@data, any_nonNA, logical(1)))
}

.S3method("any_nonNA", "Spatial", any_nonNA.Spatial)

any_nonNA.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), any_nonNA, logical(1)))
}

.S3method("any_nonNA", "sf", any_nonNA.sf)

any_nonNA.Raster <- function(x) {
  any_nonNA(terra::rast(x))
}

.S3method("any_nonNA", "Raster", any_nonNA.Raster)

any_nonNA.SpatRaster <- function(x) {
  all(terra::global(x, "notNA")[[1]] > 0)
}

.S3method("any_nonNA", "SpatRaster", any_nonNA.SpatRaster)

any_nonNA.ZonesRaster <- function(x) {
  any_nonNA(terra::rast(raster::as.list(x)))
}

.S3method("any_nonNA", "ZonesRaster", any_nonNA.ZonesRaster)

any_nonNA.ZonesSpatRaster <- function(x) {
  any_nonNA(terra::rast(terra::as.list(x)))
}

.S3method("any_nonNA", "ZonesSpatRaster", any_nonNA.ZonesSpatRaster)
