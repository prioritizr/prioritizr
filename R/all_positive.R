#' @include internal.R
NULL

#' All positive?
#'
#' Check if an object has only positive values?
#'
#' @param object.
#'
#' @return A `logical` value.
#'
#' @noRd
all_positive <- function(x) UseMethod("all_positive")

assertthat::on_failure(all_positive) <- function(call, env) {
  paste(deparse(call$x), "has negative values")
}

all_positive.default <- function(x) {
  stop("argument to x is not a recognized class")
}

.S3method("all_positive", "default", all_positive.default)

all_positive.numeric <- function(x) {
  all(x >= 0, na.rm = TRUE)
}

.S3method("all_positive", "numeric", all_positive.numeric)

all_positive.Matrix <- function(x) {
  all_positive(x@x)
}

.S3method("all_positive", "Matrix", all_positive.Matrix)

all_positive.matrix <- function(x) {
  all_positive(c(x))
}

.S3method("all_positive", "matrix", all_positive.matrix)

all_positive.Raster <- function(x) {
  all_positive(terra::rast(x))
}

.S3method("all_positive", "Raster", all_positive.Raster)

all_positive.SpatRaster <- function(x) {
  all_positive(terra::global(x, "min")[[1]])
}

.S3method("all_positive", "SpatRaster", all_positive.SpatRaster)

all_positive.ZonesRaster <- function(x) {
  all_positive(terra::rast(raster::as.list(x)))
}

.S3method("all_positive", "ZonesRaster", all_positive.ZonesRaster)

all_positive.ZonesSpatRaster <- function(x) {
  all_positive(terra::rast(terra::as.list(x)))
}

.S3method("all_positive", "ZonesSpatRaster", all_positive.ZonesSpatRaster)

all_positive.data.frame <- function(x) {
  all(vapply(x, all_positive, logical(1)))
}

.S3method("all_positive", "data.frame", all_positive.data.frame)

all_positive.Spatial <- function(x) {
  all(vapply(x@data, all_positive, logical(1)))
}

.S3method("all_positive", "Spatial", all_positive.Spatial)

all_positive.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), all_positive, logical(1)))
}

.S3method("all_positive", "sf", all_positive.sf)
