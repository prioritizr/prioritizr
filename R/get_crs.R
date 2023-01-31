#' @include internal.R
NULL

#' Get coordinate reference system
#'
#' Extract the coordinate reference system from an object.
#'
#' @param x [sf::st_sf()], [terra::rast()], [Spatial-class], or
#'   [raster::raster()] object.
#'
#' @return A [sf::st_crs()] object.
#'
#' @noRd
NULL

get_crs <- function(x) {
  rlang::check_required(x)
  UseMethod("get_crs")
}

get_crs.sf <- function(x) sf::st_crs(x)

.S3method("get_crs", "sf", get_crs.sf)

get_crs.Spatial <- function(x) sf::st_crs(x@proj4string)

.S3method("get_crs", "Spatial", get_crs.Spatial)

get_crs.Raster <- function(x) sf::st_crs(x@crs)

.S3method("get_crs", "Raster", get_crs.Raster)

get_crs.ZonesRaster <- function(x) get_crs(x[[1]])

.S3method("get_crs", "ZonesRaster", get_crs.ZonesRaster)

get_crs.SpatRaster <- function(x) {
  x_crs <- terra::crs(x)
  if (nzchar(x_crs)) {
    return(sf::st_crs(x_crs))
  } else {
    return(sf::st_crs(NA))
  }
}

.S3method("get_crs", "SpatRaster", get_crs.SpatRaster)

get_crs.ZonesSpatRaster <- function(x) get_crs(x[[1]])

.S3method("get_crs", "ZonesSpatRaster", get_crs.ZonesSpatRaster)

na_crs <- "ENGCRS[\"Undefined Cartesian SRS\",\n    EDATUM[\"\"],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"
