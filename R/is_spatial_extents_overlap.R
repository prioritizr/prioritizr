#' @include internal.R
NULL

#' Do extents intersect?
#'
#' Check if the spatial extents of two objects intersect or not.
#'
#' @param x [terra::rast()] or [sf::sf()] object.
#'
#' @param y [terra::rast()] or [sf::sf()] object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_spatial_extents_overlap <- function(x, y) {
  # initial checks
  assert(
    is_inherits(x, c(
      "SpatRaster", "Raster", "Spatial", "sf",
      "ZonesRaster", "ZonesSpatRaster"
    )),
    is_inherits(y, c(
      "SpatRaster", "Raster", "Spatial", "sf",
      "ZonesRaster", "ZonesSpatRaster"
    ))
  )
  # if needed, convert data
  if (inherits(x, c("ZonesRaster", "ZonesSpatRaster"))) x <- x[[1]]
  if (inherits(y, c("ZonesRaster", "ZonesSpatRaster"))) y <- y[[1]]
  if (inherits(x, "Raster")) x <- terra::rast(x)
  if (inherits(y, "Raster")) y <- terra::rast(y)
  if (inherits(x, "Spatial")) x <- terra::vect(sf::st_as_sf(x))
  if (inherits(y, "Spatial")) y <- terra::vect(sf::st_as_sf(y))
  # if sf object, then check for empty geometries
  if (inherits(x, "sf")) {
    ## note that we try to avoid calling sf::st_is_empty on all geometries,
    ## because it can be computationally expensive
    # nocov start
    if (isTRUE(sf::st_is_empty(sf::st_geometry(x)[1]))) {
      assert(
        !all(sf::st_is_empty(sf::st_geometry(x))),
        msg = "{.arg x} must contain at least one non-empty geometry."
      )
    }
    # nocov end
  }
  if (inherits(y, "sf")) {
    ## note that we try to avoid calling sf::st_is_empty on all geometries,
    ## because it can be computationally expensive
    # nocov start
    if (isTRUE(sf::st_is_empty(sf::st_geometry(y)[1]))) {
      assert(
        !all(sf::st_is_empty(sf::st_geometry(y))),
        msg = "{.arg y} must contain at least one non-empty geometry."
      )
    }
    # nocov end
  }
  # find overlapping indices
  x <- sf::st_as_sf(terra::as.polygons(terra::ext(x)))
  y <- sf::st_as_sf(terra::as.polygons(terra::ext(y)))
  isTRUE(
    sf::st_intersects(x, y, sparse = FALSE)[[1]]
  )
}

assertthat::on_failure(is_spatial_extents_overlap) <- function(call, env) {
  paste(
    "{.arg ", deparse(call$x),
    "} and {.arg ", deparse(call$y),
    "} must have overlapping spatial extents."
  )
}
