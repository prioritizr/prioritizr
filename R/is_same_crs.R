#' @include internal.R get_crs.R
NULL

#' Is same coordinate reference system?
#'
#' Check if two spatial objects have the same coordinate reference system.
#'
#' @param x [terra::rast()] or [sf::st_sf()] object.
#'
#' @param y [terra::rast()] or [sf::st_sf()] object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_same_crs <- function(x, y) {
  # assert valid arguments
  assert(
    is_inherits(
      x,
      c(
        "sf", "Spatial", "SpatRaster", "Raster",
        "ZonesRaster", "ZonesSpatRaster"
      )
    ),
    is_inherits(
      y,
      c(
        "sf", "Spatial", "SpatRaster", "Raster",
        "ZonesRaster", "ZonesSpatRaster"
      )
    )
  )
  # extract crs data
  x_crs <- get_crs(x)
  y_crs <- get_crs(y)
  # account for different ways of handling NA crs
  if (x_crs == sf::st_crs(NA)) x_crs <- sf::st_crs(na_crs)
  if (y_crs == sf::st_crs(NA)) y_crs <- sf::st_crs(na_crs)
  # run checks
  isTRUE(x_crs == y_crs)
}

assertthat::on_failure(is_same_crs) <- function(call, env) {
  paste(
    deparse(call$x), "and", deparse(call$y),
    "have different coordinate reference systems"
  )
}
