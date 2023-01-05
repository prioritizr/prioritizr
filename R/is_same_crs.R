#' @include internal.R
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
  assertthat::assert_that(
    is_inherits(x, c("sf", "Spatial", "SpatRaster", "Raster")),
    is_inherits(y, c("sf", "Spatial", "SpatRaster", "Raster"))
  )
  # prepare x CRS
  if (inherits(x, "sf")) {
    x_crs <- sf::st_crs(x)
  } else if (inherits(x, "Spatial")) {
    x_crs <- sf::st_crs(x@proj4string)
  } else if (inherits(x, "Raster")) {
    x_crs <- sf::st_crs(x@crs)
  } else if (inherits(x, "SpatRaster")) {
    x_crs <- terra::crs(x)
    if (nzchar(x_crs)) {
      x_crs <- sf::st_crs(x_crs)
    } else {
      x_crs <- sf::st_crs(NA)
    }
  } else {
    stop("argument to x not recognized")
  }
  # prepare y CRS
  if (inherits(y, "sf")) {
    y_crs <- sf::st_crs(y)
  } else if (inherits(y, "Spatial")) {
    y_crs <- sf::st_crs(y@proj4string)
  } else if (inherits(y, "Raster")) {
    y_crs <- sf::st_crs(y@crs)
  } else if (inherits(y, "SpatRaster")) {
    y_crs <- terra::crs(y)
    if (nzchar(y_crs)) {
      y_crs <- sf::st_crs(y_crs)
    } else {
      y_crs <- sf::st_crs(NA)
    }
  } else {
    stop("argument to y not recognized")
  }
  # run checks
  isTRUE(x_crs == y_crs)
}

assertthat::on_failure(is_same_crs) <- function(call, env) {
  paste(
    deparse(call$x), "and", deparse(call$y),
    "have different coordinate reference systems",
  )
}
