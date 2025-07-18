#' @include internal.R
NULL

#' Are rasters comparable?
#'
#' This function checks if two [terra::rast()] objects are comparable.
#'
#' @param x [terra::rast()] or [raster::raster()] object.
#'
#' @param y [terra::rast()] or [raster::raster()] object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value indicating if the
#'   objects have the same
#'   resolution, extent, dimensionality, and coordinate system.
#'
#' @noRd
is_comparable_raster <- function(x, y) {
  # wrapper for ZonesRaster and ZonesSpatRaster objects
  if (inherits(x, c("ZonesRaster", "ZonesSpatRaster"))) x <- x[[1]]
  if (inherits(y, c("ZonesRaster", "ZonesSpatRaster"))) y <- y[[1]]
  # wrapper for Raster objects
  if (inherits(x, "Raster")) x <- terra::rast(x)
  if (inherits(y, "Raster")) y <- terra::rast(y)
  # assert valid arguments
  assert(
    inherits(x, "SpatRaster"),
    inherits(y, "SpatRaster"),
    .internal = TRUE
  )
  # run checks
  is_same_crs(x, y) &&
    terra::compareGeom(
      x[[1]], y[[1]],
      crs = FALSE, res = TRUE, stopOnError = FALSE
    )
}

assertthat::on_failure(is_comparable_raster) <- function(call, env) {
  c(
    paste0(
      "{.arg ", deparse(call$x), "} must be comparable with ",
      "{.arg ", deparse(call$y),  "}."
    ),
    "x" = paste(
      "They do not have the same spatial resolution, extent,",
      "coordinate reference system, and dimensionality (rows / columns)."
    )
  )
}

#' Are planning unit rasters comparable?
#'
#' This function checks if the planning units in a [problem()] object
#' is comparable with a raster.
#'
#' @param x [problem()] object.
#'
#' @param y [terra::rast()] or [raster::raster()] object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value indicating if the
#'   objects have the same
#'   resolution, extent, dimensionality, and coordinate system.
#'
#' @noRd
is_pu_comparable_raster <- function(x, y) {
  assert(is_conservation_problem(x), .internal = TRUE)
  is_comparable_raster(x$data$cost, y)
}

assertthat::on_failure(is_pu_comparable_raster) <- function(call, env) {
  c(
    paste0(
      "{.arg ", deparse(call$x), "} must have planning units that are ",
      "comparable with {.arg ", deparse(call$y),  "}."
    ),
    "x" = paste(
      "They do not have the same spatial resolution, extent,",
      "coordinate reference system, and dimensionality (rows / columns)."
    )
  )
}
