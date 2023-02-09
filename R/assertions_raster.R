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
#' This function checks if the planning units in a `problem()` object
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

#' Is single patch?
#'
#' Check if all pixels with non-zero values in a raster form a single patch?
#'
#' @param x [terra::rast()] object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
is_single_patch_raster <- function(x) {
  assert(
    inherits(x, "SpatRaster"),
    terra::nlyr(x) == 1,
    .internal = TRUE
  )
  x <- terra::patches(x, zeroAsNA = TRUE)
  isTRUE(terra::global(x, "max", na.rm = TRUE)[[1]] == 1)
}

#' Is checkerboard?
#'
#' Check if all pixels with non-zero values in a raster do not neighbor with
#' each other?
#'
#' @param x [terra::rast()] object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
is_checkerboard_raster <- function(x) {
  assert(
    inherits(x, "SpatRaster"),
    terra::nlyr(x) == 1,
    .internal = TRUE
  )
   a <- terra::adjacent(x, cells = terra::cells(x == 0, 0)[[1]], pairs = FALSE)
   all(x[stats::na.omit(c(a))][[1]] %in% c(0, NA))
}

#' Is distinct zones?
#'

#' Check if all pixels with the same (non-zero) values values form distinct
#' patches?
#'
#' @param x [terra::rast()] object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
is_distinct_zones_raster <- function(x) {
  assert(
    inherits(x, "SpatRaster"),
    terra::nlyr(x) == 1,
    .internal = TRUE
  )
  p <- terra::patches(x, zeroAsNA = TRUE)
  ids <- unique(terra::values(p)[, 1])
  ids <- ids[is.finite(ids)]
  r <- vapply(ids, FUN.VALUE = logical(1), function(i) {
    length(unique(c(x[p == i]))) == 1
  })
  all(r)
}
