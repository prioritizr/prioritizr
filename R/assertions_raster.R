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
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    inherits(y, "SpatRaster")
  )
  # run checks
  is_same_crs(x, y) &&
    terra::compareGeom(
      x[[1]], y[[1]],
      crs = FALSE, res = TRUE, stopOnError = FALSE
    )
}

assertthat::on_failure(is_comparable_raster) <- function(call, env) {
  paste0(
    deparse(call$x), " and ", deparse(call$y),  " are not comparable; ",
    "they have different spatial resolutions, extents, ",
    "coordinate reference systems, or dimensionality (rows / columns)"
  )
}

#' Is single patch?
#'
#' Check if all pixels with non-zero values in a raster form a single patch?
#'
#' @param x [terra::rast()] object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_single_patch_raster <- function(x) {
  assertthat::assert_that(
    is(x, "SpatRaster"),
    terra::nlyr(x) == 1
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
#' @return A `logical` value.
#'
#' @noRd
is_checkerboard_raster <- function(x) {
  assertthat::assert_that(
    is(x, "SpatRaster"),
    terra::nlyr(x) == 1
  )
   a <- terra::adjacent(x, cells = terra::cells(x == 0, 0)[[1]], pairs = FALSE)
   all(x[na.omit(c(a))][[1]] %in% c(0, NA))
}

#' Is distinct zones?
#'

#' Check if all pixels with the same (non-zero) values values form distinct
#' patches?
#'
#' @param x [terra::rast()] object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_distinct_zones_raster <- function(x) {
  assertthat::assert_that(
    is(x, "SpatRaster"),
    terra::nlyr(x) == 1
  )
  p <- terra::patches(x, zeroAsNA = TRUE)
  ids <- unique(terra::values(p)[, 1])
  ids <- ids[is.finite(ids)]
  r <- vapply(ids, FUN.VALUE = logical(1), function(i) {
    length(unique(c(x[p == i]))) == 1
  })
  all(r)
}
