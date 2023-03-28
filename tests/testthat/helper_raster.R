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
