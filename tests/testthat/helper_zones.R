#' Set zones coordinate reference system
#'
#' Set the  coordinate reference system for a `zones()` object.
#'
#' @param x `zones()` object.
#'
#' @param crs [terra::crs()] object.
#'
#' @return A `zones()` object.
set_zones_crs <- function(x, crs) {
  assertthat::assert_that(
    inherits(x, "ZonesSpatRaster"),
    assertthat::is.string(crs)
  )
  do.call(
    zones,
    append(
      lapply(
        as.list(x),
        function(r) {
          terra::crs(r) <- crs
          r
        }
      ),
      list(
        zone_names = zone_names(x),
        feature_names = feature_names(x)
      )
    )
  )
}
