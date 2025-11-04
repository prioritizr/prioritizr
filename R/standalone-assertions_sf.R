# ---
# repo: prioritizr/prioritizr
# file: standalone-assertions_sf.R
# imports: [assertthat (>= 0.2.0), sf (>= 1.0-12)]
# ---

#' Is valid geometries?
#'
#' Check an [sf::st_sf()] object has valid geometries?
#'
#' @param x [sf::st_sf()] object.
#'
#' @param call Caller environment.
#'
#' @details
#' Specifically, `GEOMETRYCOLLECTION` geometries are not considered valid.
#'
#' @return A `logical` value.
#'
#' @noRd
is_valid_geometries <- function(x) {
  assert(inherits(x, "sf"), .internal = TRUE)
  !any(grepl("GEOMETRYCOLLECTION", st_geometry_classes(x), fixed = TRUE))
}

assertthat::on_failure(is_valid_geometries) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x),
    "} must not contain {.cls GEOMETRYCOLLECTION} geometries."
  )
}
