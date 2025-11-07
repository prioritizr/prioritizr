# ---
# repo: prioritizr/prioritizr
# file: standalone-assertions_sf.R
# dependencies: [standalone-assertions_handlers.R]
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

#' Geometry classes
#'
#' Extract geometry class names from a [sf::sf()] object.
#'
#' @param x [sf::sf()] object.
#'
#' @return A `character` vector.
#'
#' @noRd
st_geometry_classes <- function(x) {
  assert(inherits(x, "sf"))
  if (inherits(sf::st_geometry(x), "sfc_POLYGON")) {
    return(rep("POLYGON", nrow(x)))
  } else if (inherits(sf::st_geometry(x), "sfc_POINT")) {
    return(rep("POINT", nrow(x)))
  } else if (inherits(sf::st_geometry(x), "sfc_MULTIPOLYGON")) {
    return(rep("MULTIPOLYGON", nrow(x)))
  } else if (inherits(sf::st_geometry(x), "sfc_MULTIPOINT")) {
    return(rep("MULTIPOINT", nrow(x)))
  } else if (inherits(sf::st_geometry(x), "sfc_LINESTRING")) {
    return(rep("LINESTRING", nrow(x)))
  } else if (inherits(sf::st_geometry(x), "sfc_MULTILINESTRING")) {
    return(rep("MULTILINESTRING", nrow(x)))
  } else {
    vapply(sf::st_geometry(x), function(y) class(y)[[2]], character(1))
  }
}
