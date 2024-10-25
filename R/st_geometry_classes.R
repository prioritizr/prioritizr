#' @include internal.R reexports.R
NULL

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
