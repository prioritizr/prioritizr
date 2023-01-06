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
  assertthat::assert_that(inherits(x, "sf"))
  vapply(sf::st_geometry(x), class, character(3))[2, ]
}
