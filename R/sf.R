#' @include internal.R
NULL

#' @export
if (!methods::isClass("sf")) methods::setOldClass("sf")
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
geometry_classes <- function(x) {
  assertthat::assert_that(inherits(x, "sf"))
  vapply(sf::st_geometry(x), class, character(3))[2, ]
}
