#' @include internal.R
NULL

#' Is single patch?
#'
#' Check if all geometries form a single patch?
#'
#' @param x [sf::st_sf()] object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
is_single_patch_sf <- function(x) {
  assert(inherits(x, "sf"), .internal = TRUE)
  m <- igraph::graph_from_adjacency_matrix(adjacency_matrix(x))
  isTRUE(igraph::components(m)$no == 1)
}

#' Is valid geometries?
#'
#' Check an [sf::st_sf()] object has valid geometries?
#'
#' @param x [sf::st_sf()] object.
#'
#' @param call Caller environment.
#'
#' @details
#' Specifically, `GEOMETRYCOLLECTION` geometries are not permitted.
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
