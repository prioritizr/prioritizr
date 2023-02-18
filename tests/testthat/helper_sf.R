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
