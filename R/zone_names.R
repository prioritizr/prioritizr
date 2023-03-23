#' @include internal.R
NULL

#' Zone names
#'
#' Extract the names of zones in an object.
#'
#' @param x [problem()] or [zones()] object.
#'
#' @param ... not used.
#'
#' @return A `character` vector of zone names.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # print names of zones in a Zones object
#' print(zone_names(sim_zones_features))
#
#' # create problem with multiple zones
#' p <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions()
#'
#' # print zone names in problem
#' print(zone_names(p))
#' }
#' @export
zone_names <- function(x, ...) UseMethod("zone_names")

#' @rdname zone_names
#'
#' @export
zone_names.ConservationProblem <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  x$zone_names()
}

#' @rdname zone_names
#'
#' @export
zone_names.ZonesRaster <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  attr(x, "zone_names")
}

#' @rdname zone_names
#'
#' @export
zone_names.ZonesSpatRaster <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  attr(x, "zone_names")
}

#' @rdname zone_names
#'
#' @export
zone_names.ZonesCharacter <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  attr(x, "zone_names")
}
