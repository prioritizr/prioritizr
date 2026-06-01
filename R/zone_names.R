#' @include internal.R
NULL

#' Zone names
#'
#' Extract the names of zones in an object.
#'
#' @inheritParams feature_names
#'
#' @return A `character` vector of zone names.
#'
#' @examplesIf prioritizr::do_run_example()
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
#'
#' # create two example problems
#' p1 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions()
#'
#' p2 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions()
#'
#' # create multi-objective problem
#' mp <-
#'   multi_problem(p1, p2) %>%
#'   add_hier_approach(rel_tol = 0.1, verbose = FALSE) %>%
#'   add_gurobi_solver(gap = 0, verbose = FALSE)
#'
#' # print zone names
#' print(zone_names(mp))
#'
#' @export
zone_names <- function(x, ...) {
  assert_required(x)
  UseMethod("zone_names")
}

#' @rdname zone_names
#'
#' @export
zone_names.ConservationProblem <- function(x, ...) {
  rlang::check_dots_empty()
  x$zone_names()
}

#' @rdname zone_names
#'
#' @export
zone_names.MultiObjConservationProblem <- function(x, ...) {
  rlang::check_dots_empty()
  x$zone_names()
}

#' @rdname zone_names
#'
#' @export
zone_names.ZonesRaster <- function(x, ...) {
  rlang::check_dots_empty()
  attr(x, "zone_names")
}

#' @rdname zone_names
#'
#' @export
zone_names.ZonesSpatRaster <- function(x, ...) {
  rlang::check_dots_empty()
  attr(x, "zone_names")
}

#' @rdname zone_names
#'
#' @export
zone_names.ZonesCharacter <- function(x, ...) {
  rlang::check_dots_empty()
  attr(x, "zone_names")
}
