#' @include internal.R
NULL

#' Number of zones
#'
#' Extract the number of zones in an object.
#'
#' @param x [problem()], [optimization_problem()], or [zones()] object.
#'
#' @param ... not used.
#'
#' @return An `integer` number of zones.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # print number of zones in a Zones object
#' print(number_of_zones(sim_zones_features))
#
#' # create problem with multiple zones
#' p <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions()
#'
#' # print number of zones in the problem
#' print(number_of_zones(p))
#' }
#' @export
number_of_zones <- function(x, ...) UseMethod("number_of_zones")

#' @rdname number_of_zones
#'
#' @export
number_of_zones.ConservationProblem <- function(x, ...) {
  rlang::check_required(x)
  rlang::check_dots_empty()
  x$number_of_zones()
}

#' @rdname number_of_zones
#'
#' @export
number_of_zones.OptimizationProblem <- function(x, ...) {
  rlang::check_required(x)
  rlang::check_dots_empty()
  x$number_of_zones()
}

#' @rdname number_of_zones
#'
#' @export
number_of_zones.ZonesRaster <- function(x, ...) {
  rlang::check_required(x)
  rlang::check_dots_empty()
  length(x)
}

#' @rdname number_of_zones
#'
#' @export
number_of_zones.ZonesSpatRaster <- function(x, ...) {
  rlang::check_required(x)
  rlang::check_dots_empty()
  length(x)
}

#' @rdname number_of_zones
#'
#' @export
number_of_zones.ZonesCharacter <- function(x, ...) {
  rlang::check_required(x)
  rlang::check_dots_empty()
  length(x)
}
