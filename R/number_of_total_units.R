#' @include internal.R
NULL

#' Number of total units
#'
#' Extract the number of total units in an object.
#'
#' @param x [problem()] or [multi_problem()] object.
#'
#' @param ... not used.
#'
#' @details
#' The total units for an object corresponds to the total number
#' of entries (e.g., rows, cells) for the planning unit data.
#' For example, a single-layer raster dataset might have 90 cells
#' and only two of these cells contain non-missing (`NA`) values.
#' As such, this dataset would have 90 total units and two planning units.
#'
#' @return An `integer` number of total units.
#'
#' @name number_of_total_units
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create problem with one zone
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions()
#'
#' # print number of planning units
#' print(number_of_planning_units(p1))
#'
#' # print number of total units
#' print(number_of_total_units(p1))
#'
#' # create problem with multiple zones
#' p2 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions()
#'
#' # print number of planning units
#' print(number_of_planning_units(p2))
#'
#' # print number of total units
#' print(number_of_total_units(p2))
#'
#' # create multi-objective problem
#' # TODO
#' }
#' @export
number_of_total_units <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  UseMethod("number_of_total_units")
}

#' @rdname number_of_total_units
#'
#' @export
number_of_total_units.ConservationProblem <- function(x, ...) {
  x$number_of_total_units()
}

#' @rdname number_of_total_units
#'
#' @export
number_of_total_units.MultiObjConservationProblem <- function(x, ...) {
  x$number_of_total_units()
}
