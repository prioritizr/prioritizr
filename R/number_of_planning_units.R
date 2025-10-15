#' @include internal.R
NULL

#' Number of planning units
#'
#' Extract the number of planning units in an object.
#'
#' @param x [problem()], [multi_problem()] or [optimization_problem()] object.
#'
#' @param ... not used.
#'
#' @details
#' The planning units for an object corresponds to the number
#' of entries (e.g., rows, cells) for the planning unit data that
#' do not have missing (`NA`) values for every zone.
#' For example, a single-layer raster dataset might have 90 cells
#' and only two of these cells contain non-missing (`NA`) values.
#' As such, this dataset would have two planning units.
#'
#' @return An `integer` number of planning units.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create problem
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions()
#'
#' # print number of planning units
#' print(number_of_planning_units(p))
#'
#' # create multi-objective problem
#' # TODO
#' }
#' @export
number_of_planning_units <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  UseMethod("number_of_planning_units")
}

#' @rdname number_of_planning_units
#'
#' @export
number_of_planning_units.ConservationProblem <- function(x, ...) {
  x$number_of_planning_units()
}

#' @rdname number_of_planning_units
#'
#' @export
number_of_planning_units.MultiObjConservationProblem <- function(x, ...) {
  x$number_of_planning_units()
}

#' @rdname number_of_planning_units
#'
#' @export
number_of_planning_units.OptimizationProblem <- function(x, ...) {
  x$number_of_planning_units()
}
