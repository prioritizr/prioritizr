#' @include internal.R
NULL

#' Number of planning units
#'
#' Extract the number of planning units in an object.
#'
#' @param x [ConservationProblem-class] or
#'   [OptimizationProblem-class] object.
#'
#' @return `integer` number of planning units.
#'
#' @name number_of_planning_units
#'
#' @aliases number_of_planning_units,ConservationProblem-method number_of_planning_units,OptimizationProblem-method
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_min_set_objective() %>%
#'      add_relative_targets(0.2) %>%
#'      add_binary_decisions()
#'
#' # print number of planning units
#' print(number_of_planning_units(p))
NULL

#' @name number_of_planning_units
#'
#' @rdname number_of_planning_units
#'
#' @exportMethod number_of_planning_units
#'
#' @usage number_of_planning_units(x)
#'
methods::setGeneric("number_of_planning_units",
  function(x) standardGeneric("number_of_planning_units"))

#' @name number_of_planning_units
#'
#' @rdname number_of_planning_units
#'
#' @usage \S4method{number_of_planning_units}{ConservationProblem}(x)
#'
methods::setMethod("number_of_planning_units", "ConservationProblem",
  function(x) x$number_of_planning_units())

#' @name number_of_planning_units
#'
#' @rdname number_of_planning_units
#'
#' @usage \S4method{number_of_planning_units}{OptimizationProblem}(x)
#'
methods::setMethod("number_of_planning_units", "OptimizationProblem",
  function(x) x$number_of_planning_units())
