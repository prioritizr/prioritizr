#' @include internal.R
NULL

#' Number of total units
#'
#' Extract the number of total units in an object.
#'
#' @param x [problem()] object.
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
#' @aliases number_of_total_units,ConservationProblem-method
#'
#' @examples
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_pu_zones_raster <- get_sim_zones_pu_raster()
#' sim_features_zones <- get_sim_zones_features()
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
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions()
#'
#' # print number of planning units
#' print(number_of_planning_units(p2))
#'
#' # print number of total units
#' print(number_of_total_units(p2))
NULL

#' @name number_of_total_units
#'
#' @rdname number_of_total_units
#'
#' @exportMethod number_of_total_units
#'
#' @usage number_of_total_units(x)
#'
methods::setGeneric("number_of_total_units",
                    function(x) standardGeneric("number_of_total_units"))

#' @name number_of_total_units
#'
#' @rdname number_of_total_units
#'
#' @usage \S4method{number_of_total_units}{ConservationProblem}(x)
#'
methods::setMethod("number_of_total_units", "ConservationProblem",
  function(x) x$number_of_total_units())
