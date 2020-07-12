#' @include internal.R
NULL

#' Number of zones
#'
#' Extract the number of zones in an object.
#'
#' @param x [problem()] (i.e. [`ConservationProblem-class`]),
#'   [`OptimizationProblem-class`], or [Zones()] object.
#'
#' @return `integer` number of zones.
#'
#' @name number_of_zones
#'
#' @aliases number_of_zones,ConservationProblem-method number_of_zones,OptimizationProblem-method number_of_zones,ZonesRaster-method number_of_zones,ZonesCharacter-method
#'
#' @examples
#' # load data
#' data(sim_pu_zones_stack, sim_features_zones)
#'
#' # print number of zones in a Zones object
#' print(number_of_zones(sim_features_zones))
#
#' # create problem with multiple zones
#' p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'      add_min_set_objective() %>%
#'      add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
#'      add_binary_decisions()
#'
#' # print number of zones in the problem
#' print(number_of_zones(p))
NULL

#' @name number_of_zones
#'
#' @rdname number_of_zones
#'
#' @exportMethod number_of_zones
#'
#' @usage number_of_zones(x)
#'
methods::setGeneric("number_of_zones",
  function(x) standardGeneric("number_of_zones"))

#' @name number_of_zones
#'
#' @rdname number_of_zones
#'
#' @usage \S4method{number_of_zones}{ConservationProblem}(x)
#'
methods::setMethod("number_of_zones", "ConservationProblem",
  function(x) x$number_of_zones())

#' @name number_of_zones
#'
#' @rdname number_of_zones
#'
#' @usage \S4method{number_of_zones}{OptimizationProblem}(x)
#'
methods::setMethod("number_of_zones", "OptimizationProblem",
  function(x) x$number_of_zones())

#' @name number_of_zones
#'
#' @rdname number_of_zones
#'
#' @usage \S4method{number_of_zones}{ZonesRaster}(x)
#'
methods::setMethod("number_of_zones", "ZonesRaster", function(x) length(x))

#' @name number_of_zones
#'
#' @rdname number_of_zones
#'
#' @usage \S4method{number_of_zones}{ZonesCharacter}(x)
#'
methods::setMethod("number_of_zones", "ZonesCharacter", function(x) length(x))
