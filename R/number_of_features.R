#' @include internal.R
NULL

#' Number of features
#'
#' Extract the number of features in an object.
#'
#' @param x [ConservationProblem-class()],
#'   [OptimizationProblem-class()] or [Zones()] object.
#'
#' @return `integer` number of features.
#'
#' @name number_of_features
#'
#' @aliases number_of_features,ConservationProblem-method number_of_features,OptimizationProblem-method number_of_features,ZonesRaster-method number_of_features,ZonesCharacter-method
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
#' # print number of features
#' print(number_of_features(p))
NULL

#' @name number_of_features
#'
#' @rdname number_of_features
#'
#' @exportMethod number_of_features
#'
#' @usage number_of_features(x)
#'
methods::setGeneric("number_of_features",
  function(x) standardGeneric("number_of_features"))

#' @name number_of_features
#'
#' @rdname number_of_features
#'
#' @usage \S4method{number_of_features}{ConservationProblem}(x)
#'
methods::setMethod("number_of_features", "ConservationProblem",
  function(x) x$number_of_features())

#' @name number_of_features
#'
#' @rdname number_of_features
#'
#' @usage \S4method{number_of_features}{OptimizationProblem}(x)
#'
methods::setMethod("number_of_features", "OptimizationProblem",
  function(x) x$number_of_features())

#' @name number_of_features
#'
#' @rdname number_of_features
#'
#' @usage \S4method{number_of_features}{ZonesRaster}(x)
#'
methods::setMethod("number_of_features", "ZonesRaster",
  function(x) raster::nlayers(x[[1]]))

#' @name number_of_features
#'
#' @rdname number_of_features
#'
#' @usage \S4method{number_of_features}{ZonesCharacter}(x)
#'
methods::setMethod("number_of_features", "ZonesCharacter",
  function(x) length(x[[1]]))
