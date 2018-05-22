#' @include internal.R
NULL

#' Feature names
#'
#' Extract the names of the features in an object.
#'
#' @param x \code{\link{ConservationProblem-class}} or \code{\link{Zones}}
#    object.
#'
#' @return \code{character} feature names.
#'
#' @name feature_names
#'
#' @aliases feature_names,ConservationProblem-method feature_names,ZonesRaster-method feature_names,ZonesCharacter-method
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
#' # print feature names
#' print(feature_names(p))
NULL

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @exportMethod feature_names
#'
#' @usage feature_names(x)
#'
methods::setGeneric("feature_names",
                    function(x) standardGeneric("feature_names"))

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @usage \S4method{feature_names}{ConservationProblem}(x)
#'
methods::setMethod("feature_names", "ConservationProblem",
  function(x) x$feature_names())

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @usage \S4method{feature_names}{ZonesRaster}(x)
#'
methods::setMethod("feature_names", "ZonesRaster",
  function(x) attr(x, "feature_names"))

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @usage \S4method{feature_names}{ZonesCharacter}(x)
#'
methods::setMethod("feature_names", "ZonesCharacter",
  function(x) attr(x, "feature_names"))
