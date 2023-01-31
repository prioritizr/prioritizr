#' @include internal.R
NULL

#' Feature names
#'
#' Extract the names of the features in an object.
#'
#' @param x [problem()] or [Zones()] object.
#'
#' @return A `character` vector of feature names.
#'
#' @name feature_names
#'
#' @aliases feature_names,ConservationProblem-method feature_names,ZonesRaster-method feature_names,ZonesCharacter-method feature_names,ZonesSpatRaster-method
#'
#' @examples
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
methods::setGeneric(
  "feature_names",
   function(x) {
     rlang::check_required(x)
     standardGeneric("feature_names")
   }
 )

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @usage \S4method{feature_names}{ConservationProblem}(x)
#'
methods::setMethod(
  "feature_names",
  "ConservationProblem",
  function(x) x$feature_names()
)

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @usage \S4method{feature_names}{ZonesRaster}(x)
#'
methods::setMethod(
  "feature_names",
  "ZonesRaster",
  function(x) attr(x, "feature_names")
)

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @usage \S4method{feature_names}{ZonesSpatRaster}(x)
#'
methods::setMethod(
  "feature_names",
  "ZonesSpatRaster",
  function(x) attr(x, "feature_names")
)

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @usage \S4method{feature_names}{ZonesCharacter}(x)
#'
methods::setMethod(
  "feature_names",
  "ZonesCharacter",
  function(x) attr(x, "feature_names")
)
