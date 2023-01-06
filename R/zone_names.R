#' @include internal.R
NULL

#' Zone names
#'
#' Extract the names of zones in an object.
#'
#' @param x [problem()] or [zones()] object.
#'
#' @return A `character` vector of zone names.
#'
#' @name zone_names
#'
#' @aliases zone_names,ConservationProblem-method zone_names,OptimizationProblem-method zone_names,ZonesRaster-method zone_names,ZonesCharacter-method zone_names,ZonesSpatRaster-method
#'
#' @examples
#' # load data
#' sim_pu_zones_raster <- get_sim_zones_pu_raster()
#' sim_features_zones <- get_sim_zones_features()
#'
#' # print names of zones in a Zones object
#' print(zone_names(sim_features_zones))
#
#' # create problem with multiple zones
#' p <-
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions()
#'
#' # print zone names in problem
#' print(zone_names(p))
NULL

#' @name zone_names
#'
#' @rdname zone_names
#'
#' @exportMethod zone_names
#'
#' @usage zone_names(x)
#'
methods::setGeneric("zone_names",
                    function(x) standardGeneric("zone_names"))

#' @name zone_names
#'
#' @rdname zone_names
#'
#' @usage \S4method{zone_names}{ConservationProblem}(x)
#'
methods::setMethod("zone_names", "ConservationProblem",
  function(x) x$zone_names())

#' @name zone_names
#'
#' @rdname zone_names
#'
#' @usage \S4method{zone_names}{ZonesRaster}(x)
#'
methods::setMethod("zone_names", "ZonesRaster",
  function(x) attr(x, "zone_names"))

#' @name zone_names
#'
#' @rdname zone_names
#'
#' @usage \S4method{zone_names}{ZonesSpatRaster}(x)
#'
methods::setMethod("zone_names", "ZonesSpatRaster",
  function(x) attr(x, "zone_names"))

#' @name zone_names
#'
#' @rdname zone_names
#'
#' @usage \S4method{zone_names}{ZonesCharacter}(x)
#'
methods::setMethod("zone_names", "ZonesCharacter",
  function(x) attr(x, "zone_names"))
