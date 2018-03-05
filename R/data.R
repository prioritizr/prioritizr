#' @include internal.R
NULL

#' Simulated conservation planning data
#'
#' Data for making spatial prioritizations using simulated data.
#'
#' \describe{
#'   \item{\code{sim_pu_raster}}{Planning units are represented as raster data.
#'     Pixel values indicate planning unit cost and \code{NA} values indicate
#'     that a pixel is not a planning unit.}
#'
#'   \item{\code{sim_pu_zones_stack}}{Planning units are represented as raster
#'     stack data. Each layer indicates the cost for a different management
#'     zone. Pixels with \code{NA} values in a given zone indicate that a
#'     planning unit cannot be allocated to that zone in a solution.
#'     Additionally, pixels with \code{NA} values in all layers are not a
#'     planning unit.}
#'
#'   \item{\code{sim_locked_in_raster}}{Planning units are represented as raster
#'     data. Pixel values are binary and indicate if planning units should be
#'     locked in to the solution.}
#'
#'   \item{\code{sim_locked_out_raster}}{Planning units are represented as
#'     raster data. Pixel values are binary and indicate if planning units
#'     should be locked out from the solution.}
#'
#'   \item{\code{sim_locked_zones_stack}}{Planning units are represented as
#'      raster stack data. Here, each layer corresponds to a different
#'      management zone. Pixel values indicate the value that each planning
#'      unit should be assigned in the solution. Pixels with \code{NA} values
#'      indicate that the planning unit is not locked. This format is provided
#'      as the general-case format for locking planning units and can be
#'      used to lock proportions of of planning units to specific zones.}
#'
#'   \item{\code{sim_pu_polygons}}{Planning units represented as polygon data.
#'     The attribute table contains fields (columns) indicating the expenditure
#'     required for prioritizing each planning unit ("cost" field), if the
#'     planning units should be selected in the solution ("locked_in" field),
#'     and if the planning units should never be selected in the solution
#'     ("locked_out" field).}
#'
#'   \item{\code{sim_pu_points}}{Planning units represented as point data.
#'     The attribute table follows the same conventions as for
#'     \code{sim_pu_polygons}.}
#'
#'   \item{\code{sim_pu_lines}}{Planning units represented as line data.
#'     The attribute table follows the same conventions as for
#'     \code{sim_pu_polygons}.}
#'
#'   \item{\code{sim_pu_zone_polygons}}{Planning units represented as polygon
#'     data. The attribute table contains fields (columns) indicating the
#'     expenditure required for prioritizing each planning unit under different
#'     management zones ("cost_1", "cost_2", and "cost_3" fields), and a series
#'     of fields indicating the value that each planning unit that should be
#'     assigned in the solution ("locked_1", "locked_2", "locked_3" fields).
#'     In these locked fields, planning units that should not be locked to a
#'     specific value are assigned a \code{NA} value.}
#'
#'   \item{\code{sim_features}}{The simulated distribution of ten species.
#'     Pixel values indicate habitat suitability.}
#'
#'   \item{\code{sim_features_zones}}{The simulated distribution for five
#'     species under three different management zones.}
#'
#'   \item{\code{sim_phylogeny}}{The phylogenetic tree for the ten species.}
#' }
#'
#' @docType data
#'
#' @aliases sim_pu_polygons sim_pu_zones_polygons sim_pu_points sim_pu_lines sim_pu_raster sim_locked_in_raster sim_locked_out_raster sim_pu_zones_stack sim_locked_zones_stack sim_features sim_features_zones sim_phylogeny
#'
#' @usage data(sim_pu_polygons)
#'
#' @usage data(sim_pu_zones_polygons)
#'
#' @usage data(sim_pu_points)
#
#' @usage data(sim_pu_lines)
#'
#' @usage data(sim_pu_raster)
#'
#' @usage data(sim_locked_in_raster)
#'
#' @usage data(sim_locked_out_raster)
#'
#' @usage data(sim_pu_zones_stack)
#'
#' @usage data(sim_locked_zones_stack)
#'
#' @usage data(sim_features)
#'
#' @usage data(sim_features_zones)
#'
#' @usage data(sim_phylogeny)
#'
#' @format \describe{
#'   \item{sim_pu_polygons}{\code{\link[sp]{SpatialPolygonsDataFrame-class}}
#'     object.}
#'
#'   \item{sim_pu_zones_polygons}{
#'     \code{\link[sp]{SpatialPolygonsDataFrame-class}} object.}
#'
#'   \item{sim_pu_lines}{\code{\link[sp]{SpatialLinesDataFrame-class}} object.}
#'
#'   \item{sim_pu_points}{\code{\link[sp]{SpatialPointsDataFrame-class}}
#'     object.}
#'
#'   \item{sim_pu_raster}{\code{\link[raster]{RasterLayer-class}} object.}
#'
#'   \item{sim_pu_zones_stack}{\code{\link[raster]{RasterStack-class}} object.}
#'
#'   \item{sim_locked_in_raster}{\code{\link[raster]{RasterLayer-class}}
#'     object.}
#'
#'   \item{sim_locked_out_raster}{\code{\link[raster]{RasterLayer-class}}
#'     object.}
#'
#'   \item{sim_locked_zones_stack}{\code{\link[raster]{RasterStack-class}}
#'     object.}
#'
#'   \item{sim_features}{\code{\link[raster]{RasterStack-class}} object.}
#'
#'   \item{sim_features_zones}{\code{\link{ZonesRaster}} object.}
#'
#'   \item{sim_phylogeny}{\code{\link[ape]{phylo}} object.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' # load data
#' data(sim_pu_polygons, sim_pu_lines, sim_pu_points, sim_pu_raster,
#'      sim_locked_in_raster, sim_locked_out_raster, sim_phylogeny,
#'      sim_features)
#'
#' # plot example planning unit data
#' par(mfrow = c(2, 3))
#' plot(sim_pu_raster, main = "planning units (raster)")
#' plot(sim_locked_in_raster, main = "locked in units (raster)")
#' plot(sim_locked_out_raster, main = "locked out units (raster)")
#' plot(sim_pu_polygons, main = "planning units (polygons)")
#' plot(sim_pu_lines, main = "planning units (lines)")
#' plot(sim_pu_points, main = "planning units (points)")
#'
#' # plot example phylogeny data
#' par(mfrow = c(1, 1))
#' if (requireNamespace("ape", quietly = TRUE)) {
#'  ape::plot.phylo(sim_phylogeny, main = "simulated phylogeny")
#' } else {
#'  message("the \"ape\" package needs to be installed to plot phylogenies")
#' }
#'
#' # plot example feature data
#' par(mfrow = c(1, 1))
#' plot(sim_features)
#'
#' # plot example management zone cost data
#' par(mfrow = c(1, 1))
#' plot(sim_pu_zones_stack)
#'
#' # plot example feature data under different zones
#' par(mfrow = c(5, 3))
#' for (i in length(sim_features_zones))
#'   for (j in raster::nlayers(sim_features_zones[[i]]))
#'     plot(sim_features_zones[[i]][[j]],
#'          main = paste0("Species ", i, "(zone ", j ))
#'
#' # plot example locked data
#' par(mfrow = c(1, 1))
#' plot(sim_locked_zones_stack)
#' @name sim_data
NULL

#' @rdname sim_data
"sim_features"

#' @rdname sim_data
"sim_features_zones"

#' @rdname sim_data
"sim_pu_polygons"

#' @rdname sim_data
"sim_pu_zones_polygons"

#' @rdname sim_data
"sim_pu_zones_polygons"

#' @rdname sim_data
"sim_pu_lines"

#' @rdname sim_data
"sim_pu_points"

#' @rdname sim_data
"sim_pu_raster"

#' @rdname sim_data
"sim_pu_zones_stack"

#' @rdname sim_data
"sim_phylogeny"

#' @rdname sim_data
"sim_locked_in_raster"

#' @rdname sim_data
"sim_locked_out_raster"

#' @rdname sim_data
"sim_locked_zones_stack"
