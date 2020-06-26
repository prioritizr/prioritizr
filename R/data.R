#' @include internal.R
NULL

#' Simulated conservation planning data
#'
#' Simulated data for making spatial prioritizations.
#'
#' \describe{
#'
#' \item{`sim_pu_raster`}{Planning units are represented as raster data.
#'   Pixel values indicate planning unit cost and `NA` values indicate
#'   that a pixel is not a planning unit.}
#'
#' \item{`sim_pu_zones_stack`}{Planning units are represented as raster
#'   stack data. Each layer indicates the cost for a different management
#'   zone. Pixels with `NA` values in a given zone indicate that a
#'   planning unit cannot be allocated to that zone in a solution.
#'   Additionally, pixels with `NA` values in all layers are not a
#'   planning unit.}
#'
#' \item{`sim_locked_in_raster`}{Planning units are represented as raster
#'   data. Pixel values are binary and indicate if planning units should be
#'   locked in to the solution.}
#'
#' \item{`sim_locked_out_raster`}{Planning units are represented as
#'   raster data. Pixel values are binary and indicate if planning units
#'   should be locked out from the solution.}
#'
#' \item{`sim_pu_polygons`}{Planning units represented as polygon data.
#'   The attribute table contains fields (columns) indicating the expenditure
#'   required for prioritizing each planning unit ("cost" field), if the
#'   planning units should be selected in the solution ("locked_in" field),
#'   and if the planning units should never be selected in the solution
#'   ("locked_out" field).}
#'
#' \item{`sim_pu_points`}{Planning units represented as point data.
#'   The attribute table follows the same conventions as for
#'   `sim_pu_polygons`.}
#'
#' \item{`sim_pu_lines`}{Planning units represented as line data.
#'   The attribute table follows the same conventions as for
#'   `sim_pu_polygons`.}
#'
#' \item{`sim_pu_sf`}{Planning units represented as polygon data
#'   using the [sf::sf()] package.
#'   The attribute table follows the same conventions as for
#'   `sim_pu_polygons`.}
#'
#' \item{`sim_pu_zones_polygons`}{Planning units represented as polygon
#'   data. The attribute table contains fields (columns) indicating the
#'   expenditure required for prioritizing each planning unit under different
#'   management zones ("cost_1", "cost_2", and "cost_3" fields), and a series
#'   of fields indicating the value that each planning unit that should be
#'   assigned in the solution ("locked_1", "locked_2", "locked_3" fields).
#'   In these locked fields, planning units that should not be locked to a
#'   specific value are assigned a `NA` value.}
#'
#' \item{`sim_pu_zones_sf`}{Planning units represented as polygon data
#'   using the [sf::sf()] package. The attribute tables follows
#'   the same conventions as for `sim_pu_zone_polygons`.}
#'
#' \item{`sim_features`}{The simulated distribution of ten species.
#'   Pixel values indicate habitat suitability.}
#'
#' \item{`sim_features_zones`}{The simulated distribution for five
#'   species under three different management zones.}
#'
#'  \item{`sim_phylogeny`}{The phylogenetic tree for the ten species.}
#'
#' }
#'
#' @docType data
#'
#' @aliases sim_pu_polygons sim_pu_zones_polygons sim_pu_points sim_pu_lines sim_pu_raster sim_locked_in_raster sim_locked_out_raster sim_pu_zones_stack sim_features sim_features_zones sim_phylogeny sim_pu_sf sim_pu_zones_sf
#'
#' @usage data(sim_pu_polygons)
#'
#' @usage data(sim_pu_zones_polygons)
#'
#' @usage data(sim_pu_points)
#
#' @usage data(sim_pu_lines)
#'
#' @usage data(sim_pu_sf)
#'
#' @usage data(sim_pu_zones_sf)
#'
#' @usage data(sim_pu_raster)
#'
#' @usage data(sim_locked_in_raster)
#'
#' @usage data(sim_locked_out_raster)
#'
#' @usage data(sim_pu_zones_stack)
#'
#' @usage data(sim_features)
#'
#' @usage data(sim_features_zones)
#'
#' @usage data(sim_phylogeny)
#'
#' @format
#' \describe{
#'
#' \item{sim_pu_polygons}{[sp::SpatialPolygonsDataFrame-class]
#'   object.}
#'
#' \item{sim_pu_zones_polygons}{
#'   [sp::SpatialPolygonsDataFrame-class] object.}
#'
#' \item{sim_pu_sf}{[sf::sf()] object.}
#'
#' \item{sim_pu_zones_sf}{[sf::sf()] object.}
#'
#' \item{sim_pu_lines}{[sp::SpatialLinesDataFrame-class] object.}
#'
#' \item{sim_pu_points}{[sp::SpatialPointsDataFrame-class]
#'   object.}
#'
#' \item{sim_pu_raster}{[raster::RasterLayer-class] object.}
#'
#' \item{sim_pu_zones_stack}{[raster::RasterStack-class] object.}
#'
#' \item{sim_locked_in_raster}{[raster::RasterLayer-class]
#'   object.}
#'
#' \item{sim_locked_out_raster}{[raster::RasterLayer-class]
#'   object.}
#'
#' \item{sim_features}{[raster::RasterStack-class] object.}
#'
#' \item{sim_features_zones}{[ZonesRaster()] object.}
#'
#' \item{sim_phylogeny}{[ape::phylo()] object.}
#'
#' }
#'
#' @keywords datasets
#'
#' @examples
#' # load data
#' data(sim_pu_polygons, sim_pu_lines, sim_pu_points, sim_pu_raster,
#'      sim_locked_in_raster, sim_locked_out_raster, sim_phylogeny,
#'      sim_features, sim_pu_sf)
#'
#' # plot example Spatial-class planning unit data
#' \donttest{
#' par(mfrow = c(2, 3))
#' plot(sim_pu_raster, main = "planning units (raster)")
#' plot(sim_locked_in_raster, main = "locked in units (raster)")
#' plot(sim_locked_out_raster, main = "locked out units (raster)")
#' plot(sim_pu_polygons, main = "planning units (polygons)")
#' plot(sim_pu_lines, main = "planning units (lines)")
#' plot(sim_pu_points, main = "planning units (points)")
#'
#' # plot example sf-class planning unit data
#' plot(sim_pu_sf)
#'
#' # plot example phylogeny data
#' par(mfrow = c(1, 1))
#' ape::plot.phylo(sim_phylogeny, main = "simulated phylogeny")
#'
#' # plot example feature data
#' par(mfrow = c(1, 1))
#' plot(sim_features)
#'
#' # plot example management zone cost data
#' par(mfrow = c(1, 1))
#' plot(sim_pu_zones_stack)
#'
#' # plot example feature data for each management zone
#' plot(do.call(stack, sim_features_zones),
#'      main = paste0("Species ",
#'                    rep(seq_len(number_of_zones(sim_features_zones)),
#'                        number_of_features(sim_features_zones)),
#'                    " (zone ",
#'                    rep(seq_len(number_of_features(sim_features_zones)),
#'                        each = number_of_zones(sim_features_zones)),
#'                    ")"))
#' }
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
"sim_pu_sf"

#' @rdname sim_data
"sim_pu_zones_sf"

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
