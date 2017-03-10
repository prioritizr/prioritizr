#' @include internal.R
NULL

#' Simulated conservation planning data
#'
#' Data for making spatial prioritizations using simulated data.
#'
#' \describe{
#'   \item{\code{sim_pu_raster}}{Planning units are represented as raster data.
#'     Pixel values indicate cost.}
#'
#'   \item{\code{sim_locked_in_raster}}{Planning units are represented as raster
#'     data. Pixel values are binary and indicate if planning units should
#'     be locked in.}
#'
#'   \item{\code{sim_locked_out_raster}}{Planning units are represented as raster
#'     data. Pixel values are binary and indicate if planning units should
#'     be locked out.}
#'
#'   \item{\code{sim_pu_polygons}}{Planning units represented as polygon data.
#'     The attribute table contains data indicating the expenditure required
#'     for prioritizing each planning unit ('cost' column), if the planning
#'     units should be selected in the solution ('locked_in' column), and
#'     and if the planning units should never be selected in the solution
#'     ('locked_out' column).}
#'
#'   \item{\code{sim_pu_points}}{Planning units represented as point data.
#'     The attribute table follows the same conventions as for
#'     \code{sim_pu_polygons}}.
#'
#'   \item{\code{sim_pu_lines}}{Planning units represented as line data.
#'     The attribute table follows the same conventions as for
#'     \code{sim_pu_polygons}}.
#'
#'   \item{\code{sim_features}}{The simulated distribution of ten species.
#'     Pixel values indicate habitat suitability.}
#'
#'   \item{\code{sim_phylogeny}}{The phylogenetic tree for the ten species.}
#' }
#'
#' @docType data
#'
#' @aliases sim_pu_polygons sim_pu_points sim_pu_lines sim_pu_raster sim_phylogeny sim_features sim_phylogeny sim_locked_in_raster sim_locked_out_raster
#'
#' @usage data(sim_features)
#'
#' @usage data(sim_pu_polygons)
#'
#' @usage data(sim_pu_points)
#
#' @usage data(sim_pu_lines)
#'
#' @usage data(sim_pu_raster)
#'
#' @usage data(sim_phylogeny)
#'
#' @format \describe{
#'
#'   \item{sim_features}{\code{\link[raster]{RasterStack-class}} object.}
#'
#'   \item{sim_pu_polygons}{\code{\link[sp]{SpatialPolygonsDataFrame-class}}
#'     object.}
#'
#'   \item{sim_pu_lines}{\code{\link[sp]{SpatialLinesDataFrame-class}} object.}
#'
#'   \item{sim_pu_points}{\code{\link[sp]{SpatialPointsDataFrame-class}}
#'     object.}
#'
#'   \item{sim_pu_raster}{\code{\link[raster]{RasterLayer-class}} object.}
#'
#'   \item{sim_phylogeny}{\code{\link[ape]{phylo}} object.}
#'
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
#' # plot data
#' par(mfrow = c(3, 3))
#' plot(sim_pu_raster, main = "planning units (raster)")
#' plot(sim_locked_in_raster, main = "locked in units (raster)")
#' plot(sim_locked_out_raster, main = "locked out units (raster)")
#' plot(sim_pu_polygons, main = "planning units (polygons)")
#' plot(sim_pu_lines, main = "planning units (lines)")
#' plot(sim_pu_points, main = "planning units (points)")
#' if (requireNamespace("ape", quietly = TRUE)) {
#'  ape::plot.phylo(sim_phylogeny, main = "simulated phylogeny")
#' } else {
#'  message("the \"ape\" package needs to be installed to plot phylogenies")
#' }
#'
#' par(mfrow = c(1, 1))
#' plot(sim_features)
#'
#' @name sim_data
NULL

#' @rdname sim_data
"sim_features"

#' @rdname sim_data
"sim_pu_polygons"

#' @rdname sim_data
"sim_pu_lines"

#' @rdname sim_data
"sim_pu_points"

#' @rdname sim_data
"sim_pu_raster"

#' @rdname sim_data
"sim_phylogeny"

#' @rdname sim_data
"sim_locked_in_raster"

#' @rdname sim_data
"sim_locked_out_raster"
