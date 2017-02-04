#' @include internal.R
NULL

#' Tasmanian conservation planning scenario
#'
#' This scenario involves making a prioritsation to adequately conserve
#' the main types of vegetation found in Tasmania for minimal cost. To
#' acheive this goal, the scenario involves using the following data:
#' 
#' \describe{
#'   \item{\code{tas_pu}}{Planning unit data. The attribute table contains
#'     three columns containing unique identifiers ('ids'), unimproved land
#'     values ('cost'), and their existing level of protection ('status').
#'     Units with 50\% or more of their area contained in IUCN protected areas
#'     are associated with a status of 2, otherwise they are associated with a
#'     value of 0.}
#'   \item{\code{tas_features}}{The distribution of 63 vegetation 
#'     classes in Tasmania, Australia. Each layer in the  stack 
#'     represents a different vegetation class. For a given layer, 
#'     cells indicate the presence (value of 1) or absence (value of 0)
#'     of the vegetation class in an area.}
#' }
#'
#' @docType data
#'
#' @aliases tas_features tas_pu
#'
#' @usage data(tas_features)
#'
#' @usage data(tas_pu)
#'
#' @format \describe{
#'   \item{tas_features}{\code{\link[raster]{RasterStack-class}} object}
#'   \item{tas_pu}{\code{\link[sp]{SpatialPolygonsDataFrame}} object.}
#' }
#'
#' @keywords datasets
#'
#' @references This data was obtained from the "Introduction to Marxan" course
#'   (\url{http://marxan.net/courses}).
#'
#'
#' @examples
#' # load data
#' data(tas_pu, tas_features)
#'
#' # plot data
#' plot(tas_pu)
#' plot(tas_features)
#' 
#' # make prioritisation
#' p <- problem(tas_pu, tas_features) +
#'      minimum_set_objective() +
#'      relative_target(0.1) +
#'      rsymphony_solver()
#
#' # make solution
#' s <- solve(p)
#
#' # plot solution
#' plot(s)
#'
#' @name tas_data
NULL

#' Simulated conservation planning scenario
#' 
#' This scenario involves making a prioritsation to adequately conserve
#' as many simulated species as possible within a budget. To acheive this
#' goal, the scenario involves using the following data:
#' 
#' \describe{
#'   \item{\code{sim_pu}}{Planning unit data. Pixel values indicate cost.}
#'   \item{\code{sim_features}}{The simulated distribution of ten species.
#'     Pixel values indicate habitat suitability.}
#'   \item{\code{sim_phylogeny}}{The phylogenetic tree for the ten species.}
#' }
#'
#' @docType data
#'
#' @aliases sim_pu sim_features sim_phylogeny
#'
#' @usage data(sim_features)
#'
#' @usage data(sim_pu)
#'
#' @usage data(sim_phylogeny)
#'
#' @format \describe{
#'   \item{sim_features}{\code{\link[raster]{RasterStack-class}} object}
#'   \item{sim_pu}{\code{\link[raster]{RasterLayer-class}} object}
#'   \item{sim_phylogeny}{\code{\link[ape]{phylo}} object}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' # load data
#' data(sim_pu, sim_features, sim_taxonomy)
#'
#' # plot data
#' plot(sim_pu)
#' plot(sim_features)
#' 
#' # make prioritisation treating species equally
#' p1 <- problem(sim_pu, sim_features) +
#'      maximum_coverage_objective(budget=20) +
#'      relative_target(0.2) +
#'      rsymphony_solver()
#'
#' # make prioritisation accounting for species relatedness
#' p2 <- problem(sim_pu, sim_features) +
#'      phylogenetic_coverage_objective(budget=20, tree=sim_phylogeny) +
#'      relative_target(0.2) +
#'      rsymphony_solver()
#' 
#' # make solutions
#  s1 <- solve(p1)
#' s2 <- solve(p2)
#
#' # plot solutions
#  plot(stack(s1, s2))
#'
#' @name sim_data
NULL
