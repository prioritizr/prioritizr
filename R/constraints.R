#' @include internal.R
NULL

#' Conservation problem constraints
#'
#' A constraint can be added to a conservation planning problem as a way to make certain 
#' solutions invalid, given a cutoff criteria. Used when specific planning units or 
#' configurations of planning units are undesireable or inefficient. 
#' 
#' @details
#' Like a penalty, a constraint can be used as a mechanism to increase 
#' connectivity between planning units, but works by eliminating all solutions without a certain 
#' degree of connectivity, rather than penalizing poorly connected solutions. 
#' The solution cost will likely be less affected by applying a constraint than a penalty.
#' Use the \code{add_connected_constraints}, \code{add_corridor_constraints}, or 
#' \code{add_neighbor_constraints} functions for connectivity considerations, or see \code{\link{penalties}}. 
#' 
#' Constraints can also be used to lock in or lock out certain planning units from the solution, 
#' such as protected areas or degraded land. Use the \code{add_locked_in_constraints} 
#' and \code{add_locked_out_constraints} functions to do this.
#' 
#' Below are the constraints that can be added to a \code{ConservationProblem} object.
#'
#' \describe{
#'
#'   \item{\code{\link{add_connected_constraints}}}{
#' Add constraints to a conservation problem to ensure that all selected
#' planning units are spatially connected to each other.
#'   }
#'
#'   \item{\code{\link{add_corridor_constraints}}}{
#' It is important to maintain connectivity between reserves. However,
#' some areas are more difficult for species to traverse then other areas.
#' As a consequence, even though reserves may be connected, species may
#' not be able to move between reserves if the areas connecting them
#' are barriers to dispersal.
#'
#' This function adds constraints to ensure that corridors connect
#' reserves and that individuals from all species can utilize the
#' corridors. Friction \code{\link[raster]{Raster-class}} objects area
#' used to show each difficult areas are to traverse.
#' }
#'
#'   \item{\code{\link{add_locked_in_constraints}}}{
#' Add constraints to ensure that they are prioritized in the solution.
#' For example, it may be desirable to lock in planning units that are
#' inside existing protected areas so that the solution fills in the gaps in the
#' existing reserve network.
#' }
#'
#'   \item{\code{\link{add_locked_out_constraints}}}{
#' Add constraints to ensure that certain planning units are not prioritized
#' in the solution. For example, it may be useful to lock out planning
#' units that have been degraded and are not longer suitable for conserving
#' species.
#' }
#'
#'   \item{\code{\link{add_neighbor_constraints}}}{
#' Add constraints to a conservation problem to ensure that all selected
#' planning units have at least a certain number of neighbors.
#' }
#'
#' }
#'
#' @seealso \code{\link{penalties}}, \code{\link{objectives}}, \code{\link{targets}},
#'   \code{\link{problem}}
#'
#' @name constraints
NULL
