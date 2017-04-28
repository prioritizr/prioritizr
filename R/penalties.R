#' @include internal.R Collection-proto.R
NULL

#' Conservation problem penalties
#'
#' A penalty can be applied to a conservation planning \code{problem} to penalize 
#' ineffective solutions. A penalty acts as a tradeoff to the \code{objective} 
#' being minimized or maximized, and thereby may increase the cost of 
#' the soution.  For example, when a boundary length penalty is applied 
#' there is an explicit tradeoff between fragmentation and cost. A 
#' \code{constraint} may also be used as a mechanism to increase solution 
#' connectivity, but instead of penalizing fragmentation, a constraint makes 
#' certain solutions invalid and therefore does not necessarily affect solution cost.
#'
#' \describe{
#'
#'   \item{\code{\link{add_boundary_penalties}}}{
#'
#' Add penalties to a conservation problem that favor solutions that
#' clump selected planning units into contiguous reserves.
#'
#'  }
#'
#'   \item{\code{\link{add_connectivity_penalties}}}{
#'
#' Add constraints to a conservation problem to favor solutions that select
#' planning units with high connectivity between them.
#'
#' }
#'
#' }
#'
#' @name penalties
#'
#' @seealso \code{\link{constraints}}, \code{\link{objectives}},
#'   \code{\link{problem}}
NULL
