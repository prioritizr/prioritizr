#' @include internal.R Collection-proto.R
NULL

#' Conservation problem penalties
#'
#' A penalty can be applied to a conservation planning \code{problem} to
#' penalize solutions with low connectivity between planning units. A penalty #' acts as an explicit tradeoff with the objective
#' being minimized or maximized, and thereby increases the cost of
#' the solution.  For example, when the \code{add_boundary_penalties} function
#' is added, solution fragmentation decreases but solution cost increases.
#'
#' @details Both penalties and constraints can be used as mechanisms to increase
#'   solution connectivity, but instead of penalizing solutions with low
#'   connectivity, a constraint acts as a cutoff to make certain solutions
#'   invalid. Applying a constraint does not necessarily affect solution cost,
#'   while applying a penalty does. Below are the penalties that can be added to
#'   a \code{ConservationProblem} object.
#'
#'   \describe{
#'
#'   \item{\code{\link{add_boundary_penalties}}}{
#'     Add penalties to a conservation problem to favor solutions that clump
#'     selected planning units together into contiguous reserves. Uses shared
#'     boundary length as a measure of connectivity, equivalent to the
#'     boundary length modifier (BLM) in \href{http://marxan.net}{Marxan}.
#'     Boundary data is calculated automatically unless the planning units in
#'     \code{x} are stored in a \code{data.frame}, in which case boundary data
#'     must be added as a \code{matrix} or \code{data.frame}. This function
#'     can only be used for symmetric relationships between planning units;
#'     for asymmetric relationships use \code{add_connectivity_penalties}.}
#'
#'   \item{\code{\link{add_connectivity_penalties}}}{
#'     Add constraints to a conservation problem to favor solutions that
#'     select planning units with high connectivity between them. Uses
#'     connectivity data in the form of a \code{matrix} or \code{data.frame}
#'     object, where strength of connectivity is a value such as the inverse
#'     distance between consecutive planning units. This function can be used
#'     for symmetric or asymmetric relationships between planning units.}
#'
#' }
#'
#' @name penalties
#'
#' @seealso \code{\link{constraints}}, \code{\link{decisions}},
#'  \code{\link{objectives}} \code{\link{portfolios}}, \code{\link{problem}},
#'  \code{\link{solvers}}, \code{\link{targets}}.
#'
NULL
