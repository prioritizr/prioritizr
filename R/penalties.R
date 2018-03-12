#' @include internal.R Collection-proto.R
NULL

#' Conservation problem penalties
#'
#' A penalty can be applied to a conservation planning \code{\link{problem}} to
#' penalize solutions according to a specific metric. Penalties---unlike
#' \code{\link{constraints}}---act as an explicit trade-off with the objective
#' being minimized or maximized (e.g. total solution cost given
#' \code{\link{add_min_set_objective}}).
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
#' @seealso \code{\link{constraints}}, \code{\link{decisions}},
#'  \code{\link{objectives}} \code{\link{portfolios}}, \code{\link{problem}},
#'  \code{\link{solvers}}, \code{\link{targets}}.
#'
#' @examples
#' ## not implemented
#' # ## load data
#' # data(sim_pu_points, sim_features)
#' #
#' # ## create basic problem
#' # p1 <- problem(sim_pu_raster, sim_features) %>%
#' #       add_min_set_objective() %>%
#' #       add_relative_targets(0.2) %>%
#' #       add_default_solver()
#' #
#' # ## create problem with boundary penalties
#' # p2 <- p1 %>% add_boundary_penalties(5, 1)
#' #
#' # ## create problem with connectivity penalties
#' # # create connectivity matrix based on spatial proximity
#' # sc_matrix <- as.data.frame(sim_pu_raster, xy = TRUE)
#' # sc_matrix <- sc_matrix[is.finite(values(sim_pu_raster)), 1:2]
#' # sc_matrix <- 1 / (as.matrix(dist(sc_matrix)) + 1)
#' #
#' # # remove connections between planning units with little connectivity
#' # sc_matrix[sc_matrix < 0.85] <- 0
#' #
#' # # create problem
#' # p3 <- p1 %>% add_connectivity_penalties(25, sc_matrix)
#' # \donttest{
#' # ## solve problems
#' # s <- stack(solve(p1), solve(p2), solve(p3))
#' #
#' # # plot solutions
#' # plot(s, axes = FALSE, box = FALSE,
#' #      main = c("basic solution", "boundary penalties",
#' #               "connectivity penalties"))
#' # }
#' @name penalties
NULL
