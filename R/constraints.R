#' @include internal.R
NULL

#' Conservation problem constraints
#'
#' A constraint can be added to a conservation planning problem as a way to
#' make certain  solutions invalid, given a cutoff criteria. Used when specific
#' planning units or configurations of planning units are undesirable or
#' inefficient.
#'
#' @details
#' Like a penalty, a constraint can be used as a mechanism to increase
#' connectivity between planning units, but works by eliminating all solutions
#' without a certain degree of connectivity, rather than penalizing poorly
#' connected solutions. The solution cost will likely be less affected by
#' applying a constraint than a penalty. Use the
#' \code{add_connected_constraints}, \code{add_corridor_constraints}, or
#' \code{add_neighbor_constraints} functions for connectivity considerations,
#' or see \code{\link{penalties}}.
#'
#' Constraints can also be used to lock in or lock out certain planning units
#' from the solution, such as protected areas or degraded land. Use the
#' \code{add_locked_in_constraints} and \code{add_locked_out_constraints}
#' functions to do this.
#'
#' Below are the constraints that can be added to a \code{ConservationProblem}
#' object.
#'
#' \describe{
#'   \item{\code{\link{add_connected_constraints}}}{Add constraints to a
#'     conservation problem to ensure that all selected
#'     planning units are spatially connected to each other.}
#'
#'   \item{\code{\link{add_corridor_constraints}}}{Adds constraints to ensure
#'     that all planning units used to represent features form a contiguous
#'     reserve.}
#'
#'   \item{\code{\link{add_locked_in_constraints}}}{Add constraints to ensure
#'     that certain planning units are present in the solution.}
#'
#'   \item{\code{\link{add_locked_out_constraints}}}{Add constraints to ensure
#'     that certain planning units are absent from the solution.}
#'
#'   \item{\code{\link{add_neighbor_constraints}}}{Add constraints to a
#'     conservation problem to ensure that all selected planning units have at
#'     least a certain number of neighbors.}
#' }
#'
#' @seealso \code{\link{decisions}}, \code{\link{objectives}},
#'  \code{\link{penalties}}, code{\link{portfolios}}, \code{\link{problem}},
#'  \code{\link{solvers}}, \code{\link{targets}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create base problem with no additional constraints
#' p1 <- problem(sim_pu_polygons, sim_features, "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#'
#' # create problem with locked in constraints
#' p2 <- p1 %>% add_locked_in_constraints("locked_in")
#'
#' # create problem with locked in constraints
#' p3 <- p1 %>% add_locked_out_constraints("locked_out")
#'
#' # create problem with neighbor constraints
#' p4 <- p1 %>% add_neighbor_constraints(2)
#'
#' # create problem with connected constraints
#' p5 <- p1 %>% add_connected_constraints()
#' \donttest{
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#' s4 <- solve(p4)
#' s5 <- solve(p5)
#'
#' # plot solutions
#' par(mfrow = c(3, 2), mar = c(0, 0, 4.1, 0))
#' plot(s1, main = "basic")
#' plot(s1[s1$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s2, main="locked in")
#' plot(s2[s2$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s3, main="locked out")
#' plot(s3[s3$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s4, main = "neighbor")
#' plot(s4[s4$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s5, main = "connected")
#' plot(s5[s5$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' }
#'
#' @name constraints
NULL
