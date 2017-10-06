#' @include internal.R Constraint-proto.R
NULL

#' Add neighbor constraints
#'
#' Add constraints to a conservation problem to ensure that all selected
#' planning units have at least a certain number of neighbors.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param k \code{integer} number of neighbors each selected planning units
#'   must have.
#'
#' @param ... arguments passed to \code{\link{connected_matrix}}.
#'
#' @return \code{\link{ConservationProblem-class}} object with the constraint
#'   added to it.
#'
#' @seealso \code{\link{constraints}} for all the available constraints,
#'  and \code{\link{penalties}}.
#'
#' @examples
#' # create basic problem
#' p1 <- problem(sim_pu_polygons, sim_features, "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2)
#'
#' # create problem with constraints that require 1 neighbor
#' p2 <- p1 %>% add_neighbor_constraints(1)
#'
#' # create problem with constraints that require 2 neighbors
#' p3 <- p1 %>% add_neighbor_constraints(2)
#'
#' # create problem with constraints that require 3 neighbors
#' p4 <- p1 %>% add_neighbor_constraints(3)
#'
#' \donttest{
#' # solve problems
#' s <- list(solve(p1), solve(p2), solve(p3), solve(p4))
#'
#' # plot solutions
#' par(mfrow=c(2,2))
#'
#' plot(s[[1]], main = "basic solution")
#' plot(s[[1]][s[[1]]$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s[[2]], main="1 neighbor")
#' plot(s[[2]][s[[2]]$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s[[3]], main="2 neighbors")
#' plot(s[[3]][s[[3]]$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s[[4]], main="3 neighbors")
#' plot(s[[4]][s[[4]]$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' }
#'
#' @export
add_neighbor_constraints <- function(x, k, ...) {
  # assert argumnt is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    assertthat::is.count(k))
  # add the constraint
  x$add_constraint(pproto(
    "NeighborConstraint",
    Constraint,
    data = list(arguments = list(...)),
    name = "Neighbor constraint",
    parameters = parameters(integer_parameter("number of neighbors",
                                              as.integer(k),
                                              lower_limit = 0L)),
    calculate = function(self, x) {
      assertthat::assert_that(inherits(x, "ConservationProblem"))
      if (is.Waiver(self$get_data("connected_matrix"))) {
        # create matrix
        m <- do.call(connected_matrix, append(list(x$data$cost),
                                              self$data$arguments))
        # coerce matrix to full matrix
        m <- methods::as(m, "dgCMatrix")
        # store data
        self$set_data("connected_matrix", m)
      }
      invisible(TRUE)
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      k <- self$parameters$get("number of neighbors")
      if (k > 0)
        rcpp_apply_neighbor_constraints(x$ptr,
          self$get_data("connected_matrix"), k)
      invisible(TRUE)
    }))
}
