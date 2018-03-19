#' @include internal.R Constraint-proto.R
NULL

#' Add connected constraints
#'
#' Add constraints to a conservation problem to ensure that all selected
#' planning units are spatially connected to each other.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param ... arguments passed to \code{\link{connected_matrix}}.
#'
#' @details The mathematical ideas that underpin this function inspired by
#'   {\"O}nal and Briers (2006).
#'
#' @return \code{\link{ConservationProblem-class}} object with the constraints
#'   added to it.
#'
#' @seealso \code{\link{constraints}}.
#'
#' @references
#' {\"{O}}nal H and Briers RA (2006) Optimal selection of a connected
#' reserve network. \emph{Operations Research}, 54: 379--388.
#'
#' @examples
#' # # not implemented
#' # # create basic problem
#' # p1 <- problem(sim_pu_raster, sim_features) %>%
#' #       add_min_set_objective() %>%
#' #       add_relative_targets(0.2)
#' #
#' # # create problem with added connected constraints
#' # p2 <- p1 %>% add_connected_constraints()
#' # \donttest{
#' # # solve problems
#' # s <- stack(solve(p1), solve(p2))
#' #
#' # # plot solutions
#' # plot(s, main = c("basic solution", "connected solution"), axes = FALSE,
#' #      box = FALSE)
#' # }
#'
#' @export
add_connected_constraints <- function(x, ...) {
  # assert argumnt is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    inherits(x$data$cost, c("Raster", "Spatial")))
  # add the constraint
  x$add_constraint(pproto(
    "ConnectedConstraint",
    Constraint,
    data = list(arguments = list(...)),
    name = "Connected constraint",
    parameters = parameters(binary_parameter("apply constraint?", 1L)),
    calculate = function(self, x) {
      assertthat::assert_that(inherits(x, "ConservationProblem"))
      if (is.Waiver(self$get_data("connected_matrix"))) {
        # create matrix
        m <- do.call(connected_matrix, append(list(x$data$cost),
                                              self$data$arguments))
        # retain only the upper or lower triangle to reduce computational
        # burden
        class(m) <- "dgCMatrix"
        # store data
        self$set_data("connected_matrix", m)
      }
      invisible(TRUE)
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      if (self$parameters$get("apply constraint?") == 1)
        rcpp_apply_connected_constraints(x$ptr,
                                         self$get_data("connected_matrix"))
      invisible(TRUE)
    }))
}
