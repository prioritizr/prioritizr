#' @include Solver-proto.R
NULL

#' Default solver
#'
#' Finds the best solver currently installed on the system and uses it.
#' In decreasing order of preference: Gurobi (\code{\link{add_gurobi_solver}}),
#' Rsymphony (\code{\link{add_rsymphony_solver}}), then lpsymphony
#' (\code{\link{add_lpsymphony_solver}}).
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param ... arguments passed to the default solver.
#'
#' @seealso \code{\link{solvers}}.
#'
#' @export
add_default_solver <- function(x, ...) {
  if (requireNamespace("gurobi", quietly = TRUE)) {
    return(add_gurobi_solver(x, ...))
  } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
    return(add_rsymphony_solver(x, ...))
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    return(add_lpsymphony_solver(x, ...))
  } else {
    assertthat::assert_that(inherits(x, "ConservationProblem"))
    return(x$add_solver(pproto(
      "MissingSolver",
      Solver,
      name = "MissingSolver",
      solve = function(self, x) {
        stop("no optimization problem solvers found on system.")
      })))
  }
}
