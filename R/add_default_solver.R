#' @include Solver-proto.R
NULL

#' Default solver
#'
#' Identify the best solver currently installed on the system and specify that
#' it should be used to solve a conservation planning [problem()].
#' Ranked from best to worst, the available solvers that can be used are:
#' \pkg{gurobi}
#' ([add_gurobi_solver()]),
#' \pkg{Rsymphony} ([add_rsymphony_solver()]), then \pkg{lpsymphony}
#' ([add_lpsymphony_solver()]).
#'
#' @param x [ConservationProblem-class()] object.
#'
#' @param ... arguments passed to the solver.
#'
#' @seealso [solvers()].
#'
#' @export
add_default_solver <- function(x, ...) {
  ds <- default_solver_name()
  if (identical(ds, "gurobi")) {
    return(add_gurobi_solver(x, ...))
  } else if (identical(ds, "Rsymphony")) {
    return(add_rsymphony_solver(x, ...))
  } else if (identical(ds, "lpsymphony")) {
    return(add_lpsymphony_solver(x, ...))
  } else {
    assertthat::assert_that(inherits(x, "ConservationProblem"))
    return(x$add_solver(pproto(
      "MissingSolver",
      Solver,
      name = "MissingSolver",
      solve = function(self, x) {
        stop("no optimization problem solvers found on system")
      })))
  }
}
