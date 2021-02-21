#' @include Solver-proto.R
NULL

#' Add a default solver
#'
#' Identify the best solver currently installed on the system and specify that
#' it should be used to solve a conservation planning [problem()].
#' For information on the performance of different solvers,
#' please see Schuster _et al._ (2020) for benchmarks comparing the
#' run time and solution quality of some of the available solvers when applied
#' to different sized datasets.
#'
#' @param x [problem()] (i.e. [`ConservationProblem-class`]) object.
#'
#' @param ... arguments passed to the solver.
#'
#' @details
#' Ranked from best to worst, the available solvers that can be used are:
#' [add_gurobi_solver()], [add_cplex_solver()], [add_cbc_solver()],
#' [add_lpsymphony_solver()], and finally [add_rsymphony_solver()].
#'
#' @inherit add_gurobi_solver return seealso
#'
#' @references
#' Schuster R, Hanson JO, Strimas-Mackey M, and Bennett JR (2020). Exact
#' integer linear programming solvers outperform simulated annealing for
#' solving conservation planning problems. *PeerJ*, 8: e9258.
#'
#' @export
add_default_solver <- function(x, ...) {
  ds <- default_solver_name()
  if (identical(ds, "gurobi")) {
    return(add_gurobi_solver(x, ...))
  } else if (identical(ds, "cplexAPI")) {
    return(add_cplex_solver(x, ...))
  } else if (identical(ds, "rcbc")) {
    return(add_cbc_solver(x, ...))
  } else if (identical(ds, "lpsymphony")) {
    return(add_lpsymphony_solver(x, ...))
  } else if (identical(ds, "Rsymphony")) {
    return(add_rsymphony_solver(x, ...))
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

#' Default solver name
#'
#' This function returns the name of the default solver. If no solvers are
#' detected on the system, then a `NULL` object is returned.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{Rsymphony}, \pkg{lpsymphony}, \pkg{gurobi}, \pkg{cplexAPI}.
#'
#' @return `character` indicating the name of the default solver.
#'
#' @noRd
default_solver_name <- function() {
  if (requireNamespace("gurobi", quietly = TRUE)) {
    return("gurobi")
  } else if (requireNamespace("cplexAPI", quietly = TRUE)) {
    return("cplexAPI")
  } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
    return("Rsymphony")
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    return("lpsymphony")
  } else if (requireNamespace("rcbc", quietly = TRUE)) {
    return("rcbc")
  } else {
    return(NULL)
  }
}
