#' @include Solver-class.R
NULL

#' Add default solver
#'
#' Specify that the best solver currently available should be
#' used to solve a conservation planning problem.
#'
#' @param x [problem()] object.
#'
#' @param ... arguments passed to the solver.
#'
#' @details
#' Ranked from best to worst, the available solvers that can be used are:
#' [add_gurobi_solver()], [add_cplex_solver()], [add_cbc_solver()],
#' [add_highs_solver()], [add_lpsymphony_solver()], and finally
#' [add_rsymphony_solver()].
#' For information on the performance of different solvers,
#' please see Schuster _et al._ (2020).
#'
#' @inherit add_gurobi_solver return
#'
#' @seealso
#' See [solvers] for an overview of all functions for adding a solver.
#'
#' @family solvers
#'
#' @references
#' Schuster R, Hanson JO, Strimas-Mackey M, and Bennett JR (2020). Exact
#' integer linear programming solvers outperform simulated annealing for
#' solving conservation planning problems. *PeerJ*, 8: e9258.
#'
#' @export
add_default_solver <- function(x, ...) {
  # assert valid arguments
  assert_required(x)
  assert(is_conservation_problem(x), call = NULL)
  if (!identical(Sys.getenv("PRIORITIZR_ENABLE_COMPILE_SOLVER"), "TRUE")) {
    assert(
      any_solvers_installed(),
      call = NULL
    )
  }
  # find solver
  ds <- default_solver_name()
  # return solver
  if (identical(ds, "gurobi")) {
    return(add_gurobi_solver(x, ...))
  } else if (identical(ds, "cplexAPI")) {
    return(add_cplex_solver(x, ...))
  } else if (identical(ds, "rcbc")) {
    return(add_cbc_solver(x, ...))
  } else if (identical(ds, "highs")) {
    return(add_highs_solver(x, ...))
  } else if (identical(ds, "lpsymphony")) {
    return(add_lpsymphony_solver(x, ...))
  } else if (identical(ds, "Rsymphony")) {
    return(add_rsymphony_solver(x, ...))
  } else if (
      identical(Sys.getenv("PRIORITIZR_ENABLE_COMPILE_SOLVER"), "TRUE")
    )
  {
    return(add_compile_solver(x, ...))
  } else {
    cli::cli_abort(
      "Solver not recognized.",
      call = NULL,
      .internal = TRUE
    )
  }
}

#' Default solver name
#'
#' This function returns the name of the default solver. If no solvers are
#' detected on the system, then a `NULL` object is returned.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{gurobi}, \pkg{cplexAPI}, \pkg{rcbc}, \pkg{highs},
#'   \pkg{lpsymphony}, \pkg{Rsymphony}.
#'
#' @return `character` indicating the name of the default solver.
#'
#' @noRd
default_solver_name <- function() {
  if (requireNamespace("gurobi", quietly = TRUE)) {
    return("gurobi")
  } else if (requireNamespace("cplexAPI", quietly = TRUE)) {
    return("cplexAPI")
  } else if (requireNamespace("rcbc", quietly = TRUE)) {
    return("rcbc")
  } else if (requireNamespace("highs", quietly = TRUE)) {
    return("highs")
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    return("lpsymphony")
  } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
    return("Rsymphony")
  } else {
    return(NULL)
  }
}
