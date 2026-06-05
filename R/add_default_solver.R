#' @include Solver-class.R standalone-assertions_misc.R
NULL

#' Add default solver
#'
#' Specify that the best solver currently available should be
#' used to solve a conservation planning problem.
#'
#' @inheritParams add_gurobi_solver
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
#' @examplesIf prioritizr::do_run_example()
#' # set seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create minimal problem with default portfolio
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.05) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # plot solution
#' plot(s)
#'
#' @export
add_default_solver <- function(x, ...) {
  # assert valid arguments
  assert_required(x)
  assert(is_generic_conservation_problem(x), call = NULL)
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
