#' @include internal.R ConservationProblem-class.R MultiObjConservationProblem-class.R
NULL

#' Evaluate objective value of solution
#'
#' Calculate the objective value of a solution to a conservation planning
#' problem.
#'
#' @inheritParams eval_cost_summary
#'
#' @param include_penalties `logical` should penalties be included when
#' calculating objectives values? Defaults to `TRUE`.
#'
#' @details
#' The mathematical objective function of an optimization problem describes
#' the performance metric that is minimized or maximized during
#' optimization.
#' In a conservation planning [problem()], [objectives] specify the primary
#' metric should be maximized or minimized (e.g., [add_min_set_objective()]
#' specify that costs should be minimized) and [penalties] can
#' (optionally) be used to specify additional metrics that should be maximized
#' or minimized during optimization
#' (e.g., [add_boundary_penalties()] specify that spatial
#' fragmentation should be minimized).
#' Given this, the mathematical objective function of a
#' conservation planning [problem()] is calculated based on
#' a weighted sum of the [objectives] and [penalties]
#' (i.e., where the weights are the `penalty` values specified
#' in the [penalties] function).
#'
#' @return
#' A [tibble::tibble()] object describing the performance of the solution.
#' It contains the following columns.
#'
#' \describe{
#'
#' \item{problem}{`character` name of problem. Note that this column
#' is only present if `x` is a [multi_problem()] object.}
#'
#' \item{value}{`numeric` objective value.}
#'
#' }
#'
#' @name eval_objective_summary
#'
#' @inherit eval_n_summary seealso
#'
#' @family summaries
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # build conservation problem with boundary penalties
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # calculate objective value including penalties
#' v1 <- eval_objective_summary(p1, s1, include_penalties = TRUE)
#' print(v1)
#'
#' # calculate objective value excluding penalties
#' v2 <- eval_objective_summary(p1, s1, include_penalties = FALSE)
#' print(v2)
#' }
#' @export
eval_objective_summary <- function(x, solution, include_penalties = TRUE) {
  assert_required(x)
  assert_required(solution)
  assert_required(include_penalties)
  UseMethod("eval_objective_summary")
}

#' @rdname eval_objective_summary
#' @method eval_objective_summary ConservationProblem
#' @export
eval_objective_summary.ConservationProblem <- function(
  x, solution, include_penalties = TRUE
) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(solution)
  assert_required(include_penalties)
  assert(
    is_conservation_problem(x),
    assertthat::is.flag(include_penalties),
    assertthat::noNA(include_penalties)
  )
  # convert solution to status matrix format
  solution <- planning_unit_solution_status(x, solution)
  solution[is.na(solution)] <- 0
  # return result
  internal_eval_objective_summary(x, solution, include_penalties)
}

#' @rdname eval_objective_summary
#' @method eval_objective_summary MultiObjConservationProblem
#' @export
eval_objective_summary.MultiObjConservationProblem <- function(
  x, solution, include_penalties = TRUE
) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(solution)
  assert(
    is_multi_conservation_problem(x),
    assertthat::is.flag(include_penalties),
    assertthat::noNA(include_penalties)
  )
  # convert solution to status matrix format
  solution <- planning_unit_solution_status(x$problems[[1]], solution)
  solution[is.na(solution)] <- 0
  # run calculations
  out <- tibble::as_tibble(
    do.call(
      rbind,
      lapply(x$problems, function(y) {
        ## set solver for problem y based on x
        y$solver <- x$solver
        ## calculate objective value for y
        internal_eval_objective_summary(y, solution, include_penalties)
      })
    )
  )
  out$problem <- x$problem_names()
  # return result
  out[, c("problem", setdiff(names(out), "problem")), drop = FALSE]
}

internal_eval_objective_summary <- function(
  x, solution, include_penalties = TRUE, call = fn_caller_env()
) {
  # update problem for calculating objective value
  ## if solver is missing, then add default solver
  if (!inherits(x$solver, "Solver")) {
    x <- add_default_solver(x) # nocov
  }
  ## manually set gap to (approximately) 0
  old_gap <- x$solver$data$gap
  x$solver$data$gap <- 1e-6
  ## ensure that gap will get changed back to original value after
  ## the function has finished running
  on.exit(x$solver$data$gap <- old_gap, add = TRUE)
  ## manually remove penalties from problem,
  ## so that they do not influence objective value calculations
  if (!isTRUE(include_penalties)) {
    x <- x$remove_all_penalties()
  }
  # prepare problem for optimization
  ## compile problem
  o <- internal_compile(x, call = call)
  ## ensure that optimization problem is based on solution
  o$set_ub(replace(o$ub(), seq_along(solution), c(solution)))
  o$set_lb(replace(o$lb(), seq_along(solution), c(solution)))
  # solve problem
  sol <- x$solver$solve(o)
  if (is.null(sol$x) || is.null(sol$objective)) {
    # nocov start
    cli::cli_abort(
      message = c(
        "{.arg solution} is not feasible for {.arg x}.",
        "i" = paste(
          "This is because it does not meet the",
          "targets, budgets, or constraints."
        )
      ),
      call = rlang::expr(eval_objective_summary())
    )
    # nocov end
  }
  # calculate objective value
  tibble::tibble(value = sol$objective)
}
