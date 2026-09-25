#' @include Solver-class.R
NULL

#' Add a *SCIP* solver
#'
#' Specify that the [*SCIP*](https://www.scipopt.org/) (Solving Constraint
#' Integer Programs) software
#' should be used to solve a conservation planning problem
#' (Achterberg *et al.* 2008, Achterberg 2009).
#' This function can also be used to
#' customize the behavior of the solver.
#' It requires the \pkg{scip} package to be installed.
#'
#' @inheritParams add_gurobi_solver
#'
#' @param control `list` with additional parameters for tuning
#' the optimization process.
#' For example, `control = list(mem_limit = 200)` could be used to
#' set the `mem_limit` parameter.
#' See [scip::scip_control()] for information on the parameters.
#'
#' @details
#' [*SCIP*](https://www.scipopt.org/) is an open source optimization software.
#' It is not recommended to use this solver because it tends to have the slowest
#' performance.
#'
#' @inherit add_gurobi_solver return seealso
#'
#' @family solvers
#'
#' @references
#' Achterberg T, Berthold T, Koch T, and Wolter K (2008)
#' Integration of AI and OR techniques in constraint programming for
#' combinatorial optimization problems, CPAIOR 2008, LNCS 5015, pp. 6--20.
#'
#' Achterberg T (2009) SCIP: solving constraint integer programs
#' *Mathematical programming computation*, 1: 1--41.
#'
#' @examplesIf asNamespace("prioritizr")$do_run_example("scip")
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create problem
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_scip_solver(gap = 0, verbose = FALSE)
#'
#' # generate solution
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution", axes = FALSE)
#'
#' @name add_scip_solver
NULL

#' @rdname add_scip_solver
#' @export
add_scip_solver <- function(x, gap = 0.1, time_limit = .Machine$integer.max,
                            presolve = TRUE, threads = 1,
                            first_feasible = FALSE, verbose = TRUE,
                            control = list()) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(gap)
  assert_required(time_limit)
  assert_required(presolve)
  assert_required(threads)
  assert_required(first_feasible)
  assert_required(verbose)
  assert_required(control)
  assert(
    is_generic_conservation_problem(x),
    assertthat::is.number(gap),
    all_finite(gap),
    gap >= 0,
    assertthat::is.count(time_limit),
    all_finite(time_limit),
    assertthat::is.flag(presolve),
    assertthat::noNA(presolve),
    is_thread_count(threads),
    assertthat::is.flag(first_feasible),
    assertthat::noNA(first_feasible),
    assertthat::is.flag(verbose),
    is.list(control),
    is_installed("scip")
  )
  # additional argument validation
  verify(is_recommended_thread_count(threads))
  # additional checks for control
  if (length(control) > 0) {
    assert(
      !is.null(names(control)),
      all(nzchar(names(control))),
      msg = "all elements in {.arg control} must have a name."
    )
  }
  # add solver
  x$add_solver(
    R6::R6Class(
      "ScipSolver",
      inherit = Solver,
      public = list(
        name = "scip solver",
        data = list(
          gap = gap,
          time_limit = time_limit,
          presolve = presolve,
          threads = threads,
          first_feasible = first_feasible,
          verbose = verbose,
          control = control
        ),
        calculate = function(x, ...) {
          # create problem
          model <- list(
            obj = x$obj(),
            vtype = x$vtype(),
            A = x$A(),
            b = x$rhs(),
            sense = x$sense(),
            lb = x$lb(),
            ub = x$ub()
          )
          # update model sense
          model$sense[model$sense == "="] <- "=="
          # set model sense
          if (identical(x$modelsense(), "max")) {
            model$obj <- model$obj * -1
          }
          # assert that model contains valid variable types
          assert(
            all(model$vtype %in% c("B", "I", "C")),
            msg = "Solver does not support semi-continuous variables.",
            call = rlang::expr(add_scip_solver())
          )
          # create parameters
          p <- list(
            verbose = self$get_data("verbose"),
            presolving = self$get_data("presolve"),
            gap_limit = self$get_data("gap"),
            time_limit = self$get_data("time_limit"),
            threads = self$get_data("threads"),
            sol_limit = ifelse(self$get_data("first_feasible"), 1L, -1L)
          )
          # specify custom parameters
          control <- self$get_data("control")
          if (length(control) > 0) {
            p[names(control)] <- control
          }
          # store internal data and parameters
          self$set_internal("model", model)
          self$set_internal("parameters", p)
          # return success
          invisible(TRUE)
        },
        set_variable_ub = function(index, value) {
          self$internal$model$ub[index] <- value
          invisible(TRUE)
        },
        set_variable_lb = function(index, value) {
          self$internal$model$lb[index] <- value
          invisible(TRUE)
        },
        set_constraint_rhs = function(index, value) {
          self$internal$model$b[index] <- value
          invisible(TRUE)
        },
        run = function() {
          # access internal data and parameters
          model <- self$get_internal("model")
          p <- self$get_internal("parameters")
          # solve problem
          rt <- system.time({
            x <- do.call(
              scip::scip_solve,
              append(
                model,
                list(control = do.call(scip::scip_control, p)))
            )
          })
          # manually return NULL to indicate error if no solution
          # nocov start
          if (
            is.null(x) ||
            is.null(x$x) ||
            any(is.na(x$x)) ||
            identical(x$status, "infeasible")
          ) {
            return(NULL)
          }
          # nocov end
          # extract solution values
          sol <- x$x
          # sanitize solver output
          if (is.numeric(sol)) {
            sol <- sanitize_solver_output(
              sol,
              lb = model$lb, ub = model$ub,
              is_integer = model$vtype %in% c("I", "B")
            )
          }
          # return solution
          list(
            x = sol,
            objective = x$objval,
            status = x$status,
            runtime = rt[[3]],
            gap = x$gap,
            objbound = NA_real_
          )
        }
      )
    )$new()
  )
}
