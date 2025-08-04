#' @include Solver-class.R
NULL

#' Add a *HiGHS* solver
#'
#' Specify that the [*HiGHS*](https://highs.dev/) software
#' should be used to solve a conservation planning problem
#' (Huangfu and Hall 2018). This function can also be used to
#' customize the behavior of the solver.
#' It requires the \pkg{highs} package to be installed.
#'
#' @inheritParams add_cplex_solver
#' @inheritParams add_gurobi_solver
#'
#' @param control `list` with additional parameters for tuning
#'  the optimization process.
#'  For example, `control = list(simplex_strategy = 1)` could be used to
#'  set the `simplex_strategy` parameter.
#'  See the [online documentation](https://ergo-code.github.io/HiGHS/dev/options/definitions/)
#'  for information on the parameters.
#'
#' @details
#' [*HiGHS*](https://highs.dev/) is an open source optimization software.
#' Although this solver can have comparable performance to the *CBC* solver
#' (i.e., [add_cbc_solver()]) for particular problems and is generally faster
#' than the *SYMPHONY* based solvers (i.e., [add_rsymphony_solver()],
#' [add_lpsymphony_solver()]), it can sometimes take much longer than the
#' *CBC* solver for particular problems. This solver is recommended if
#' the [add_gurobi_solver()], [add_cplex_solver()], [add_cbc_solver()] cannot
#' be used.
#'
#' @inherit add_gurobi_solver return seealso
#'
#' @family solvers
#'
#' @references
#' Huangfu Q and Hall JAJ (2018). Parallelizing the dual revised simplex
#' method. *Mathematical Programming Computation*, 10: 119-142.
#'
#' @examples
#' \dontrun{
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
#'   add_highs_solver(gap = 0, verbose = FALSE)
#'
#' # generate solution
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution", axes = FALSE)
#' }
#' @name add_highs_solver
NULL

#' @rdname add_highs_solver
#' @export
add_highs_solver <- function(x, gap = 0.1, time_limit = .Machine$integer.max,
                             presolve = TRUE, threads = 1,
                             verbose = TRUE,
                             control = list()) {
  # assert that arguments are valid (except start_solution)
  assert_required(x)
  assert_required(gap)
  assert_required(time_limit)
  assert_required(presolve)
  assert_required(threads)
  assert_required(verbose)
  assert_required(control)
  assert(
    is_conservation_problem(x),
    assertthat::is.number(gap),
    all_finite(gap),
    gap >= 0,
    assertthat::is.count(time_limit),
    all_finite(time_limit),
    assertthat::is.flag(presolve),
    assertthat::noNA(presolve),
    is_thread_count(threads),
    assertthat::is.flag(verbose),
    is.list(control),
    is_installed("highs")
  )
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
      "HighsSolver",
      inherit = Solver,
      public = list(
        name = "highs solver",
        data = list(
          gap = gap,
          time_limit = time_limit,
          presolve = presolve,
          threads = threads,
          verbose = verbose,
          control = control
        ),
        calculate = function(x, ...) {
          # prepare constraints
          ## extract info
          rhs <- x$rhs()
          sense <- x$sense()
          assert(
            all(sense %in% c("=", "<=", ">=")),
            msg = "Failed to prepare problem for {.pkg highs} package.",
            call = rlang::expr(add_highs_solver()),
            .internal = TRUE
          )
          ## initialize arguments
          row_lb <- numeric(length(rhs))
          row_ub <- numeric(length(rhs))
          ## set equality constraints
          idx <- which(sense == "=")
          row_lb[idx] <- rhs[idx]
          row_ub[idx] <- rhs[idx]
          ## set lte constraints
          idx <- which(sense == "<=")
          row_lb[idx] <- -Inf
          row_ub[idx] <- rhs[idx]
          ## set gte constraints
          idx <- which(sense == ">=")
          row_lb[idx] <- rhs[idx]
          row_ub[idx] <- Inf
          # create problem
          model <- list(
            maximum = identical(x$modelsense(), "max"),
            L = x$obj(),
            A = x$A(),
            lhs = row_lb,
            rhs = row_ub,
            types = x$vtype(),
            lower = x$lb(),
            upper = x$ub()
          )
          # round values < 1e-6 to zero and drop them
          model$A@x[abs(model$A@x) < 1e-6] <- 0
          model$A <- Matrix::drop0(model$A)
          model$L[abs(model$L) < 1e-6] <- 0
          # set variables types
          ## C = continuous (same as gurobi)
          ## I = integer (same as gurobi)
          ## SC = semi-continuous (not same as gurobi)
          ## binary type not supported, convert to integer gurobi)
          model$types[model$types == "B"] <- "I"
          model$types[model$types == "S"] <- "SC"
          # create parameters
          p <- list(
            log_to_console = self$get_data("verbose"),
            presolve = ifelse(self$get_data("presolve") > 0.5, "on", "off"),
            mip_rel_gap = self$get_data("gap"),
            time_limit = as.numeric(self$get_data("time_limit")),
            threads = self$get_data("threads")
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
          self$internal$model$upper[index] <- value
          invisible(TRUE)
        },
        set_variable_lb = function(index, value) {
          self$internal$model$lower[index] <- value
          invisible(TRUE)
        },
        set_constraint_rhs = function(index, value) {
          lb_idx <- is.finite(self$internal$model$lhs[index])
          ub_idx <- is.finite(self$internal$model$rhs[index])
          self$internal$model$lhs[index[lb_idx]] <- value[lb_idx]
          self$internal$model$rhs[index[ub_idx]] <- value[ub_idx]
          invisible(TRUE)
        },
        run = function() {
          # access internal data and parameters
          model <- self$get_internal("model")
          p <- self$get_internal("parameters")
          # solve problem
          rt <- system.time({
            x <- do.call(
              highs::highs_solve,
              append(model, list(control = do.call(highs::highs_control, p)))
            )
          })
          # manually return NULL to indicate error if no solution
          # nocov start
          if (
            is.null(x) ||
            is.null(x$primal_solution) ||
            any(is.na(x$primal_solution)) ||
            isTRUE(!x$status %in% c(7L, 11L, 12L, 13L, 14L))
          ) {
            return(NULL)
          }
          # nocov end
          # extract solution values
          sol <- x$primal_solution
          ## fix potential floating point arithmetic issues
          i <- model$types == "I"
          if (is.numeric(sol)) {
            ## round integer variables
            sol[i] <- round(sol[i])
            ## truncate variables to account for rounding issues
            sol <- pmax(sol, model$lower)
            sol <- pmin(sol, model$upper)
          }
          # extract optimality gap
          if (!is.null(x$info) && !is.null(x$info$mip_gap)) {
            x_gap <- x$info$mip_gap
          } else {
            x_gap <- NA_real_ # nocov
          }
          # return solution
          list(
            x = sol,
            objective = x$objective_value,
            status = x$status_message,
            runtime = rt[[3]],
            gap = x_gap,
            objbound = NA_real_
          )
        }
      )
    )$new()
  )
}
