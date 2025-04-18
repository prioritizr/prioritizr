#' @include Solver-class.R
NULL

#' Add a *SYMPHONY* solver with *Rsymphony*
#'
#' Specify that the [*SYMPHONY*](https://github.com/coin-or/SYMPHONY)
#' software -- using the \pkg{Rsymphony} package --
#' should be used to solve a conservation planning problem
#' (Ralphs & Güzelsoy 2005).
#' This function can also be used to customize the behavior of the solver.
#' It requires the \pkg{Rsymphony} package to be installed.
#'
#' @inheritParams add_gurobi_solver
#'
#' @details
#' [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) is an
#' open-source mixed integer programming solver that is part of the
#' Computational
#' Infrastructure for Operations Research (COIN-OR) project.
#' The \pkg{Rsymphony} package
#' provides an interface to COIN-OR and -- unlike dependencies for other
#' solvers -- is available on *CRAN*.
#' For information on the performance of different solvers,
#' please see Schuster _et al._ (2020) for benchmarks comparing the
#' run time and solution quality of different solvers when applied to
#' different sized datasets.
#'
#' @inherit add_gurobi_solver return references
#'
#' @seealso
#' See [solvers] for an overview of all functions for adding a solver.
#'
#' @family solvers
#'
#' @encoding UTF-8
#'
#' @references
#' Ralphs TK and Güzelsoy M (2005) The SYMPHONY callable library for mixed
#' integer programming. In The Next Wave in Computing, Optimization, and
#' Decision Technologies (pp. 61--76). Springer, Boston, MA.
#'
#' Schuster R, Hanson JO, Strimas-Mackey M, and Bennett JR (2020). Exact
#' integer linear programming solvers outperform simulated annealing for
#' solving conservation planning problems. *PeerJ*, 8: e9258.
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
#'   add_rsymphony_solver(time_limit = 10, verbose = FALSE)
#'
#' # generate solution
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution", axes = FALSE)
#' }
#' @name add_rsymphony_solver
NULL

#' @rdname add_rsymphony_solver
#' @export
add_rsymphony_solver <- function(x, gap = 0.1,
                                 time_limit = .Machine$integer.max,
                                 first_feasible = FALSE,
                                 verbose = TRUE) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(gap)
  assert_required(time_limit)
  assert_required(first_feasible)
  assert_required(verbose)
  assert(
    is_conservation_problem(x),
    assertthat::is.number(gap),
    all_finite(gap),
    gap >= 0,
    assertthat::is.number(time_limit),
    all_finite(time_limit),
    assertthat::noNA(time_limit),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose),
    assertthat::is.flag(first_feasible),
    assertthat::noNA(first_feasible),
    is_installed("Rsymphony")
  )
  # add solver
  x$add_solver(
    R6::R6Class(
      "RsymphonySolver",
      inherit = Solver,
      public = list(
        name = "rsymphony solver",
        data = list(
          gap = gap,
          time_limit = time_limit,
          first_feasible = first_feasible,
          verbose = verbose
        ),
        calculate = function(x, ...) {
          # create model
          model <- list(
            obj = x$obj(),
            mat = x$A(),
            dir = x$sense(),
            rhs = x$rhs(),
            types = x$vtype(),
            bounds = list(
              lower = list(ind = seq_along(x$lb()), val = x$lb()),
              upper = list(ind = seq_along(x$ub()), val = x$ub())
            ),
            max = isTRUE(x$modelsense() == "max")
          )
          # convert constraint matrix to sparse format
          model$dir <- replace(model$dir, model$dir == "=", "==")
          model$types <- replace(model$types, model$types == "S", "C")
          # prepare parameters
          p <- self$data
          p$verbosity <- -1
          if (!p$verbose)
            p$verbosity <- -2
          p <- p[names(p) != "verbose"]
          names(p)[which(names(p) == "gap")] <- "gap_limit"
          p$first_feasible <- as.logical(p$first_feasible)
          p$gap_limit <- p$gap_limit * 100
          # store internal data and parameters
          self$set_internal("model", model)
          self$set_internal("parameters", p)
          # return success
          invisible(TRUE)
        },
        set_variable_ub = function(index, value) {
          self$internal$model$bounds$upper$val[index] <- value
          invisible(TRUE)
        },
        set_variable_lb = function(index, value) {
          self$internal$model$bounds$lower$val[index] <- value
          invisible(TRUE)
        },
        set_constraint_rhs = function(index, value) {
          self$internal$model$rhs[index] <- value
          invisible(TRUE)
        },
        run = function() {
          # access input data and parameters
          model <- self$get_internal("model")
          p <- self$get_internal("parameters")
          # solve problem
          rt <- system.time({
            x <- do.call(Rsymphony::Rsymphony_solve_LP, append(model, p))
          })
          # manually return NULL to indicate error if no solution
          #nocov start
          if (is.null(x$solution) ||
              names(x$status) %in% c("TM_NO_SOLUTION", "PREP_NO_SOLUTION"))
            return(NULL)
          #nocov end
          # fix floating point issues with binary variables
          #nocov start
          b <- which(model$types == "B")
          if (any(x$solution[b] > 1)) {
            if (max(x$solution[b]) < 1.01) {
              x$solution[x$solution[b] > 1] <- 1
            } else {
              cli::cli_abort(
                c(
                  "Solver returned infeasible solution.",
                  "i" = paste(
                    "Try using a different solver or relaxing parameters",
                    "(e.g., {.arg gap} or {.arg time_limit})."
                  )
                ),
                call = rlang::expr(add_rsymphony_solver())
              )
            }
          }
          if (any(x$solution[b] < 0)) {
            if (min(x$solution[b]) > -0.01) {
              x$solution[x$solution[b] < 0] <- 0
            } else {
              cli::cli_abort(
                c(
                  "Solver returned infeasible solution.",
                  "i" = paste(
                    "Try using a different solver or relaxing parameters",
                    "(e.g., {.arg gap} or {.arg time_limit})."
                  )
                ),
                call = rlang::expr(add_rsymphony_solver())
              )
            }
          }
          #nocov end
          # fix floating point issues with continuous variables
          cv <- which(model$types == "C")
          x$solution[cv] <-
            pmax(x$solution[cv], self$internal$model$bounds$lower$val[cv])
          x$solution[cv] <-
            pmin(x$solution[cv], self$internal$model$bounds$upper$val[cv])
          # return output
          list(
            x = x$solution,
            objective = x$objval,
            status = as.character(x$status),
            runtime = rt[[3]],
            gap = NA_real_
          )
        }
      )
    )$new()
  )
}
