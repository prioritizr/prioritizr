#' @include Solver-class.R
NULL

#' Add a *SYMPHONY* solver with *lpsymphony*
#'
#' Specify that the [*SYMPHONY*](https://github.com/coin-or/SYMPHONY)
#' software -- using the \pkg{lpsymphony} package --
#' should be used to solve a conservation planning problem
#' (Ralphs & Güzelsoy 2005).
#' This function can also be used to customize the behavior of the solver.
#' It requires the \pkg{lpsymphony} package to be installed
#' (see below for installation instructions).
#'
#' @inheritParams add_gurobi_solver
#'
#' @details
#' [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) is an
#' open-source mixed integer programming solver that is part of the
#' Computational Infrastructure for Operations Research (COIN-OR) project.
#' This solver is provided because it may be easier to install
#' on some systems than the \pkg{Rsymphony} package. Additionally --
#' although the \pkg{lpsymphony} package doesn't provide the functionality
#' to specify the number of threads for solving a problem -- the
#' \pkg{lpsymphony} package will solve problems using parallel processing
#' (unlike the \pkg{Rsymphony} package). As a consequence, this
#' solver will likely generate solutions much faster than the
#' [add_rsymphony_solver()].
#' Although formal benchmarks examining the performance of this solver
#' have yet to be completed,
#' please see Schuster _et al._ (2020) for benchmarks comparing the
#' run time and solution quality of the \pkg{Rsymphony} solver.
#'
#' @section Installation:
#' The \pkg{lpsymphony} package is
#' distributed through
#' [Bioconductor](https://doi.org/doi:10.18129/B9.bioc.lpsymphony).
#' To install the \pkg{lpsymphony} package, please use the following code:
#' ```
#' if (!require(remotes)) install.packages("remotes")
#' remotes::install_bioc("lpsymphony")
#' ```
#'
#' @inherit add_rsymphony_solver return references seealso
#'
#' @family solvers
#'
#' @encoding UTF-8
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
#'   add_relative_targets(0.05) %>%
#'   add_proportion_decisions() %>%
#'   add_lpsymphony_solver(time_limit = 5, verbose = FALSE)
#'
#' # generate solution
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution", axes = FALSE)
#' }
#' @name add_lsymphony_solver
NULL

#' @rdname add_lsymphony_solver
#' @export
add_lpsymphony_solver <- function(x, gap = 0.1,
                                  time_limit = .Machine$integer.max,
                                  first_feasible = FALSE, verbose = TRUE) {
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
    time_limit >= -1,
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose),
    assertthat::is.flag(first_feasible),
    assertthat::noNA(first_feasible),
    is_installed("lpsymphony"),
    is_installed("slam")
  )
  # throw error about bug in early version of lpsymphony
  assert(
    utils::packageVersion("lpsymphony") > as.package_version("1.4.1"),
    msg = c(
      "The version of {.pkg lpsymphony} currently installed is outdated.",
      "i" =
        "Please update it using: {.code remotes::install_bioc(\"lpsymphony\")}."
    )
  )

  # add solver
  x$add_solver(
    R6::R6Class(
      "LpsymphonySolver",
      inherit = Solver,
      public = list(
        name = "lpsymphony",
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
            mat = as_Matrix(x$A(), "dgTMatrix"),
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
          model$mat <- slam::simple_triplet_matrix(
            i = model$mat@i + 1,
            j = model$mat@j + 1, v = model$mat@x,
            nrow = nrow(model$mat),
            ncol = ncol(model$mat)
          )
          # prepare sense and variables types for SYMPHONY
          model$dir <- replace(model$dir, model$dir == "=", "==")
          model$types <- replace(model$types, model$types == "S", "C")
          # set parameters
          p <- self$data
          p$verbosity <- ifelse(p$verbose, -1, -2)
          p <- p[names(p) != "verbose"]
          names(p)[which(names(p) == "gap")] <- "gap_limit"
          p$gap_limit <- p$gap_limit * 100
          # store internal data and parameters
          self$set_internal("model", model)
          self$set_internal("parameters", p)
          # return success
          invisible(TRUE)
        },
        run = function() {
          # access internal data and parameters
          model <- self$get_internal("model")
          p <- self$get_internal("parameters")
          # solve problem
          rt <- system.time({
            x <- do.call(lpsymphony::lpsymphony_solve_LP, append(model, p))
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
                call = rlang::expr(add_lpsymphony_solver())
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
                call = rlang::expr(add_lpsymphony_solver())
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
        }
      )
    )$new()
  )
}
