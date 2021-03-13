#' @include Solver-proto.R
NULL

#' Add a *SYMPHONY* solver with *Rsymphony*
#'
#' Specify that the [*SYMPHONY*](https://projects.coin-or.org/SYMPHONY)
#' software (Ralphs & Güzelsoy 2005) -- using the \pkg{Rsymphony} package --
#' should be used to solve a conservation planning [problem()].
#' This function can also be used to customize the behavior of the solver.
#' It requires the \pkg{Rsymphony} package to be installed.
#'
#' @inheritParams add_gurobi_solver
#'
#' @details
#' [*SYMPHONY*](https://projects.coin-or.org/SYMPHONY) is an
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
#' @inherit add_gurobi_solver seealso return references
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
#' data(sim_pu_raster, sim_features)
#'
#' # create problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_min_set_objective() %>%
#'      add_relative_targets(0.1) %>%
#'      add_binary_decisions() %>%
#'      add_rsymphony_solver(time_limit = 10, verbose = FALSE)
#'
#' # generate solution
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_rsymphony_solver
NULL

#' @rdname add_rsymphony_solver
#' @export
add_rsymphony_solver <- function(x, gap = 0.1,
                                 time_limit = .Machine$integer.max,
                                 first_feasible = FALSE, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.scalar(gap),
                          isTRUE(gap >= 0),
                          isTRUE(all(is.finite(time_limit))),
                          assertthat::is.scalar(time_limit),
                          assertthat::noNA(time_limit),
                          assertthat::is.flag(verbose),
                          assertthat::is.flag(first_feasible),
                          assertthat::noNA(first_feasible),
                          requireNamespace("Rsymphony", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "RsymphonySolver",
    Solver,
    name = "Rsymphony",
    data = list(),
    parameters = parameters(
      numeric_parameter("gap", gap, lower_limit = 0),
      integer_parameter("time_limit", time_limit, lower_limit = -1,
                        upper_limit = .Machine$integer.max),
      binary_parameter("first_feasible", first_feasible),
      binary_parameter("verbose", verbose)),
    calculate = function(self, x, ...) {
      # create model
      model <- list(
        obj = x$obj(),
        mat = x$A(),
        dir = x$sense(),
        rhs = x$rhs(),
        types = x$vtype(),
        bounds = list(lower = list(ind = seq_along(x$lb()), val = x$lb()),
                      upper = list(ind = seq_along(x$ub()), val = x$ub())),
        max = isTRUE(x$modelsense() == "max"))
      # convert constraint matrix to sparse format
      model$dir <- replace(model$dir, model$dir == "=", "==")
      model$types <- replace(model$types, model$types == "S", "C")
      # prepare parameters
      p <- as.list(self$parameters)
      p$verbosity <- 1
      if (!p$verbose)
        p$verbosity <- -2
      p <- p[names(p) != "verbose"]
      names(p)[which(names(p) == "gap")] <- "gap_limit"
      p$first_feasible <- as.logical(p$first_feasible)
      p$gap_limit <- p$gap_limit * 100
      # store input data and parameters
      self$set_data("model", model)
      self$set_data("parameters", p)
      # return success
      invisible(TRUE)
    },
    set_variable_ub = function(self, index, value) {
      self$data$model$bounds$upper$val[index] <- value
      invisible(TRUE)
    },
    set_variable_lb = function(self, index, value) {
      self$data$model$bounds$lower$val[index] <- value
      invisible(TRUE)
    },
    run = function(self) {
      # access input data and parameters
      model <- self$get_data("model")
      p <- self$get_data("parameters")
      # print parameters
      base::print("########## RSYMPHONY PARAMETERS ##############")
      base::print(p)
      base::print("##############################################")

      # solve problem
      rt <- system.time({
        x <- do.call(Rsymphony::Rsymphony_solve_LP, append(model, p))
      })
      if (is.null(x$solution) ||
          names(x$status) %in% c("TM_NO_SOLUTION", "PREP_NO_SOLUTION"))
        return(NULL)
      # fix floating point issues with binary variables
      b <- which(model$types == "B")
      if (any(x$solution[b] > 1)) {
        if (max(x$solution[b]) < 1.01) {
          x$solution[x$solution[b] > 1] <- 1
        } else {
          stop("infeasible solution returned, try relaxing solver parameters")
        }
      }
      if (any(x$solution[b] < 0)) {
        if (min(x$solution[b]) > -0.01) {
          x$solution[x$solution[b] < 0] <- 0
        } else {
          stop("infeasible solution returned, try relaxing solver parameters")
        }
      }
      # fix floating point issues with continuous variables
      cv <- which(model$types == "C")
      x$solution[cv] <-
        pmax(x$solution[cv], self$data$model$bounds$lower$val[cv])
      x$solution[cv] <-
        pmin(x$solution[cv], self$data$model$bounds$upper$val[cv])
      # return output
      list(
        x = x$solution,
        objective = x$objval,
        status = as.character(x$status),
        runtime = rt[[3]])
    }))
}
