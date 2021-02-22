#' @include Solver-proto.R
NULL

#' Add a *SYMPHONY* solver with *lpsymphony*
#'
#' Specify that the [*SYMPHONY*](https://projects.coin-or.org/SYMPHONY)
#' software (Ralphs & Güzelsoy 2005) -- using the \pkg{lpsymphony} package --
#' should be used to solve a conservation planning [problem()].
#' This function can also be used to customize the behavior of the solver.
#' It requires the \pkg{lpsymphony} package to be installed
#' (see below for installation instructions).
#'
#' @inheritParams add_gurobi_solver
#'
#' @details
#' [*SYMPHONY*](https://projects.coin-or.org/SYMPHONY) is an
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
#' @inherit add_rsymphony_solver seealso return references
#'
#' @seealso [solvers].
#'
#' @examples
#' \dontrun{
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.05) %>%
#'   add_proportion_decisions() %>%
#'   add_lpsymphony_solver(time_limit = 5, verbose = FALSE)
#'
#' # generate solution
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_lsymphony_solver
NULL

#' @rdname add_lsymphony_solver
#' @export
add_lpsymphony_solver <- function(x, gap = 0.1,
                                  time_limit = .Machine$integer.max,
                                  first_feasible = FALSE, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.scalar(gap),
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.scalar(time_limit),
                          assertthat::is.count(time_limit) || isTRUE(time_limit
                            == -1),
                          assertthat::is.flag(verbose),
                          assertthat::is.flag(first_feasible),
                          assertthat::noNA(first_feasible),
                          requireNamespace("lpsymphony", quietly = TRUE))
  # throw warning about bug in lpsymphony
  if (utils::packageVersion("lpsymphony") <= as.package_version("1.4.1"))
    warning(paste0("The solution may be incorrect due to a bug in ",
                   "lpsymphony, please verify that it is correct, ",
                   "or use a different solver to generate solutions"))
  # add solver
  x$add_solver(pproto(
    "LpsymphonySolver",
    Solver,
    name = "Lpsymphony",
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
        mat = as.matrix(x$A()),
        dir = x$sense(),
        rhs = x$rhs(),
        types = x$vtype(),
        bounds = list(lower = list(ind = seq_along(x$lb()), val = x$lb()),
                      upper = list(ind = seq_along(x$ub()), val = x$ub())),
        max = isTRUE(x$modelsense() == "max"))
      p <- as.list(self$parameters)
      p$verbosity <- -1
      if (!p$verbose)
        p$verbosity <- -2
      p <- p[names(p) != "verbose"]
      names(p)[which(names(p) == "gap")] <- "gap_limit"
      model$dir <- replace(model$dir, model$dir == "=", "==")
      model$types <- replace(model$types, model$types == "S", "C")
      p$first_feasible <- as.logical(p$first_feasible)
      p$gap_limit <- p$gap_limit * 100
      # store input data and parameters
      self$set_data("model", model)
      self$set_data("parameters", p)
      # return success
      invisible(TRUE)
    },
    run = function(self) {
      # access input data and parameters
      model <- self$get_data("model")
      p <- self$get_data("parameters")
      # solve problem
      start_time <- Sys.time()
      x <- do.call(lpsymphony::lpsymphony_solve_LP, append(model, p))
      end_time <- Sys.time()
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
      # return output
      return(list(x = x$solution, objective = x$objval,
                  status = as.character(x$status),
                  runtime = as.double(end_time - start_time,
                                      format = "seconds")))
    },
    set_variable_ub = function(self, index, value) {
      self$data$model$bounds$upper$val[index] <- value
      invisible(TRUE)
    },
    set_variable_lb = function(self, index, value) {
      self$data$model$bounds$lower$val[index] <- value
      invisible(TRUE)
    }))
}
