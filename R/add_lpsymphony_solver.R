#' @include Solver-proto.R
NULL

#' Add a SYMPHONY solver with \pkg{lpsymphony}
#'
#' Specify that the *SYMPHONY* software should be used to solve a
#' conservation planning problem using the \pkg{lpsymphony} package. This
#' function can also be used to customize the behavior of the solver.
#' It requires the \pkg{lpsymphony} package.
#'
#' @inheritParams add_rsymphony_solver
#'
#' @details [*SYMPHONY*](https://projects.coin-or.org/SYMPHONY) is an
#'   open-source integer programming solver that is part of the Computational
#'   Infrastructure for Operations Research (COIN-OR) project, an initiative
#'   to promote development of open-source tools for operations research (a
#'   field that includes linear programming). The \pkg{lpsymphony} package is
#'   distributed through
#'   [Bioconductor](https://doi.org/doi:10.18129/B9.bioc.lpsymphony).
#'   This functionality is provided because the \pkg{lpsymphony} package may
#'   be easier to install to install on Windows and Mac OSX systems than the
#'   \pkg{Rsymphony} package.
#'
#' @inherit add_rsymphony_solver seealso return
#'
#' @seealso [solvers()].
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions()
#' \donttest{
#' # if the package is installed then add solver and generate solution
#' # note that this solver is skipped on Linux systems due to the fact
#' # that the lpsymphony package randomly crashes on these systems
#' if (require(lpsymphony) &
#'     isTRUE(Sys.info()[["sysname"]] != "Linux")) {
#'   # specify solver and generate solution
#'   s <- p %>% add_lpsymphony_solver(time_limit = 5) %>%
#'              solve()
#'
#'   # plot solutions
#'   plot(stack(sim_pu_raster, s), main = c("planning units", "solution"))
#' }
#' }
#'
#' @name add_lsymphony_solver
NULL

#' @rdname add_lsymphony_solver
#' @export
add_lpsymphony_solver <- function(x, gap = 0.1, time_limit = -1,
                                  first_feasible = 0, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.scalar(gap),
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.scalar(time_limit),
                          assertthat::is.count(time_limit) || isTRUE(time_limit
                            == -1),
                          assertthat::is.flag(verbose),
                          assertthat::is.scalar(first_feasible),
                          isTRUE(first_feasible == 1 || isTRUE(first_feasible
                            == 0)),
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
