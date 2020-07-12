#' @include Solver-proto.R
NULL

#' Add a SYMPHONY solver with *Rsymphony*
#'
#' Specify that the *SYMPHONY* software should be used to solve a
#' conservation planning problem using the \pkg{Rsymphony} package. This
#' function can also be used to customize the behavior of the solver.
#' It requires the \pkg{Rsymphony} package.
#'
#' @param x [problem()] (i.e. [`ConservationProblem-class`]) object.
#'
#' @param gap `numeric` gap to optimality. This gap is absolute and
#'   expresses the acceptable deviance from the optimal objective. For example,
#'   solving a minimum set objective problem with a gap of 5 will cause the
#'   solver to terminate when the cost of the solution is within 5 cost units
#'   from the optimal solution.
#'
#' @param time_limit `numeric` time limit in seconds to run the optimizer.
#'   The solver will return the current best solution when this time limit is
#'   exceeded.
#'
#' @param first_feasible `logical` should the first feasible solution be
#'   be returned? If `first_feasible` is set to `TRUE`, the solver
#'   will return the first solution it encounters that meets all the
#'   constraints, regardless of solution quality. Note that the first feasible
#'   solution is not an arbitrary solution, rather it is derived from the
#'   relaxed solution, and is therefore often reasonably close to optimality.
#'
#' @param verbose `logical` should information be printed while solving
#'  optimization problems? Defaults to `TRUE`.
#'
#' @details [*SYMPHONY*](https://projects.coin-or.org/SYMPHONY) is an
#'   open-source integer programming solver that is part of the Computational
#'   Infrastructure for Operations Research (COIN-OR) project, an initiative
#'   to promote development of open-source tools for operations research (a
#'   field that includes linear programming). The \pkg{Rsymphony} package
#'   provides an interface to COIN-OR and is available on *CRAN*.
#'   This solver uses the \pkg{Rsymphony} package to solve problems.
#'
#' @inherit add_gurobi_solver seealso return
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
#' if (require("Rsymphony")) {
#'   # specify solver and generate solution
#'   s <- p %>% add_rsymphony_solver(time_limit = 10) %>%
#'              solve()
#'
#'   # plot solutions
#'   plot(stack(sim_pu_raster, s), main = c("planning units", "solution"),
#'        axes = FALSE, box = FALSE)
#' }
#' }
#'
#' @name add_rsymphony_solver
NULL

#' @rdname add_rsymphony_solver
#' @export
add_rsymphony_solver <- function(x, gap = 0.1, time_limit = -1,
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
      p <- as.list(self$parameters)
      p$verbosity <- -1
      if (!p$verbose)
        p$verbosity <- -2
      p <- p[names(p) != "verbose"]
      model$dir <- replace(model$dir, model$dir == "=", "==")
      model$types <- replace(model$types, model$types == "S", "C")
      names(p)[which(names(p) == "gap")] <- "gap_limit"
      p$first_feasible <- as.logical(p$first_feasible)
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
      # solve problem
      start_time <- Sys.time()
      x <- do.call(Rsymphony::Rsymphony_solve_LP, append(model, p))
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
    }))
}
