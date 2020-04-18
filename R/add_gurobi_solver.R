#' @include Solver-proto.R
NULL

#' Add a \emph{Gurobi} solver
#'
#' Specify that the \emph{Gurobi} software should be used to solve a
#' conservation planning problem. This function can also be used to
#' customize the behavior of the solver. It requires the \pkg{gurobi} package.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param gap \code{numeric} gap to optimality. This gap is relative when
#'   solving problems using \pkg{gurobi}, and will cause the optimizer to
#'   terminate when the difference between the upper and lower objective
#'   function bounds is less than the gap times the upper bound. For example, a
#'   value of 0.01 will result in the optimizer stopping when the difference
#'   between the bounds is 1 percent of the upper bound.
#'
#' @param time_limit \code{numeric} time limit in seconds to run the optimizer.
#'   The solver will return the current best solution when this time limit is
#'   exceeded.
#'
#' @param presolve \code{integer} number indicating how intensively the
#'   solver should try to simplify the problem before solving it. The default
#'   value of 2 indicates to that the solver should be very aggressive in
#'   trying to simplify the problem.
#'
#' @param threads \code{integer} number of threads to use for the
#'   optimization algorithm. The default value of 1 will result in only
#'   one thread being used.
#'
#' @param first_feasible \code{logical} should the first feasible solution be
#'   be returned? If \code{first_feasible} is set to \code{TRUE}, the solver
#'   will return the first solution it encounters that meets all the
#'   constraints, regardless of solution quality. Note that the first feasible
#'   solution is not an arbitrary solution, rather it is derived from the
#'   relaxed solution, and is therefore often reasonably close to optimality.
#'   Defaults to \code{FALSE}.
#'
#' @param numeric_focus \code{logical} should extra attention be paid
#'   to verifying the accuracy of numerical calculations? This may be
#'   useful when dealing problems that may suffer from numerical instability
#'   issues. Beware that it will likely substantially increase run time
#'   (sets the \emph{Gurobi} \code{NumericFocus} parameter
#'   to 3). Defaults to \code{FALSE}.
#'
#' @param verbose \code{logical} should information be printed while solving
#'  optimization problems?
#'
#' @details \href{http://gurobi.com}{\emph{Gurobi}} is a
#'   state-of-the-art commercial optimization software with an R package
#'   interface. It is by far the fastest of the solvers available in this
#'   package, however, it is also the only solver that is not freely
#'   available. That said, licenses are available to academics at no cost. The
#'   \pkg{gurobi} package is distributed with the \emph{Gurobi} software suite.
#'   This solver uses the \pkg{gurobi} package to solve problems.
#'
#' @return \code{\link{ConservationProblem-class}} object with the solver added
#'   to it.
#'
#' @seealso \code{\link{solvers}}.
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
#' if (require("gurobi")) {
#'   # specify solver and generate solution
#'   s <- p %>% add_gurobi_solver(gap = 0.1, presolve = 2, time_limit = 5) %>%
#'              solve()
#'
#'   # plot solutions
#'   plot(stack(sim_pu_raster, s), main = c("planning units", "solution"),
#'        axes = FALSE, box = FALSE)
#' }
#' }
#' @name add_gurobi_solver
NULL

#' @rdname add_gurobi_solver
#' @export
add_gurobi_solver <- function(x, gap = 0.1, time_limit = .Machine$integer.max,
                              presolve = 2, threads = 1, first_feasible = 0,
                              numeric_focus = FALSE, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.scalar(gap),
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.count(time_limit),
                          isTRUE(all(is.finite(presolve))),
                          assertthat::is.count(presolve), isTRUE(presolve <= 2),
                          isTRUE(all(is.finite(threads))),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)),
                          assertthat::is.scalar(first_feasible),
                          isTRUE(first_feasible == 1 | first_feasible == 0),
                          assertthat::is.flag(numeric_focus),
                          assertthat::noNA(numeric_focus),
                          assertthat::is.flag(verbose),
                          requireNamespace("gurobi", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "GurobiSolver",
    Solver,
    name = "Gurobi",
    data = list(),
    parameters = parameters(
      numeric_parameter("gap", gap, lower_limit = 0),
      integer_parameter("time_limit", time_limit, lower_limit = -1L,
                        upper_limit = as.integer(.Machine$integer.max)),
      integer_parameter("presolve", presolve, lower_limit = 0L,
                        upper_limit = 2L),
      integer_parameter("threads", threads, lower_limit = 1L,
                        upper_limit = parallel::detectCores(TRUE)),
      binary_parameter("first_feasible", first_feasible),
      binary_parameter("numeric_focus", numeric_focus),
      binary_parameter("verbose", verbose)),
    calculate = function(self, x, ...) {
      # create problem
      model <- list(
        modelsense = x$modelsense(),
        vtype = x$vtype(),
        obj = x$obj(),
        A = x$A(),
        rhs = x$rhs(),
        sense = x$sense(),
        lb = x$lb(),
        ub = x$ub())
      # create parameters
      p <- list(LogToConsole = as.numeric(self$parameters$get("verbose")),
                LogFile = "",
                Presolve = self$parameters$get("presolve"),
                MIPGap = self$parameters$get("gap"),
                TimeLimit = self$parameters$get("time_limit"),
                Threads = self$parameters$get("threads"),
                NumericFocus = self$parameters$get("numeric_focus"),
                SolutionLimit = self$parameters$get("first_feasible"))
      if (p$SolutionLimit == 0)
        p$SolutionLimit <- NULL
      # add extra parameters from portfolio if needed
      p2 <- list(...)
      for (i in seq_along(p2))
        p[[names(p2)[i]]] <- p2[[i]]
      # store input data and parameters
      self$set_data("model", model)
      self$set_data("parameters", p)
      # return success
      invisible(TRUE)
    },
    set_variable_ub = function(self, index, value) {
      self$data$model$ub[index] <- value
      invisible(TRUE)
    },
    set_variable_lb = function(self, index, value) {
      self$data$model$lb[index] <- value
      invisible(TRUE)
    },
    run = function(self, x) {
      # access input data and parameters
      model <- self$get_data("model")
      p <- self$get_data("parameters")
      # solve problem
      x <- gurobi::gurobi(model = model, params = p)
      # fix potential floating point arithmetic issues
      b <- model$vtype == "B"
      if (is.numeric(x$x)) {
        ## round binary variables because default precision is 1e-5
        x$x[b] <- round(x$x[b])
        ## truncate semicontinuous variables
        v <- model$vtype == "S"
        x$x[v] <- pmax(x$x[v], 0)
        x$x[v] <- pmin(x$x[v], 1)
        ## truncate variables to account for rounding issues
        x$x <- pmax(x$x, model$lb)
        x$x <- pmin(x$x, model$ub)
      }
      # extract solutions
      out <- list(x = x$x, objective = x$objval, status = x$status,
                 runtime = x$runtime)
      # add pool if required
      if (!is.null(p$PoolSearchMode) && is.numeric(x$x) &&
          isTRUE(length(x$pool) > 1)) {
        out$pool <- x$pool[-1]
        for (i in seq_len(length(out$pool))) {
          out$pool[[i]]$xn[b] <- round(out$pool[[i]]$xn[b])
          out$pool[[i]]$status <-
            ifelse(abs(out$pool[[i]]$objval - x$objval) < 1e-5,
                   "OPTIMAL", "SUBOPTIMAL")
        }
      }
      out
    }))
}
