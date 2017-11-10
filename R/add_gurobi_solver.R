#' @include Solver-proto.R
NULL

#' Add a Gurobi solver
#'
#' Specify the use of a Gurobi algorithm to solve a
#' \code{\link{ConservationProblem-class}} object. Requires the \code{gurobi}
#' package.
#'
#' @details
#'  \href{http://gurobi.com}{Gurobi} is a
#'     state-of-the-art commercial optimization software with an R package
#'     interface. It is by far the fastest of the solvers available in this
#'     package, however, it is also the only solver that is not freely
#'     available. That said, licenses are available to academics at no cost. The
#'     \code{gurobi} package is distributed with the Gurobi software suite.
#'     This solver uses the \code{gurobi} package to solve problems.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param gap \code{numeric} gap to optimality. This gap is relative when
#'   solving problems using \code{gurobi}, and will cause the optimizer to
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
#' @param verbose \code{logical} should information be printed while solving
#'  optimization problems?
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
#' if (requireNamespace("gurobi", quietly = TRUE)) {
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

#' @export
methods::setClass("GurobiSolver", contains = "Solver")

#' @rdname add_gurobi_solver
#' @export
add_gurobi_solver <- function(x, gap = 0.1, time_limit = .Machine$integer.max,
                              presolve = 2, threads = 1, first_feasible = 0,
                              verbose = TRUE) {
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
                          assertthat::is.flag(verbose),
                          requireNamespace("gurobi", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "GurobiSolver",
    Solver,
    name = "Gurobi",
    parameters = parameters(
      numeric_parameter("gap", gap, lower_limit = 0),
      integer_parameter("time_limit", time_limit, lower_limit = -1L,
                        upper_limit = as.integer(.Machine$integer.max)),
      integer_parameter("presolve", presolve, lower_limit = 0L,
                        upper_limit = 2L),
      integer_parameter("threads", threads, lower_limit = 1L,
                        upper_limit = parallel::detectCores(TRUE)),
      binary_parameter("first_feasible", first_feasible),
      binary_parameter("verbose", verbose)),
    solve = function(self, x) {
      model <- list(
        modelsense = x$modelsense(),
        vtype = x$vtype(),
        obj = x$obj(),
        A = x$A(),
        rhs = x$rhs(),
        sense = x$sense(),
        lb = x$lb(),
        ub = x$ub())
      p <- list(LogToConsole = as.numeric(self$parameters$get("verbose")),
                Presolve = self$parameters$get("presolve"),
                MIPGap = self$parameters$get("gap"),
                TimeLimit = self$parameters$get("time_limit"),
                Threads = self$parameters$get("threads"),
                SolutionLimit = self$parameters$get("first_feasible"))
      if (p$SolutionLimit == 0)
        p$SolutionLimit <- NULL
      x <- gurobi::gurobi(model = model, params = p)
      if (file.exists("gurobi.log")) unlink("gurobi.log")
      return(list(x = x$x, objective = x$objval, status = x$status,
                  runtime = x$runtime))
    }))
}
