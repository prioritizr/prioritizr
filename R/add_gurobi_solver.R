#' @include Solver-proto.R
NULL

#' Add a Gurobi Solver
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
#'   between the bounds is 1 percent of the upper bound. But for other solvers
#'   (eg. \code{Rsymhpony}), this gap is absolute and expresses the acceptable
#'   deviance from the optimal objective. For example, solving a
#'   minimum set objective problem with a gap of 5 will cause the solver
#'   to terminate when the cost of the solution is within 5 cost units
#'   from the optimal solution.
#'
#' @param time_limit \code{numeric} time limit in seconds to run the optimizer.
#'   The solver will return the current best solution when this time limit is
#'   exceeded.
#'
#' @param first_feasible \code{logical} should the first feasible solution be
#'   be returned? If \code{first_feasible} is set to \code{TRUE}, the solver
#'   will return the first solution it encounters that meets all the
#'   constraints, regardless of solution quality. Note that the first feasible
#'   solution is not an arbitrary solution, rather it is derived from the
#'   relaxed solution, and is therefore often reasonably close to optimality.
#'
#' @param threads \code{integer} number of threads to use for the
#'   optimization algorithm. The default value of 1 will result in only
#'   one thread being used.
#'
#' @param presolve \code{integer} number indicating how intensively the
#'   solver should try to simplify the problem before solving it. The default
#'   value of 2 indicates to that the solver should be very aggressive in
#'   trying to simplify the problem.
#'
#' @param ... arguments passed to the default solver.
#'
#' @seealso \code{\link{solvers}}, \code{\link{add_rsymphony_solver}}, \code{\link{add_lpsymphony_solver}}
#'
#' @examples
#' \donttest{
#' # load packages
#' require(gurobi)
#' require(lpsymphony)
#' require(Rsymphony)
#'
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create basic problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1)
#'
#' # create vector to store plot titles
#' titles <- c()
#'
#' # create empty stack to store solutions
#' s <- stack()
#'
#' # create problem with added rsymphony solver and limit the time spent
#' # searching for the optimal solution to 2 seconds
#' if (requireNamespace("Rsymphony", quietly = TRUE)) {
#'   titles <- c(titles, "Rsymphony (2s)")
#'   p1 <- p %>% add_rsymphony_solver(time_limit = 2)
#'   s <- addLayer(s, solve(p1))
#' }
#'
#' # create problem with added rsymphony solver and limit the time spent
#' # searching for the optimal solution to 5 seconds
#' if (requireNamespace("Rsymphony", quietly = TRUE)) {
#'   titles <- c(titles, "Rsymphony (5s)")
#'   p2 <- p %>% add_rsymphony_solver(time_limit = 5)
#'   s <- addLayer(s, solve(p2))
#' }
#'
#' # if the gurobi is installed: create problem with added gurobi solver
#' if (requireNamespace("gurobi", quietly = TRUE)) {
#'   titles <- c(titles, "gurobi (5s)")
#'   p3 <- p %>% add_gurobi_solver(gap = 0.1, presolve = 2, time_limit = 5)
#'   s <- addLayer(s, solve(p3))
#' }
#'
#' # if the lpsymphony is installed: create problem with added lpsymphony solver
#' if (requireNamespace("lpsymphony", quietly = TRUE)) {
#'   titles <- c(titles, "lpsymphony")
#'   p4 <- p %>% add_lpsymphony_solver(gap = 0.1, time_limit = 5)
#'   s <- addLayer(s, solve(p4))
#' }
#'
#' # plot solutions
#' plot(s, main = titles)
#' }
#'
#' @name add_gurobi_solver
NULL

#' @export
methods::setClass("GurobiSolver", contains = "Solver")

#' @rdname add_gurobi_solver
#' @export
add_gurobi_solver <- function(x, gap=0.1, time_limit=.Machine$integer.max,
                              presolve=2, threads=1, first_feasible=0) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.scalar(gap), isTRUE(gap <= 1),
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.count(time_limit),
                          isTRUE(all(is.finite(presolve))),
                          assertthat::is.count(presolve), isTRUE(presolve <= 2),
                          isTRUE(all(is.finite(threads))),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)),
                          assertthat::is.scalar(first_feasible),
                          isTRUE(first_feasible == 1 | first_feasible == 0),
                          requireNamespace("gurobi", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "GurobiSolver",
    Solver,
    name = "Gurobi",
    parameters = parameters(
      integer_parameter("presolve", presolve, lower_limit = 0L,
                        upper_limit = 2L),
      proportion_parameter("gap", gap),
      integer_parameter("time_limit", time_limit, lower_limit = -1L,
                        upper_limit = as.integer(.Machine$integer.max)),
      integer_parameter("threads", threads, lower_limit = 1L,
                        upper_limit = parallel::detectCores(TRUE)),
      binary_parameter("first_feasible", first_feasible)),
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
      p <- list(Presolve = self$parameters$get("presolve"),
                MIPGap = self$parameters$get("gap"),
                TimeLimit = self$parameters$get("time_limit"),
                Threads = self$parameters$get("threads"),
                SolutionLimit = self$parameters$get("first_feasible"))
      if (p$SolutionLimit == 0)
        p$SolutionLimit <- NULL
      x <- gurobi::gurobi(model = model, params = p)$x
      if (file.exists("gurobi.log")) unlink("gurobi.log")
      return(x)
    }))
}
