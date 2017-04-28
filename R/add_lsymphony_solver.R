#' @include Solver-proto.R
NULL

#' Add a SYMPHONY solver with lsymphony
#'
#' Specify the use of a SYMPHONY algorithm to solve a
#' \code{\link{ConservationProblem-class}} object. Requires the \code{lsymphony} package.
#'
#' @details
#'    The \code{lpsymphony} package provides a
#'    different interface to the COIN-OR software suite. Unlike the
#'    \code{Rsymhpony} package, the \code{lpsymphony} package is distributed
#'    through
#'    \href{http://bioconducto/packages/release/bioc/html/lpsymphony.html}{Bioconductor}.
#'    On Windows and Mac, \code{lpsymphony}
#'    may be easier to install. This solver uses the \code{lpsymphony} package
#'    to solve.
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
#'   @param verbosity \code{integer} how verbose should the solver be when
#'   reporting progress on solving the problem?
#'
#' @param ... arguments passed to the default solver.
#' 
#' @seealso \code{\link{solvers}}, \code{\link{add_gurobi_solver}}, \code{\link{add_rsymphony_solver}}
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
#' @name add_lsymphony_solver
NULL

#' @export
methods::setClass("LpsymphonySolver", contains = "Solver")

#' @rdname add_lsymphony_solver
#' @export
add_lpsymphony_solver <- function(x, gap = 0.1, time_limit = -1, verbosity = 1,
                                  first_feasible = 0) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(gap))), assertthat::is.scalar(gap), isTRUE(gap <= 1),
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.scalar(time_limit),
                          assertthat::is.count(time_limit) || isTRUE(time_limit == -1),
                          isTRUE(all(is.finite(verbosity))), assertthat::is.count(abs(verbosity)),
                          isTRUE(verbosity <= 1), isTRUE(verbosity >= -2),
                          assertthat::is.scalar(first_feasible),
                          isTRUE(first_feasible == 1 || isTRUE(first_feasible == 0)),
                          requireNamespace("lpsymphony", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "LpsymphonySolver",
    Solver,
    name = "Lpsymphony",
    parameters = parameters(
      integer_parameter("verbosity", verbosity, lower_limit = -2L,
                        upper_limit = 1L),
      proportion_parameter("gap", gap),
      integer_parameter("time_limit", time_limit, lower_limit = -1,
                        upper_limit = .Machine$integer.max),
      binary_parameter("first_feasible", first_feasible)),
    solve = function(self, x) {
      model <- list(
        obj = x$obj(),
        mat = as.matrix(x$A()),
        dir = x$sense(),
        rhs = x$rhs(),
        types = x$vtype(),
        bounds = list(lower = list(ind = seq_along(x$lb()), val = x$lb()),
                      upper = list(ind = seq_along(x$ub()), val = x$ub())),
        max = x$modelsense() == "max")
      p <- as.list(self$parameters)
      names(p)[which(names(p) == "gap")] <- "gap_limit"
      model$dir <- replace(model$dir, model$dir == "=", "==")
      model$types <- replace(model$types, model$types == "S", "C")
      p$first_feasible <- as.logical(p$first_feasible)
      s <- do.call(lpsymphony::lpsymphony_solve_LP, append(model, p))
      if (names(s$status) %in% c("TM_NO_SOLUTION", "PREP_NO_SOLUTION"))
        return(NULL)
      return(s$solution)
    }))
}
