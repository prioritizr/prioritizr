#' @include Solver-proto.R
NULL

#' Add a SYMPHONY solver with lpsymphony
#'
#' Specify the use of a SYMPHONY algorithm to solve a
#' \code{\link{ConservationProblem-class}} object. Requires the
#' \code{lpsymphony} package.
#'
#' @details
#'    The \code{lpsymphony} package provides a
#'    different interface to the COIN-OR software suite. Unlike the
#'    \code{Rsymhpony} package, the \code{lpsymphony} package is distributed
#'    through
#'    \href{https://doi.org/doi:10.18129/B9.bioc.lpsymphony}{Bioconductor}.
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
#'   (e.g. \code{Rsymhpony}), this gap is absolute and expresses the acceptable
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
#' @param verbose \code{logical} should information be printed while solving
#'  optimization problems? Defaults to \code{TRUE}.
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
#' if (requireNamespace("lpsymphony", quietly = TRUE)) {
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

#' @export
methods::setClass("LpsymphonySolver", contains = "Solver")

#' @rdname add_lsymphony_solver
#' @export
add_lpsymphony_solver <- function(x, gap = 0.1, time_limit = -1,
                                  first_feasible = 0, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.scalar(gap), isTRUE(gap <= 1),
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.scalar(time_limit),
                          assertthat::is.count(time_limit) || isTRUE(time_limit
                            == -1),
                          assertthat::is.flag(verbose),
                          assertthat::is.scalar(first_feasible),
                          isTRUE(first_feasible == 1 || isTRUE(first_feasible
                            == 0)),
                          requireNamespace("lpsymphony", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "LpsymphonySolver",
    Solver,
    name = "Lpsymphony",
    parameters = parameters(
      proportion_parameter("gap", gap),
      integer_parameter("time_limit", time_limit, lower_limit = -1,
                        upper_limit = .Machine$integer.max),
      binary_parameter("first_feasible", first_feasible),
      binary_parameter("verbose", verbose)),
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
      p$verbosity <- -1
      if (!p$verbose)
        p$verbosity <- -2
      p <- p[names(p) != "verbose"]
      names(p)[which(names(p) == "gap")] <- "gap_limit"
      model$dir <- replace(model$dir, model$dir == "=", "==")
      model$types <- replace(model$types, model$types == "S", "C")
      p$first_feasible <- as.logical(p$first_feasible)
      start_time <- Sys.time()
      s <- do.call(lpsymphony::lpsymphony_solve_LP, append(model, p))
      end_time <- Sys.time()
      if (is.null(x$solution) ||
          names(x$status) %in% c("TM_NO_SOLUTION", "PREP_NO_SOLUTION"))
        return(NULL)
      return(list(x = x$solution, objective = x$objval,
                  status = as.character(x$status),
                  runtime = as.double(end_time - start_time,
                                      format = "seconds")))
    }))
}
