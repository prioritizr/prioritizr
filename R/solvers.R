#' @include Solver-proto.R
NULL

#' Problem solvers
#'
#' Specify the software and configurations used to solve a conservation planning 
#' problem. Below is a list of different solvers that can be added to a 
#' \code{\link{ConservationProblem}} object.
#'
#' \describe{
#'   \item{\code{add_gurobi_solver}}{\href{http://gurobi.com}{Gurobi} is a
#'   state-of-the-art commercial optimization software with an R package
#'   interface. It is by far the fastest of the solvers available in this
#'   package, however, it is also the only one that isn't free. That said, free
#'   academic licenses are available. The \code{gurobi} package is 
#'   distributed with the Gurobi software suite. This solver uses the
#'   \code{gurobi} package to solve problems.}
#'
#'   \item{\code{add_rsymphony_solver}}{
#'   \href{https://projects.coin-or.org/SYMPHONY}{SYMPHONY} is an open-source
#'   integer programming solver that is part of the Computational Infrastructure
#'   for Operations Research (COIN-OR) project, an initiative to promote
#'   development of open-source tools for operations research (a field that
#'   includes linear programming). The \code{Rsymphony} package provides an
#'   interface to COIN-OR and is available on CRAN. This solver uses the
#'   \code{Rsymphony} package to solve problems.}
#' 
#'  \item{\code{add_lpsymphony_solver}}{The \code{lpsymphony} package provides a
#'    different interface to the COIN-OR software suite. Unlike the 
#'    \code{Rsymhpony} package, the \code{lpsymphony} package is distributed
#'    through 
#'    \href{http://bioconducto/packages/release/bioc/html/lpsymphony.html}
#'    {Bioconductor}. On Windows and Mac, \code{lpsymphony} may be easier to
#'    may be easier to install. This solver uses the \code{lpsymphony} package 
#'    to solve.}
#'
#' \item{\code{default_solver}{This solver uses the best software installed 
#'    currently installed on the system.}}
#'
#' }
#'
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param gap \code{numeric} relative gap to optimality. The optimizer will
#'   terminate when the difference between the upper and lower objective
#'   function bounds is less than the gap times the upper bound. For example, a
#'   value of 0.01 will result in the optimizer stopping when the difference
#'   between the bounds is 1 percent of the upper bound.
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
#'   one thread being used. Currently only implemented for \code{gurobi_solver}.
#'
#' @examples
#'
#' # create basic problem and use defaults
#' p <- problem(cost=sim_pu_raster, features=sim_features) %>%
#'   add_minimium_set_objective() %>%
#'   add_relative_targets(0.1)
#'
#' # add rsymphony solver with default parameters
#' p %>% add_lpsymphony_solver()
#'
#' # add rsymphony solver with custom parameters
#' p %>% add_lpsymphony_solver(gap=0.1, time_limit=100)
#'
#' \dontrun{
#' # add gurobi solver
#' p %>% add_gurobi_solver(gap=0.1, presolve=2, time_limit=100)
#'
#' # add lpsolver solver
#' p %>% add_lpsymphony_solver(gap=0.1, time_limit=100)
#'
#' }
#'
#' @name solvers
NULL

#' @rdname solvers
#' @export
add_gurobi_solver <- function(x, gap=0.1, time_limit=.Machine$integer.max,
                              presolve=2,threads=1) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'),
    isTRUE(all(is.finite(gap))), assertthat::is.scalar(gap), isTRUE(gap <= 1),
    isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))), 
    assertthat::is.count(time_limit), isTRUE(all(is.finite(presolve))), 
    assertthat::is.count(presolve), isTRUE(presolve <= 2), 
    isTRUE(all(is.finite(threads))), assertthat::is.count(threads),
    isTRUE(threads <= parallel::detectCores()))
  # add solver
  x$add_solver(pproto(
    'GurobiSolver',
    Solver,
    name='gurobi',
    parameters=parameters(
      integer_parameter('presolve', presolve, lower_limit=0L, upper_limit=2L),
      proportion_parameter('gap', gap),
      integer_parameter('time_limit', time_limit, lower_limit=-1, 
        upper_limit=.Machine$integer.max),
      integer_parameter('threads', threads, lower_limit=1L, 
        upper_limit=parallel::detectCores())),
    solve=function(self, x) {
      model <- list(
        modelsense = x$modelsense(),
        vtype = x$vtype(),
        obj = x$obj(),
        A = x$A(),
        rhs = x$rhs(),
        sense = x$sense(),
        lb = x$lb(),
        ub = x$ub())
      p <- as.list(self$parameters)
      names(p) <- c('Presolve', 'MIPGap', 'TimeLimit', 'Threads')
      do.call(gurobi::gurobi, append(list(model), p))$x
    }))
  # return problem
  return(x)
}

#' @rdname solvers
#' @export
add_lpsymphony_solver <- function(x, gap=0.1, time_limit=-1, verbosity=1,
                                  first_feasible=0) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'),
    isTRUE(all(is.finite(gap))), assertthat::is.scalar(gap), isTRUE(gap <= 1), 
    isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
    assertthat::is.scalar(time_limit),
    assertthat::is.count(time_limit) || isTRUE(time_limit==-1),
    isTRUE(all(is.finite(verbosity))), assertthat::is.count(abs(verbosity)),
    isTRUE(verbosity <= 1), isTRUE(verbosity >= -2),
    assertthat::is.scalar(first_feasible), isTRUE(first_feasible<=1),
    isTRUE(first_feasible>=0))
  # add solver
  x$add_solver(pproto(
    'LpsymphonySolver',
    Solver,
    name='lpsymphony',
    parameters=parameters(
      integer_parameter('verbosity', verbosity, lower_limit=-2L, 
        upper_limit=1L),
      proportion_parameter('gap',gap),
      integer_parameter('time_limit', time_limit, lower_limit=-1,
        upper_limit=.Machine$integer.max),
      binary_parameter('first_feasible', first_feasible)),
    solve=function(self, x) {
      model = list(
        obj = x$obj(),
        mat = x$A(),
        dir = x$sense(),
        rhs = x$rhs(),
        types = x$vtype(),
        bounds = list(lower = x$lb(), upper=x$ub()),
        max = x$modelsense()=='max')
      args <- append(model, as.list(self$parameters))
      args$first_feasible <- as.logical(args$first_feasible)
      do.call(lpsymphony::symphony_solve_LP, args)$solution
    }))
  # return problem
  return(x)
}

#' @rdname solvers
#' @export
add_rsymphony_solver <- function(gap=0.1, time_limit=-1, first_feasible=0,
                             verbosity=1) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'),
    isTRUE(all(is.finite(gap))), assertthat::is.scalar(gap), isTRUE(gap <= 1), 
    isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
    assertthat::is.scalar(time_limit),
    assertthat::is.count(time_limit) || isTRUE(time_limit==-1),
    isTRUE(all(is.finite(verbosity))), assertthat::is.count(abs(verbosity)),
    isTRUE(verbosity <= 1), isTRUE(verbosity >= -2),
    assertthat::is.scalar(first_feasible), isTRUE(first_feasible<=1),
    isTRUE(first_feasible>=0))
  # add solver
  x$add_solver(pproto(
    'RsymphonySolver',
    Solver,
    name='rsymphony',
    parameters=parameters(
      integer_parameter('verbosity', verbosity, lower_limit=-2L, upper_limit=1L),
      proportion_parameter('gap',gap),
      integer_parameter('time_limit', time_limit, lower_limit=-1, 
        upper_limit=.Machine$integer.max),
      binary_parameter('first_feasible', first_feasible)),
    solve=function(self, x) {
      model = list(
        obj = x$obj(),
        mat = x$A(),
        dir = x$sense(),
        rhs = x$rhs(),
        types = x$vtype(),
        bounds = list(lower = x$lb(), upper=x$ub()),
        max = x$modelsense()=='max')
      args <- append(model, as.list(self$parameters))
      args$first_feasible <- as.logical(args$first_feasible)
      do.call(Rsymphony::Rsymphony_solve_LP, args)$solution
    }))
  # return problem
  return(x)
}

#' @rdname solvers
#' @export
add_default_solver <- function(x) {
  if (requireNamespace("gurobi", quietly = TRUE)) {
    return(add_gurobi_solver(x))
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    return(add_lpsymphony_solver(x))
  } else {
    return(add_rsymphony_solver(x))
  }
}

