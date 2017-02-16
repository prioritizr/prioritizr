#' @include Solver-proto.R
NULL

#' Problem solvers
#'
#' The software and configurations used to solve a conservation planning 
#' problem. Below is a list of different solvers that can be used.
#' \strong{Only a single solver type should be added  to a 
#' \code{ConservationProblem} object}.
#'
#' \describe{
#'   \item{\code{gurobi_solver}}{\href{http://gurobi.com}{Gurobi} is a
#'   state-of-the-art commercial optimization software with an R package
#'   interface. It is by far the fastest of the solvers available in this
#'   package, however, it is also the only one that isn't free. That said, free
#'   academic licenses are available. The \code{gurobi} package is 
#'   distributed with the Gurobi software suite. This solver uses the
#'   \code{gurobi} package to solve problems.}
#'
#'   \item{\code{rsymphony_solver}}{
#'   \href{https://projects.coin-or.org/SYMPHONY}{SYMPHONY} is an open-source
#'   integer programming solver that is part of the Computational Infrastructure
#'   for Operations Research (COIN-OR) project, an initiative to promote
#'   development of open-source tools for operations research (a field that
#'   includes linear programming). The \code{Rsymphony} package provides an
#'   interface to COIN-OR and is available on CRAN. This solver uses the
#'   \code{Rsymphony} package to solve problems.}
#'
#' 
#'  \item{\code{lpsymphony_solver}}{The \code{lpsymphony} package provides a
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
#' p <- problem(cost=sim_pu_raster, features=sim_features) + 
#'   minimium_set_objective() + 
#'   relative_targets(0.1)
#'
#' # add lpsymphony solver with default parameters
#' p + lpsymphony_solver()
#'
#' # add lpsymphony solver with custom parameters
#' p + lpsymphony_solver(gap=0.1, time_limit=100)
#'
#' \dontrun{
#' # add gurobi solver
#' p + gurobi_solver(MIPGap=0.1, Presolve=2, TimeLimit=100)
#'
#' # add Rsymhpony solver
#' p + absolute_targets(gap=0.1, time_limit=100)
#' }
#'
#' @name solvers
NULL

#' @rdname solvers
#' @export
gurobi_solver <- function(gap=0.1, time_limit=.Machine$integer.max, presolve=2,
                          threads=1) {
  pproto(
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
    })
}

#' @rdname solvers
#' @export
lpsymphony_solver <- function(gap=0.1, time_limit=-1, verbosity=1,
                              first_feasible=0) {
  # return solver object
  pproto(
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
    })
}

#' @rdname solvers
#' @export
rsymphony_solver <- function(gap=0.1, time_limit=-1, first_feasible=0,
                             verbosity=1) {
  pproto(
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
    })
}

#' @rdname solvers
#' @export
default_solver <- function() {
  if (assertthat::assert_that(requireNamespace("gurobi", quietly = TRUE))) {
    return(gurobi_solver())
  } else if (assertthat::assert_that(requireNamespace("lpsymphony",
             quietly = TRUE))) {
    return(lpsymphony_solver())
  } else if (assertthat::assert_that(requireNamespace("Rsymphony",
             quietly = TRUE))) {
    return(rsymphony_solver())
  } else {
    return(lpsolve_solver())
  }
}

