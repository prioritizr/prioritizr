#' @include Solver-proto.R
NULL

#' Problem solvers
#'
#' These functions are used to specify the method for solving conservation
#' planning problems. 
#'
#' @param gap TODO
#'
#' @param time_limit TODO
#'
#' @param presolve TODO
#'
#' @param threads TODO
#'
#' @param verbosity TODO
#'
#' @param first_feasible TODO 
#'
#' @details Each solver uses a different package to solve
#'   problems. As such, the arguments available to customize the 
#'   process of solving the problem vary depending on the solver
#'   used. Below is list showing the table with the name of the solver
#'   and a link to the function used to solve the problem.
#'   \describe{
#'   \item{gurobi}{\code{\link[gurobi]{gurobi}}}
#'   \item{rsymphony_solver}{\code{\link[Rsymphony]{Rsymphony_solve_LP}}}
#'   \item{lpsymphony_solver}{\code{\link[lpsymphony]{symphony_solve_LP}}}
#'   }
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
#' @name solvers
NULL

#' @rdname solvers
#' @export
gurobi_solver <- function(gap=0.1, time_limit=.Machine$integer.max, presolve=2,
                          threads=1) {
  pproto(
    'Solver'
    Solver,
    name='gurobi',
    parameters=parameters(
      integer_parameter('presolve', presolve, range=c(0L, 2L)),
      proportion_parameter('gap', gap, 0.1),
      integer_parameter('time_limit', time_limit,
        range=c(-1, .Machine$integer.max)),
      integer_parameter('threads', threads,
        range=c(1L, parallel::detectCores()))),
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
    'Solver',
    Solver,
    name='lpsymphony',
    parameters=parameters(
      integer_parameter('verbosity', verbosity, range=c(-2L, 1L)),
      proportion_parameter('gap',gap),
      integer_parameter('time_limit', time_limit,
        range=c(-1, .Machine$integer.max)),
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
    'Solver',
    Solver,
    name='rsymphony',
    parameters=parameters(
      integer_parameter('verbosity', 1, range=c(-2L, 1L)),
      proportion_parameter('gap',0.1),
      integer_parameter('time_limit', -1, range=c(-1, .Machine$integer.max)),
      binary_parameter('first_feasible', 0)),
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
