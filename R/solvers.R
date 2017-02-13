#' @include Solver-class.R
NULL

#' Problem solvers
#'
#' These functions are used to specify the method for solving conservation
#' planning problems. 
#'
#' @param ... arguments passed to solver function. The available arguments 
#'            depends on the solver specified. See below for details
#'            on valid arguments.
#'
#' @details Each solver uses a function from a different package for solving
#'          problems. As such the arguments available to customize the 
#'          process of solving the problem vary depending on the solver
#'          used. Below is list showing the table with the name of the solver
#'          and a link to the function used to solve the problem.
#'          \describe{
#'          \item{gurobi}{\code{\link[gurobi]{gurobi}}}
#'          \item{rsymphony_solver}{\code{\link[Rsymphony]{Rsymphony_solve_LP}}}
#'          \item{lpsymphony_solver}{\code{\link[lpsymphony]{symphony_solve_LP}}}
#'          }
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
gurobi_solver <- function(...) {
  # capture args
  args <- list(...)
  # set default parameters
  params <- parameters(integer_parameter('Presolve', 2L, range=c(0L, 2L)),
                         proportion_parameter('MIPGap',0.1),
                         integer_parameter('TimeLimit', .Machine$integer.max, range=c(-1, .Machine$integer.max)),
                         integer_parameter('Threads', 1L, range=c(1L, parallel::detectCores())))
  # add in user specified parameters
  for (i in seq_along(args)) {
    if (sum(!names(args)[i] %in% params$names())!=0)
      stop(paste0(names(args)[i], 'is not a valid argument in gurobi::gurobi'))
    params$set(names(args)[i], args[[i]])
  }
  # return solver object
  Solver$new(
    name='gurobi',
    parameters=params,
    f=function(x) {
      model <- list(
        modelsense = x$modelsense(),
        vtype = x$vtype(),
        obj = x$obj(),
        A = x$A(),
        rhs = x$rhs(),
        sense = x$sense(),
        lb = x$lb(),
        ub = x$ub())
      do.call(gurobi::gurobi, 
              append(list(model), as.list(self$parameters)))$x
    }
  )
}

#' @rdname solvers
#' @export
lpsymphony_solver <- function(...) {
  # capture args
  args <- list(...)
  # set default parameters
  params <- parameters(integer_parameter('verbosity', 1L, range=c(-2L, 1L)),
                         proportion_parameter('gap',0.1),
                         integer_parameter('time_limit', -1L, range=c(-1, .Machine$integer.max)),
                         binary_parameter('first_feasible', 0L))
  # add in user specified parameters
  for (i in seq_along(args)) {
    if (sum(!names(args)[i] %in% params$names())!=0)
      stop(paste0(names(args)[i], 'is not a valid argument in lpsymphony::symphony_solve_LP'))
    params$set(names(args)[i], args[[i]])
  }
  # return solver object
  Solver$new(
    name='lpsymphony',
    parameters=params,
    f=function(x) {
      model = list(
        obj = x$obj(),
        mat = x$A(),
        dir = x$sense(),
        rhs = x$rhs(),
        types = x$vtype(),
        bounds = list(lower = x$lb(), upper=x$ub()),
        max = x$modelsense()=='max',
      )
      args <- append(model, as.list(self$parameters))
      args$first_feasible <- as.logical(args$first_feasible)
      do.call(lpsymphony::symphony_solve_LP, args)$solution
    }
  )
}

#' @rdname solvers
#' @export
rsymphony_solver <- function(...) {
  # capture args
  args <- list(...)
  # set default parameters
  params <- parameters(integer_parameter('verbosity', 1, range=c(-2L, 1L)),
                         proportion_parameter('gap',0.1),
                         integer_parameter('time_limit', -1, range=c(-1, .Machine$integer.max)),
                         binary_parameter('first_feasible', 0))
  # add in user specified parameters
  for (i in seq_along(args)) {
    if (sum(!names(args)[i] %in% params$names())!=0)
      stop(paste0(names(args)[i], 'is not a valid argument in Rsymphony::symphony_solve_LP'))
    params$set(names(args)[i], args[[i]])
  }
  # return solver object
  Solver$new(
    name='rsymphony',
    parameters=params,
    f=function(x) {
      model = list(
        obj = x$obj(),
        mat = x$A(),
        dir = x$sense(),
        rhs = x$rhs(),
        types = x$vtype(),
        bounds = list(lower = x$lb(), upper=x$ub()),
        max = x$modelsense()=='max',
      )
      args <- append(model, as.list(self$parameters))
      args$first_feasible <- as.logical(args$first_feasible)
      do.call(Rsymphony::Rsymphony_solve_LP, args)$solution
    }
  )
}
