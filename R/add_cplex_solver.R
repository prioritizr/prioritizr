#' @include Solver-class.R
NULL

#' Add a *CPLEX* solver
#'
#' Specify that the
#' [*IBM CPLEX*](https://www.ibm.com/products/ilog-cplex-optimization-studio/cplex-optimizer) software
#' should be used to solve a conservation planning problem (IBM 2017) .
#' This function can also be used to customize the behavior of the solver.
#' It requires the \pkg{cplexAPI} package to be installed
#' (see below for installation instructions).
#'
#' @inheritParams add_gurobi_solver
#'
#' @param presolve `logical` attempt to simplify the
#'   problem before solving it? Defaults to `TRUE`.
#'
#' @details
#' [*IBM CPLEX*](https://www.ibm.com/products/ilog-cplex-optimization-studio/cplex-optimizer) is a
#' commercial optimization software. It is faster than
#' the available open source solvers (e.g., [add_lpsymphony_solver()] and
#' [add_rsymphony_solver()].
#' Although formal benchmarks examining the performance of this solver for
#' conservation planning problems have yet to be completed, preliminary
#' analyses suggest that it performs slightly slower than the *Gurobi*
#' solver (i.e., [add_gurobi_solver()]).
#' We recommend using this solver if the *Gurobi* solver is not available.
#' Licenses are available for the *IBM CPLEX* software to academics at no cost
#' (see <  https://www.ibm.com/products/ilog-cplex-optimization-studio/cplex-optimizer>).
#'
#' @section Installation:
#' The \pkg{cplexAPI} package is used to interface with *IBM CPLEX* software.
#' To install the package, the *IBM CPLEX* software must be installed
#' (see <https://www.ibm.com/products/ilog-cplex-optimization-studio/cplex-optimizer>). Next, the `CPLEX_BIN`
#' environmental variable must be set to specify the file path for the
#' *IBM CPLEX* software. For example, on a Linux system,
#' this variable can be specified by adding the following text to the
#' `~/.bashrc` file:
#' ```
#'   export CPLEX_BIN="/opt/ibm/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/cplex"
#' ```
#' Please Note that you may need to change the version number in the file path
#' (i.e., `"CPLEX_Studio128"`). After specifying the `CPLEX_BIN`
#' environmental variable, the \pkg{cplexAPI} package can be installed.
#' Since the \pkg{cplexAPI} package is not available on the
#' the Comprehensive R Archive Network (CRAN), it must be installed from
#' [its GitHub repository](https://github.com/cran/cplexAPI). To
#' install the \pkg{cplexAPI} package, please use the following code:
#' ```
#' if (!require(remotes)) install.packages("remotes")
#' remotes::install_github("cran/cplexAPI")
#' ```
#' For further details on installing this package, please consult the
#' [installation instructions](https://github.com/cran/cplexAPI/blob/master/inst/INSTALL).
#'
#' @inherit add_gurobi_solver return seealso
#'
#' @family solvers
#'
#' @references
#' IBM (2017) IBM ILOG CPLEX Optimization Studio CPLEX User's Manual.
#' Version 12 Release 8. IBM ILOG CPLEX Division, Incline Village, NV.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create problem
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_cplex_solver(gap = 0.1, time_limit = 5, verbose = FALSE)
#'
#' # generate solution
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution", axes = FALSE)
#' }
#' @name add_cplex_solver
NULL

#' @rdname add_cplex_solver
#' @export
add_cplex_solver <- function(x, gap = 0.1, time_limit = .Machine$integer.max,
                             presolve = TRUE, threads = 1, verbose = TRUE) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(gap)
  assert_required(time_limit)
  assert_required(presolve)
  assert_required(threads)
  assert_required(verbose)
  assert(
    is_conservation_problem(x),
    assertthat::is.number(gap),
    all_finite(gap),
    gap >= 0,
    assertthat::is.count(time_limit),
    all_finite(time_limit),
    assertthat::is.flag(presolve),
    assertthat::noNA(presolve),
    is_thread_count(threads),
    assertthat::noNA(threads),
    assertthat::is.flag(verbose),
    is_installed("cplexAPI")
  )
  # add solver
  x$add_solver(
    R6::R6Class(
      "CplexSolver",
      inherit = Solver,
      public = list(
        name = "cplex solver",
        data = list(
          gap = gap,
          time_limit = time_limit,
          threads = threads,
          presolve = presolve,
          verbose = verbose
        ),
        calculate = function(x, ...) {
          # create problem
          model <- list(
            modelsense = x$modelsense(),
            vtype = x$vtype(),
            obj = x$obj(),
            A = x$A(),
            A2 = cplex_matrix(x$A()),
            rhs = x$rhs(),
            sense = x$sense(),
            lb = x$lb(),
            ub = x$ub()
          )
          # format problem for CPLEX
          model$sense[model$sense == ">="] <- "G"
          model$sense[model$sense == "="] <- "E"
          model$sense[model$sense == "<="] <- "L"
          model$vtype[model$vtype == "S"] <- "C"
          # create parameters
          p <- list(
            verbose = as.integer(self$get_data("verbose")),
            presolve = as.integer(self$get_data("presolve")),
            gap = self$get_data("gap"),
            threads = self$get_data("threads"),
            time_limit = self$get_data("time_limit")
          )
          # store input data and parameters
          self$set_internal("model", model)
          self$set_internal("parameters", p)
          # return success
          invisible(TRUE)
        },
        run = function() {
          # access input data and parameters
          model <- self$get_internal("model")
          p <- self$get_internal("parameters")
          # solve problem
          rt <- system.time({
            x <- cplex(model, p)
          })
          # fix potential floating point arithmetic issues
          b <- model$vtype == "B"
          if (is.numeric(x$x)) {
            ## round binary variables because default precision is 1e-5
            x$x[b] <- round(x$x[b])
            ## truncate variables to account for rounding issues
            x$x <- pmax(x$x, model$lb)
            x$x <- pmin(x$x, model$ub)
          }
          # extract solution values, and
          # set values to NULL if any values have NA in result
          sol <- x$x
          if (any(is.na(sol))) sol <- NULL
          # return solution
          list(
            x = sol,
            objective = x$objval,
            status = x$status,
            runtime = rt[[3]]
          )
        },
        set_variable_ub = function(index, value) {
          self$internal$model$ub[index] <- value
          invisible(TRUE)
        },
        set_variable_lb = function(index, value) {
          self$internal$model$lb[index] <- value
          invisible(TRUE)
        }
      )
    )$new()
  )
}

cplex_error_wrap <- function(result, env = NULL) {
  if (!(identical(result, 0) || identical(result, 0L))) {
    if (!is.null(env)) {
      cplexAPI::closeEnvCPLEX(env)
    }
    cli::cli_abort(
      cplexAPI::errmsg(result),
      call = rlang::expr(add_cbc_solver()),
      .internal = TRUE
    )
  }
  invisible(TRUE)
}

cplex_matrix <- function(m) {
  # inspired by Rcplex:::toCPXMatrix function
  assert(inherits(m, "dgCMatrix"))
  matbeg <- m@p
  matcnt <- diff(c(m@p, length(m@x)))
  matind <- m@i
  matval <- m@x
  list(
    matbeg = as.integer(matbeg),
    matcnt = as.integer(matcnt),
    matind = as.integer(matind),
    matval = as.double(matval)
  )
}

cplex <- function(model, control) {
  # assert valid arguments
  assert(is.list(model), is.list(control))
  # prepare model data for CPLEX
  model$lb[which(!is.finite(model$lb) & model$lb < 0)] <-
    -1 * cplexAPI::CPX_INFBOUND
  model$lb[which(!is.finite(model$lb) & model$lb > 0)] <-
    cplexAPI::CPX_INFBOUND
  model$ub[which(!is.finite(model$ub) & model$ub < 0)] <-
    -1 * cplexAPI::CPX_INFBOUND
  model$ub[which(!is.finite(model$ub) & model$ub > 0)] <-
    cplexAPI::CPX_INFBOUND
  # create environment
  env <- cplexAPI::openEnvCPLEX()
  if (inherits(env, "cplexError")) {
    stop(cplexAPI::errmsg(env))
  }
  # set solving parameters
  ## verbose (parameter: CPX_PARAM_SCRIND)
  cplex_error_wrap(
    cplexAPI::setIntParmCPLEX(env, 1035, as.integer(control$verbose)),
    env
  )
  ## presolve (parameter: CPX_PARAM_PREIND)
  cplex_error_wrap(
    cplexAPI::setIntParmCPLEX(env, 1030, as.integer(control$presolve)),
    env
  )
  ## threads (parameter: CPX_PARAM_THREADS)
  cplex_error_wrap(
    cplexAPI::setIntParmCPLEX(env, 1067, as.integer(control$threads)),
    env
  )
  ## (relative) optimality gap (parameter: CPX_PARAM_EPGAP)
  cplex_error_wrap(
    cplexAPI::setDblParmCPLEX(env, 2009, as.double(control$gap)),
    env
  )
  ## time limit (parameter: CPX_PARAM_TILIM)
  cplex_error_wrap(
    cplexAPI::setDblParmCPLEX(env, 1039, as.double(control$time_limit)),
    env
  )
  # initialize problem
  p <- cplexAPI::initProbCPLEX(env)
  cplex_error_wrap(cplexAPI::chgProbNameCPLEX(env, p, "prioritizr"), env)
  # build problem
  result <- cplexAPI::copyLpwNamesCPLEX(
    env = env, lp = p,
    nCols = ncol(model$A),
    nRows = nrow(model$A),
    lpdir = ifelse(
      identical(model$modelsense, "max"),
      cplexAPI::CPX_MAX, cplexAPI::CPX_MIN
    ),
    objf = model$obj,
    rhs = model$rhs,
    sense = model$sense,
    lb = model$lb,
    ub = model$ub,
    matbeg = model$A2$matbeg,
    matcnt = model$A2$matcnt,
    matind = model$A2$matind,
    matval = model$A2$matval
  )
  if (!(identical(result, 0) || identical(result, 0L))) {
    cli::cli_abort(
      "Failed to prepare data for IBM CPLEX.",
      .internal = TRUE,
      call = rlang::expr(add_cplex_solver())
    )
  }
  # solve problem
  if (all(model$vtype == "C")) {
    result <- cplexAPI::lpoptCPLEX(env, p)
  } else {
    cplexAPI::copyColTypeCPLEX(env, p, model$vtype)
    result <- cplexAPI::mipoptCPLEX(env, p)
  }
  # extract solution
  stat <- cplexAPI::getStatStrCPLEX(env, cplexAPI::getStatCPLEX(env, p))
  if (identical(result, 0) || identical(result, 0L)) {
    sol <- cplexAPI::solutionCPLEX(env, p)
    if (!inherits(sol, "cplexError")) {
      out <- list(x = sol$x, objval = sol$objval, status = stat)
    } else {
      out <- list(x = NULL, objval = NULL, status = stat)
    }
  } else {
    out <- list(x = NULL, objval = NULL, status = stat)
  }
  # clean up
  cplex_error_wrap(cplexAPI::delProbCPLEX(env, p), env)
  cplex_error_wrap(cplexAPI::closeEnvCPLEX(env))
  # return result
  out
}
