#' @include Solver-proto.R
NULL

#' Add a *CPLEX* solver
#'
#' Specify that the
#' [*IBM CPLEX*](https://www.ibm.com/products/ilog-cplex-optimization-studio/cplex-optimizer) software
#' (IBM 2017) should be used to solve a conservation planning [problem()].
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
#' data(sim_pu_raster, sim_features)
#'
#' # create problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_min_set_objective() %>%
#'      add_relative_targets(0.1) %>%
#'      add_binary_decisions() %>%
#'      add_cplex_solver(gap = 0.1, time_limit = 5, verbose = FALSE)
#'
#' # generate solution
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_cplex_solver
NULL

#' @rdname add_cplex_solver
#' @export
add_cplex_solver <- function(x, gap = 0.1, time_limit = .Machine$integer.max,
                              presolve = TRUE, threads = 1, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.scalar(gap),
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.count(time_limit),
                          assertthat::is.flag(presolve),
                          assertthat::noNA(presolve),
                          assertthat::is.count(threads),
                          assertthat::noNA(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)),
                          assertthat::is.flag(verbose),
                          requireNamespace("cplexAPI", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "CplexSolver",
    Solver,
    name = "CPLEX",
    data = list(),
    parameters = parameters(
      numeric_parameter("gap", gap, lower_limit = 0),
      integer_parameter("time_limit", time_limit, lower_limit = 0L,
                        upper_limit = as.integer(.Machine$integer.max)),
      integer_parameter("threads", threads, lower_limit = 1L,
                        upper_limit = parallel::detectCores(TRUE)),
      binary_parameter("presolve", presolve),
      binary_parameter("verbose", verbose)),
    calculate = function(self, x, ...) {
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
        ub = x$ub())
      # format problem for CPLEX
      model$sense[model$sense == ">="] <- "G"
      model$sense[model$sense == "="] <- "E"
      model$sense[model$sense == "<="] <- "L"
      model$vtype[model$vtype == "S"] <- "C"
      # create parameters
      p <- list(verbose = as.integer(self$parameters$get("verbose")),
                presolve = as.integer(self$parameters$get("presolve")),
                gap = self$parameters$get("gap"),
                threads = self$parameters$get("threads"),
                time_limit = self$parameters$get("time_limit"))
      # store input data and parameters
      self$set_data("model", model)
      self$set_data("parameters", p)
      # return success
      invisible(TRUE)
    },
    run = function(self, x) {
      # access input data and parameters
      model <- self$get_data("model")
      p <- self$get_data("parameters")
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
        runtime = rt[[3]])
    },
    set_variable_ub = function(self, index, value) {
      self$data$model$ub[index] <- value
      invisible(TRUE)
    },
    set_variable_lb = function(self, index, value) {
      self$data$model$lb[index] <- value
      invisible(TRUE)
    }))
}

cplex_error_wrap <- function(result, env = NULL) {
  if (!(identical(result, 0) || identical(result, 0L))) {
    if (!is.null(env)) {
      cplexAPI::closeEnvCPLEX(env)
    }
    stop(cplexAPI::errmsg(result))
  }
  invisible(TRUE)
}

cplex_matrix <- function(m) {
  # inspired by Rcplex:::toCPXMatrix function
  assertthat::assert_that(inherits(m, "dgCMatrix"))
  matbeg <- m@p
  matcnt <- diff(c(m@p, length(m@x)))
  matind <- m@i
  matval <- m@x
  list(matbeg = as.integer(matbeg), matcnt = as.integer(matcnt),
        matind = as.integer(matind), matval = as.double(matval))
}

cplex <- function(model, control) {
  # assert valid arguments
  assertthat::assert_that(is.list(model), is.list(control))
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
  cplex_error_wrap(cplexAPI::setIntParmCPLEX(
      env, 1035, as.integer(control$verbose)), env)
  ## presolve (parameter: CPX_PARAM_PREIND)
  cplex_error_wrap(cplexAPI::setIntParmCPLEX(
      env, 1030, as.integer(control$presolve)), env)
  ## threads (parameter: CPX_PARAM_THREADS)
  cplex_error_wrap(cplexAPI::setIntParmCPLEX(
      env, 1067, as.integer(control$threads)), env)
  ## (relative) optimality gap (parameter: CPX_PARAM_EPGAP)
  cplex_error_wrap(cplexAPI::setDblParmCPLEX(
      env, 2009, as.double(control$gap)), env)
  ## time limit (parameter: CPX_PARAM_TILIM)
  cplex_error_wrap(cplexAPI::setDblParmCPLEX(
      env, 1039, as.double(control$time_limit)), env)
  # initialize problem
  p <- cplexAPI::initProbCPLEX(env)
  cplex_error_wrap(cplexAPI::chgProbNameCPLEX(env, p, "prioritizr"), env)
  # build problem
  result <- cplexAPI::copyLpwNamesCPLEX(
    env = env, lp = p, nCols = ncol(model$A), nRows = nrow(model$A),
    lpdir = ifelse(identical(model$modelsense, "max"),
                   cplexAPI::CPX_MAX, cplexAPI::CPX_MIN),
    objf = model$obj,
    rhs = model$rhs,
    sense = model$sense,
    lb = model$lb,
    ub = model$ub,
    matbeg = model$A2$matbeg,
    matcnt = model$A2$matcnt,
    matind = model$A2$matind,
    matval = model$A2$matval)
  if (!(identical(result, 0) || identical(result, 0L))) {
     msg <- paste("issue preparing data for IBM CPLEX, please file an issue at",
                   utils::packageDescription("prioritizr")$BugReports)
    stop(msg)
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
