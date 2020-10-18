#' @include Solver-proto.R
NULL

#' Add a *CPLEX* solver
#'
#' Specify that the *IBM CPLEX* software should be used to solve a
#' conservation planning problem. This function can also be used to
#' customize the behavior of the solver. It requires the \pkg{Rcplex} package.
#'
#' @inheritParams add_gurobi_solver
#'
#' @inherit add_gurobi_solver return seealso
#'
#' @param presolve `logical` should the presolver be used to simplify the
#'   problem before solving it? The default value is \code{TRUE}.
#'
#' @details [*IBM CPLEX*](https://www.ibm.com/analytics/cplex-optimizer) is a
#'   commercial optimization software. Although this software is faster than
#'   the available open source solvers (i.e [add_lpsymphony_solver()] and
#'   [add_rsymphony_solver()], it is slower than the *Gurobi* solver
#'   ([add_gurobi_solver()]).
#'   Licenses are available for the *IBM CPLEX* software to academics at no cost
#'   (see <https://www.ibm.com/products/ilog-cplex-optimization-studio>).
#'
#'   The pkg{Rcplex} package is used to interface with *IBM CPLEX*. To install
#'   this package, the `CPLEX_BIN` variable must be set (similar to
#'   the `GUROBI_HOME` variable for the *Gurobi* software) to specify
#'   the file path for the *CPLEX* software. For example, on a Linux system,
#'   this variable can be specified by adding
#'   `export CPLEX_BIN="/opt/ibm/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/cplex"`
#'   to the `~/.bashrc` file. Note that you may need to change the version
#'   number in the file parth (i.e. `"CPLEX_Studio128"`). For more information
#'   on installing the pkg{Rcplex} package, please see the
#'   [official installation instructions for the package](https://CRAN.R-project.org/package=Rcplex/INSTALL).
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
#' \dontrun{
#' # if the package is installed then add solver and generate solution
#' if (require("Rcplex")) {
#'   # specify solver and generate solution
#'   s <- p %>% add_cplex_solver(gap = 0.1, time_limit = 5) %>%
#'              solve()
#'
#'   # plot solutions
#'   plot(stack(sim_pu_raster, s), main = c("planning units", "solution"),
#'        axes = FALSE, box = FALSE)
#' }
#' }
#' @name add_cplex_solver
NULL

#' @rdname add_cplex_solver
#' @export
add_cplex_solver <- function(x, gap = 0.1, time_limit = .Machine$integer.max,
                              presolve = TRUE, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.scalar(gap),
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.count(time_limit),
                          assertthat::is.flag(presolve),
                          assertthat::noNA(presolve),
                          assertthat::is.flag(verbose),
                          requireNamespace("Rcplex", quietly = TRUE))
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
      binary_parameter("presolve", presolve),
      binary_parameter("verbose", verbose)),
    calculate = function(self, x, ...) {
      # create problem
      model <- list(
        objsense = x$modelsense(),
        vtype = x$vtype(),
        cvec = x$obj(),
        Amat = x$A(),
        bvec = x$rhs(),
        sense = x$sense(),
        lb = x$lb(),
        ub = x$ub())
      # format problem for CPLEX
      model$sense[model$sense == ">="] <- "G"
      model$sense[model$sense == "="] <- "E"
      model$sense[model$sense == "<="] <- "L"
      model$vtype[model$vtype == "S"] <- "C"
      # create parameters
      p <- list(trace = as.integer(self$parameters$get("verbose")),
                preind = as.integer(self$parameters$get("presolve")),
                epgap = self$parameters$get("gap"),
                tilim = self$parameters$get("time_limit"))
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
      start_time <- Sys.time()
        x <- do.call(Rcplex::Rcplex, append(model, list(control = p)))
      end_time <- Sys.time()
      # release license
      Rcplex::Rcplex.close()
      # fix potential floating point arithmetic issues
      b <- model$vtype == "B"
      if (is.numeric(x$xopt)) {
        ## round binary variables because default precision is 1e-5
        x$xopt[b] <- round(x$xopt[b])
        ## truncate variables to account for rounding issues
        x$xopt <- pmax(x$xopt, model$lb)
        x$xopt <- pmin(x$xopt, model$ub)
      }
      # extract solution values, and
      # set values to NULL if any values have NA in result
      # (this is how Rcplex returns solutions to infeasible problems)
      sol <- x$xopt
      if (any(is.na(sol))) sol <- NULL
      # extract status codes
      msg <- paste("unrecognised CPLEX status code, please file an issue at",
                   utils::packageDescription("prioritizr")$BugReports)
      assertthat::assert_that(assertthat::is.count(x$status), msg = msg)
      raw_code <- as.character(x$status)
      if (!raw_code %in% names(cplex_status_codes)) stop(msg)
      status_code <- cplex_status_codes[raw_code]
      # extract solutions
      out <- list(x = sol, objective = x$obj, status = status_code,
                  runtime = as.double(end_time - start_time,
                                      format = "seconds"))
      # return solution
      out
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

cplex_status_codes <- c(
  "1"="CPX_STAT_OPTIMAL",
  "2"="CPX_STAT_UNBOUNDED",
  "3"="CPX_STAT_INFEASIBLE",
  "4"="CPX_STAT_INForUNBD",
  "5"="CPX_STAT_OPTIMAL_INFEAS",
  "6"="CPX_STAT_NUM_BEST",
  "10"="CPX_STAT_ABORT_IT_LIM",
  "11"="CPX_STAT_ABORT_TIME_LIM",
  "12"="CPX_STAT_ABORT_OBJ_LIM",
  "13"="CPX_STAT_ABORT_USER",
  "14"="CPX_STAT_FEASIBLE_RELAXED_SUM",
  "15"="CPX_STAT_OPTIMAL_RELAXED_SUM",
  "16"="CPX_STAT_FEASIBLE_RELAXED_INF",
  "17"="CPX_STAT_OPTIMAL_RELAXED_INF",
  "18"="CPX_STAT_FEASIBLE_RELAXED_QUAD",
  "19"="CPX_STAT_OPTIMAL_RELAXED_QUAD",
  "23"="CPX_STAT_FEASIBLE",
  "30"="CPX_STAT_CONFLICT_FEASIBLE",
  "31"="CPX_STAT_CONFLICT_MINIMAL",
  "32"="CPX_STAT_CONFLICT_ABORT_CONTRADICTION",
  "33"="CPX_STAT_CONFLICT_ABORT_TIME_LIM",
  "34"="CPX_STAT_CONFLICT_ABORT_IT_LIM",
  "35"="CPX_STAT_CONFLICT_ABORT_NODE_LIM",
  "36"="CPX_STAT_CONFLICT_ABORT_OBJ_LIM",
  "37"="CPX_STAT_CONFLICT_ABORT_MEM_LIM",
  "38"="CPX_STAT_CONFLICT_ABORT_USER",
  "20"="CPX_STAT_OPTIMAL_FACE_UNBOUNDED",
  "21"="CPX_STAT_ABORT_PRIM_OBJ_LIM",
  "22"="CPX_STAT_ABORT_DUAL_OBJ_LIM",
  "101"="CPXMIP_OPTIMAL",
  "102"="CPXMIP_OPTIMAL_TOL",
  "103"="CPXMIP_INFEASIBLE",
  "104"="CPXMIP_SOL_LIM",
  "105"="CPXMIP_NODE_LIM_FEAS",
  "106"="CPXMIP_NODE_LIM_INFEAS",
  "107"="CPXMIP_TIME_LIM_FEAS",
  "108"="CPXMIP_TIME_LIM_INFEAS",
  "109"="CPXMIP_FAIL_FEAS",
  "110"="CPXMIP_FAIL_INFEAS",
  "111"="CPXMIP_MEM_LIM_FEAS",
  "112"="CPXMIP_MEM_LIM_INFEAS",
  "113"="CPXMIP_ABORT_FEAS",
  "114"="CPXMIP_ABORT_INFEAS",
  "115"="CPXMIP_OPTIMAL_INFEAS",
  "116"="CPXMIP_FAIL_FEAS_NO_TREE",
  "117"="CPXMIP_FAIL_INFEAS_NO_TREE",
  "118"="CPXMIP_UNBOUNDED",
  "119"="CPXMIP_INForUNBD",
  "120"="CPXMIP_FEASIBLE_RELAXED_SUM",
  "121"="CPXMIP_OPTIMAL_RELAXED_SUM",
  "122"="CPXMIP_FEASIBLE_RELAXED_INF",
  "123"="CPXMIP_OPTIMAL_RELAXED_INF",
  "124"="CPXMIP_FEASIBLE_RELAXED_QUAD",
  "125"="CPXMIP_OPTIMAL_RELAXED_QUAD",
  "126"="CPXMIP_ABORT_RELAXED",
  "127"="CPXMIP_FEASIBLE",
  "128"="CPXMIP_POPULATESOL_LIM",
  "129"="CPXMIP_OPTIMAL_POPULATED",
  "130"="CPXMIP_OPTIMAL_POPULATED_TOL")
