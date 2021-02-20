#' @include Solver-proto.R
NULL

#' Add a *CBC* solver
#'
#' Specify that the [*CBC*](https://projects.coin-or.org/Cbc)
#' (COIN-OR branch and cut) software (Forrest &
#' Lougee-Heimer 2005) should be used to solve a conservation planning
#' [problem()].
#' This function can also be used to customize the behavior of the solver.
#' It requires the \pkg{rcbc} package to be installed
#' (only [available on GitHub](https://github.com/dirkschumacher/rcbc),
#' see below for installation instructions).
#'
#' @inheritParams add_cplex_solver
#' @inheritParams add_gurobi_solver
#'
#' @details
#' [*CBC*](https://projects.coin-or.org/Cbc) is an
#' open-source mixed integer programming solver that is part of the
#' Computational Infrastructure for Operations Research (COIN-OR) project, an
#' initiative to promote development of open-source tools for operations
#' research (a field that includes linear programming).
#' Although the SYMHPONY software is also part of the COIN-OR project, it is
#' distinct from the CBC software. Thus CBC and SYMPHONY have different
#' performance and functionality.
#'
#' @section Installation:
#' The \pkg{rcbc} package is required to use this solver. Since the
#' \pkg{rcbc} package is not available on the
#' the Comprehensive R Archive Network (CRAN), it must be installed from
#' [its GitHub repository](https://github.com/dirkschumacher/rcbc). To
#' install the \pkg{rcbc} package, please use the following code:
#' ```
#' if (!require(remotes)) install.packages("remotes")
#' remotes::install_github("dirkschumacher/rcbc")
#' ```
#' Note that you may also need to install several dependencies --
#' such as the
#' [Rtools software](https://cran.r-project.org/bin/windows/Rtools/)
#' or system libraries -- prior to installing the \pkg{rcbc} package.
#' For further details on installing this package, please consult
#' [official installation instructions for the package](https://dirkschumacher.github.io/rcbc/).
#'
#' @return Object (i.e. [`ConservationProblem-class`]) with the solver
#'  added to it.
#'
#' @references
#' Forrest J and Lougee-Heimer R (2005) CBC User Guide. In Emerging theory,
#' Methods, and Applications (pp. 257--277). INFORMS, Catonsville, MD.
#' <doi:10.1287/educ.1053.0020>
#'
#' @seealso [solvers].
#'
#' @examples
#' \dontrun{
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_cbc_solver(gap = 0.1, verbose = FALSE)
#'
#' # generate solution %>%
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_cbc_solver
NULL

#' @rdname add_cbc_solver
#' @export
add_cbc_solver <- function(x, gap = 0.1,
                           time_limit = .Machine$integer.max,
                           presolve = TRUE, threads = 1,
                           first_feasible = FALSE,
                           verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.scalar(gap),
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.count(time_limit),
                          isTRUE(all(is.finite(presolve))),
                          assertthat::is.flag(presolve),
                          assertthat::noNA(presolve),
                          isTRUE(all(is.finite(threads))),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)),
                          assertthat::is.flag(first_feasible),
                          assertthat::noNA(first_feasible),
                          assertthat::is.flag(verbose),
                          requireNamespace("rcbc", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "CbcSolver",
    Solver,
    name = "CBC",
    data = list(),
    parameters = parameters(
      numeric_parameter("gap", gap, lower_limit = 0),
      integer_parameter("time_limit", time_limit, lower_limit = -1L,
                        upper_limit = as.integer(.Machine$integer.max)),
      binary_parameter("presolve", presolve),
      integer_parameter("threads", threads, lower_limit = 1L,
                        upper_limit = parallel::detectCores(TRUE)),
      binary_parameter("first_feasible", first_feasible),
      binary_parameter("verbose", verbose)),
    calculate = function(self, x, ...) {
      # prepare constraints
      ## extract info
      rhs <- x$rhs()
      sense <- x$sense()
      assertthat::assert_that(
        all(sense %in% c("=", "<=", ">=")),
        msg = "failed to prepare problem formulation for rcbc package")
      ## initialize CBC arguments
      row_lb <- numeric(length(rhs))
      row_ub <- numeric(length(rhs))
      ## set equality constraints
      idx <- which(sense == "=")
      row_lb[idx] <- rhs[idx] - 1e-5
      row_ub[idx] <- rhs[idx] + 1e-5
      ## set lte constraints
      idx <- which(sense == "<=")
      row_lb[idx] <- -Inf
      row_ub[idx] <- rhs[idx]
      ## set gte constraints
      idx <- which(sense == ">=")
      row_lb[idx] <- rhs[idx]
      row_ub[idx] <- Inf
      # create problem
      model <- list(
        max = identical(x$modelsense(), "max"),
        obj = x$obj(),
        is_integer = x$vtype() == "B",
        mat = x$A(),
        col_lb = x$lb(),
        col_ub = x$ub(),
        row_lb = row_lb,
        row_ub = row_ub)
      # create parameters
      p <- list(
        LOG = as.character(as.numeric(self$parameters$get("verbose"))),
        PRESOLVE = ifelse(self$parameters$get("presolve") > 0.5, "On", "Off"),
        RATIO = as.character(self$parameters$get("gap")),
        SEC = as.character(self$parameters$get("time_limit")),
        TIMEM = "ELAPSED",
        THREADS = as.character(self$parameters$get("threads")))
      if (self$parameters$get("first_feasible") > 0.5) {
        p$MAXSO <- "1"
      }
      # store input data and parameters
      self$set_data("model", model)
      self$set_data("parameters", p)
      # return success
      invisible(TRUE)
    },
    set_variable_ub = function(self, index, value) {
      self$data$model$col_ub[index] <- value
      invisible(TRUE)
    },
    set_variable_lb = function(self, index, value) {
      self$data$model$col_lb[index] <- value
      invisible(TRUE)
    },
    run = function(self, x) {
      # access input data and parameters
      model <- self$get_data("model")
      p <- self$get_data("parameters")
      # solve problem
      start_time <- Sys.time()
      x <- do.call(rcbc::cbc_solve, append(model, list(cbc_args = p)))
      end_time <- Sys.time()
      # return NULL if infeasible
      if (x$is_proven_dual_infeasible ||
          x$is_proven_infeasible ||
          x$is_abandoned) {
        return(NULL)
      }
      # fix potential floating point arithmetic issues
      if (is.numeric(x$objective_value)) {
        ## round binary variables because default precision is 1e-5
        x$column_solution[model$is_integer] <-
          round(x$column_solution[model$is_integer])
        ## truncate variables to account for rounding issues
        x$column_solution <- pmax(x$column_solution, model$col_lb)
        x$column_solution <- pmin(x$column_solution, model$col_ub)
      }
      # return output
      return(list(x = x$column_solution, objective = x$objective_value,
                  status = as.character(rcbc::solution_status(x)),
                  runtime = as.double(end_time - start_time,
                                      format = "seconds")))
    }))
}
