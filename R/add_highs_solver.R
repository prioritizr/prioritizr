#' @include Solver-proto.R
NULL

#' Add a *HiGHS* solver
#'
#' Specify that the [*HiGHS*](https://highs.dev/) software
#' (Huangfu and Hall 2018) should be used to solve a
#' conservation planning [problem()]. This function can also be used to
#' customize the behavior of the solver.
#' It requires the \pkg{highs} package to be installed.
#'
#' @inheritParams add_cplex_solver
#' @inheritParams add_gurobi_solver
#'
#' @details
#' [*HiGHS*](https://highs.dev/) is an open source optimization software.
#' Although this solver can have comparable performance to the *CBC* solver
#' (i.e., [add_cbc_solver()]) for particular problems and is generally faster
#' than the *SYMPHONY* based solvers (i.e., [add_rsymphony_solver()],
#' [add_lpsymphony_solver()]), it can sometimes take much longer than the
#' *CBC* solver for particular problems. This solver is recommended if
#' the [add_gurobi_solver()], [add_cplex_solver()], [add_cbc_solver()] cannot
#' be used.
#'
#' @inheritSection add_gurobi_solver Start solution format
#'
#' @inherit add_gurobi_solver return seealso
#'
#' @family solvers
#'
#' @references
#' Huangfu Q and Hall JAJ (2018). Parallelizing the dual revised simplex
#' method. *Mathematical Programming Computation*, 10: 119-142.
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
#'      add_highs_solver(gap = 0, verbose = FALSE)
#'
#' # generate solution
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_highs_solver
NULL

#' @rdname add_highs_solver
#' @export
add_highs_solver <- function(x, gap = 0.1, time_limit = .Machine$integer.max,
                             presolve = TRUE, threads = 1,
                             verbose = TRUE) {
  # assert that arguments are valid (except start_solution)
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.scalar(gap),
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.count(time_limit),
                          isTRUE(all(is.finite(presolve))),
                          assertthat::is.scalar(presolve),
                          isTRUE(presolve >= -1 & presolve <= 2),
                          isTRUE(all(is.finite(threads))),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)),
                          assertthat::is.flag(verbose),
                          requireNamespace("highs", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "HighsSolver",
    Solver,
    name = "HiGHS",
    data = list(),
    parameters = parameters(
      numeric_parameter("gap", gap, lower_limit = 0),
      integer_parameter("time_limit", time_limit, lower_limit = -1L,
                        upper_limit = as.integer(.Machine$integer.max)),
      binary_parameter("presolve", presolve),
      integer_parameter("threads", threads, lower_limit = 1L,
                        upper_limit = parallel::detectCores(TRUE)),
      binary_parameter("verbose", verbose)),
    calculate = function(self, x, ...) {
      # prepare constraints
      ## extract info
      rhs <- x$rhs()
      sense <- x$sense()
      assertthat::assert_that(
        all(sense %in% c("=", "<=", ">=")),
        msg = "failed to prepare problem formulation for \"highs\" package")
      ## initialize arguments
      row_lb <- numeric(length(rhs))
      row_ub <- numeric(length(rhs))
      ## set equality constraints
      idx <- which(sense == "=")
      row_lb[idx] <- rhs[idx]
      row_ub[idx] <- rhs[idx]
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
        maximum = identical(x$modelsense(), "max"),
        L = x$obj(),
        A = x$A(),
        lhs = row_lb,
        rhs = row_ub,
        types = x$vtype(),
        lower = x$lb(),
        upper = x$ub())
      # round values < 1e-6 to zero and drop them
      model$A@x[abs(model$A@x) < 1e-6] <- 0
      model$A <- Matrix::drop0(model$A)
      model$L[abs(model$L) < 1e-6] <- 0
      # set variables types
      ## C = continuous (same as gurobi)
      ## I = integer (same as gurobi)
      ## SC = semi-continuous (not same as gurobi)
      ## binary type not supported, convert to integer gurobi)
      model$types[model$types == "B"] <- "I"
      model$types[model$types == "S"] <- "SC"
      # create parameters
      p <- list(
        log_to_console = self$parameters$get("verbose"),
        presolve = ifelse(self$parameters$get("presolve"), "on", "off"),
        mip_rel_gap = self$parameters$get("gap"),
        time_limit = as.numeric(self$parameters$get("time_limit")),
        threads = self$parameters$get("threads")
      )
      # store input data and parameters
      self$set_data("model", model)
      self$set_data("parameters", p)
      # return success
      invisible(TRUE)
    },
    set_variable_ub = function(self, index, value) {
      self$data$model$upper[index] <- value
      invisible(TRUE)
    },
    set_variable_lb = function(self, index, value) {
      self$data$model$lower[index] <- value
      invisible(TRUE)
    },
    run = function(self, x) {
      # access input data and parameters
      model <- self$get_data("model")
      p <- self$get_data("parameters")
      # solve problem
      rt <- system.time({
        x <- do.call(
          highs::highs_solve,
          append(model, list(control = p))
        )
      })
      .GlobalEnv$o1 <- model
      # manually return NULL to indicate error if no solution
      # nocov start
      if (
        is.null(x) ||
        is.null(x$primal_solution) ||
        any(is.na(x$primal_solution)) ||
        isTRUE(!x$status %in% c(7L, 11L, 12L, 13L, 14L))
      ) {
        return(NULL)
      }
      # nocov end
      # extract solution values
      sol <- x$primal_solution
      ## fix potential floating point arithmetic issues
      i <- model$types == "I"
      if (is.numeric(sol)) {
        ## round integer variables
        sol[i] <- round(sol[i])
        ## truncate variables to account for rounding issues
        sol <- pmax(sol, model$lower)
        sol <- pmin(sol, model$upper)
      }
      # return solution
      list(
        x = sol,
        objective = x$objective_value,
        status = x$status_message,
        runtime = rt[[3]])
    }))
}
