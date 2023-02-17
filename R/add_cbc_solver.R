#' @include Solver-class.R
NULL

#' Add a *CBC* solver
#'
#' Specify that the [*CBC*](https://github.com/coin-or/Cbc)
#' (COIN-OR branch and cut) software should be used to solve a conservation
#' planning problem (Forrest & Lougee-Heimer 2005).
#' This function can also be used to customize the behavior of the solver.
#' It requires the \pkg{rcbc} package to be installed
#' (only [available on GitHub](https://github.com/dirkschumacher/rcbc),
#' see below for installation instructions).
#'
#' @inheritParams add_cplex_solver
#' @inheritParams add_gurobi_solver
#'
#' @details
#' [*CBC*](https://github.com/coin-or/Cbc) is an
#' open-source mixed integer programming solver that is part of the
#' Computational Infrastructure for Operations Research (COIN-OR) project.
#' This solver seems to have much better performance than the other open-source
#' solvers (i.e., [add_highs_solver()], [add_rsymphony_solver()],
#' [add_lpsymphony_solver()])
#' (see the _Solver benchmarks_ vignette for details).
#' As such, it is strongly recommended to use this solver if the *Gurobi* and
#' *IBM CPLEX* solvers are not available.
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
#' For further details on installing this package, please consult the
#' [online package documentation](https://dirkschumacher.github.io/rcbc/).
#'
#' @inheritSection add_gurobi_solver Start solution format
#'
#' @inherit add_gurobi_solver return seealso
#'
#' @family solvers
#'
#' @references
#' Forrest J and Lougee-Heimer R (2005) CBC User Guide. In Emerging theory,
#' Methods, and Applications (pp. 257--277). INFORMS, Catonsville, MD.
#' \doi{10.1287/educ.1053.0020}.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create problem
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_cbc_solver(gap = 0, verbose = FALSE)
#'
#' # generate solution %>%
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # create a similar problem with boundary length penalties and
#' # specify the solution from the previous run as a starting solution
#' p2 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_boundary_penalties(10) %>%
#'   add_binary_decisions() %>%
#'   add_cbc_solver(gap = 0, start_solution = s1, verbose = FALSE)
#'
#' # generate solution
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2, main = "solution with boundary penalties", axes = FALSE)
#' }
#' @name add_cbc_solver
NULL

#' @rdname add_cbc_solver
#' @export
add_cbc_solver <- function(x,
                           gap = 0.1,
                           time_limit = .Machine$integer.max,
                           presolve = TRUE,
                           threads = 1,
                           first_feasible = FALSE,
                           start_solution = NULL,
                           verbose = TRUE) {
  # assert that arguments are valid (except start_solution)
  rlang::check_required(x)
  rlang::check_required(gap)
  rlang::check_required(time_limit)
  rlang::check_required(presolve)
  rlang::check_required(threads)
  rlang::check_required(first_feasible)
  rlang::check_required(start_solution)
  rlang::check_required(verbose)
  assert(
    is_conservation_problem(x),
    assertthat::is.number(gap),
    all_finite(gap),
    gap >= 0,
    assertthat::is.count(time_limit),
    all_finite(time_limit),
    assertthat::is.flag(presolve),
    assertthat::noNA(presolve),
    assertthat::is.count(threads),
    all_finite(threads),
    is_thread_count(threads),
    assertthat::is.flag(first_feasible),
    assertthat::noNA(first_feasible),
    assertthat::is.flag(verbose),
    is_installed("rcbc")
  )
 # extract start solution
  if (!is.null(start_solution)) {
    # verify that version of rcbc installed supports starting solution
    assert(
      any(
        grepl(
          "initial_solution", deparse1(args(rcbc::cbc_solve)),
          fixed = TRUE
        )
      ),
      msg = paste(
        "To use {.arg start_solution}, please install a newer",
        "version of the {.pkg rcbc} package."
      )
    )
    # extract data
    start_solution <- planning_unit_solution_status(x, start_solution)
  }
  # add solver
  x$add_solver(
    R6::R6Class(
      "CbcSolver",
      inherit = Solver,
      public = list(
        name = "cbc solver",
        data = list(
          gap = gap,
          time_limit = time_limit,
          presolve = presolve,
          threads = threads,
          first_feasible = first_feasible,
          start = start_solution,
          verbose = verbose
        ),
        calculate = function(x, ...) {
          # prepare constraints
          ## extract info
          rhs <- x$rhs()
          sense <- x$sense()
          assert(
            all(sense %in% c("=", "<=", ">=")),
            msg =
              "failed to prepare problem formulation for {.pkg rcbc} package.",
            call = rlang::expr(add_cbc_solver()),
            .internal = TRUE
          )
          ## initialize CBC arguments
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
            max = identical(x$modelsense(), "max"),
            obj = x$obj(),
            is_integer = x$vtype() == "B",
            mat = as_Matrix(x$A(), "dgTMatrix"),
            col_lb = x$lb(),
            col_ub = x$ub(),
            row_lb = row_lb,
            row_ub = row_ub
          )
          # if needed, insert dummy row to ensure non-zero value in last cell
          if (abs(model$mat[nrow(model$mat), ncol(model$mat)]) < 1e-300) {
            model$mat <- as_Matrix(
              rbind(
                model$mat,
                Matrix::sparseMatrix(
                  i = 1, j = ncol(model$mat), x = 1, repr = "T"
                )
              ),
              "dgTMatrix"
            )
            model$row_lb <- c(model$row_lb, -Inf)
            model$row_ub <- c(model$row_ub, Inf)
          }
          # add starting solution if specified
          start <- self$get_data("start")
          if (!is.null(start) && !is.Waiver(start)) {
            n_extra <- length(model$obj) - length(start)
            model$initial_solution <- c(c(start), rep(NA_real_, n_extra))
          }
          # create parameters
          p <- list(
            log = as.character(as.numeric(self$get_data("verbose"))),
            verbose = "1",
            presolve = ifelse(self$get_data("presolve") > 0.5, "on", "off"),
            ratio = as.character(self$get_data("gap")),
            sec = as.character(self$get_data("time_limit")),
            threads = as.character(self$get_data("threads"))
          )
          if (self$get_data("first_feasible") > 0.5) {
            p$maxso <- "1"
          }
          p$timeMode <- "elapsed"
          # store input data and parameters
          self$set_internal("model", model)
          self$set_internal("parameters", p)
          # return success
          invisible(TRUE)
        },
        set_variable_ub = function(index, value) {
          self$internal$model$col_ub[index] <- value
          invisible(TRUE)
        },
        set_variable_lb = function(index, value) {
          self$internal$model$col_lb[index] <- value
          invisible(TRUE)
        },
        run = function() {
          # access input data and parameters
          model <- self$get_internal("model")
          p <- self$get_internal("parameters")
          # solve problem
          rt <- system.time({
            x <- do.call(rcbc::cbc_solve, append(model, list(cbc_args = p)))
          })
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
          list(
            x = x$column_solution,
            objective = x$objective_value,
            status = as.character(rcbc::solution_status(x)),
            runtime = rt[[3]]
          )
        }
      )
    )$new()
  )
}
