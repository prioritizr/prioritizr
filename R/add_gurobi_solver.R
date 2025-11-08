#' @include Solver-class.R
NULL

#' Add a *Gurobi* solver
#'
#' Specify that the [*Gurobi*](https://www.gurobi.com/) software
#' should be used to solve a conservation planning problem
#' (Gurobi Optimization LLC 2021). This function can also be used to
#' customize the behavior of the solver.
#' It requires the \pkg{gurobi} package to be installed
#' (see below for installation instructions).
#'
#' @param x [problem()] or [multi_problem()] object.
#'
#' @param gap `numeric` gap to optimality. This gap is relative
#'   and expresses the acceptable deviance from the optimal objective.
#'   For example, a value of 0.01 will result in the solver stopping when
#'   it has found a solution within 1% of optimality.
#'   Additionally, a value of 0 will result in the solver stopping
#'   when it has found an optimal solution.
#'   The default value is 0.1 (i.e., 10% from optimality).
#'
#' @param time_limit `numeric` time limit (seconds) for generating solutions.
#'   The solver will return the current best solution when this time limit is
#'   exceeded. The default value is the largest integer value
#'   (i.e., `.Machine$integer.max`), effectively meaning that solver
#'   will keep running until a solution within the optimality gap is found.
#'
#' @param presolve `integer` number indicating how intensively the
#'   solver should try to simplify the problem before solving it. Available
#'   options are: (-1) automatically determine the intensity of
#'   pre-solving, (0) disable pre-solving, (1) conservative
#'   level of pre-solving, and (2) very aggressive level of pre-solving .
#'   The default value is 2.
#'
#' @param threads `integer` number of threads to use for the
#'   optimization algorithm. The default value is 1.
#'
#' @param first_feasible `logical` should the first feasible solution be
#'   be returned? If `first_feasible` is set to `TRUE`, the solver
#'   will return the first solution it encounters that meets all the
#'   constraints, regardless of solution quality. Note that the first feasible
#'   solution is not an arbitrary solution, rather it is derived from the
#'   relaxed solution, and is therefore often reasonably close to optimality.
#'   Defaults to `FALSE`.
#'
#' @param numeric_focus `logical` should extra attention be paid
#'   to verifying the accuracy of numerical calculations? This may be
#'   useful when dealing with problems that may suffer from numerical
#'   instability issues.
#'   Beware that it will likely substantially increase run time
#'   (sets the *Gurobi* `NumericFocus` parameter
#'   to 2). Defaults to `FALSE`.
#'
#' @param node_file_start `numeric` threshold amount of memory (in GB).
#'   Once the amount of memory (RAM) used to store information for solving
#'   the optimization problem exceeds this parameter value, the solver
#'   will begin storing this information on disk
#'   (using the *Gurobi* `NodeFileStart` parameter).
#'   This functionality is useful if the system has insufficient memory to
#'   solve a given problem (e.g., solving the problem with default settings
#'   yields the `OUT OF MEMORY` error message) and a system with more memory is
#'   not readily available.
#'   For example, a value of 4 indicates that the solver will start using
#'   the disk after it uses more than 4 GB of memory to store information
#'   on solving the problem.
#'   Defaults to `Inf` such that the solver will not attempt
#'   to store information on disk when solving a given problem.
#'
#' @param start_solution `NULL` or object containing the starting solution
#'   for the solver. This is can be useful because specifying a starting
#'   solution can speed up the optimization process.
#'   Defaults to `NULL` such that no starting solution is used.
#'   To specify a starting solution, the argument to `start_solution` should
#'   be in the same format as the planning units (i.e., a `NULL`, `numeric`,
#'   `matrix`, `data.frame`, [terra::rast()], or [sf::sf()] object).
#'   See the Start solution format section for more information.
#'
#' @param verbose `logical` should information be printed while solving
#'  optimization problems? Defaults to `TRUE`.
#'
#' @param control `list` with additional parameters for tuning
#'  the optimization process.
#'  For example, `control = list(Method = 2)` could be used to
#'  set the `Method` parameter.
#'  See the [online documentation](https://docs.gurobi.com/projects/optimizer/en/current/reference/parameters.html)
#'  for information on the parameters.
#"
#' @details
#' [*Gurobi*](https://www.gurobi.com/) is a
#' state-of-the-art commercial optimization software with an R package
#' interface. It is by far the fastest of the solvers available for
#' generating prioritizations, however, it is not freely
#' available. That said, licenses are available to academics at no cost. The
#' \pkg{gurobi} package is distributed with the *Gurobi* software suite.
#' This solver uses the \pkg{gurobi} package to solve problems.
#' For information on the performance of different solvers,
#' please see Schuster _et al._ (2020) for benchmarks comparing the
#' run time and solution quality of different solvers when applied to
#' different sized datasets.
#'
#' @section Installation:
#' Please see the *Gurobi Installation Guide* vignette for details on
#' installing the *Gurobi* software and the \pkg{gurobi} package.
#' You can access this vignette
#' [online](https://prioritizr.net/articles/gurobi_installation_guide.html)
#' or using the following code:
#' ```
#' vignette("gurobi_installation_guide", package = "prioritizr")
#' ```
#'
#' @section Start solution format:
#' Broadly speaking, the argument to `start_solution` must be in the same
#' format as the planning unit data in the argument to `x`.
#' Further details on the correct format are listed separately
#' for each of the different planning unit data formats:
#' `r solution_format_documentation("start_solution")`
#'
#' @return An updated [problem()] object with the solver added to it.
#'
#' @seealso
#' See [solvers] for an overview of all functions for adding a solver.
#'
#' @family solvers
#'
#' @references
#' Gurobi Optimization LLC (2021) Gurobi Optimizer Reference Manual.
#' <https://www.gurobi.com>.
#'
#' Schuster R, Hanson JO, Strimas-Mackey M, and Bennett JR (2020). Exact
#' integer linear programming solvers outperform simulated annealing for
#' solving conservation planning problems. *PeerJ*, 8: e9258.
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
#'   add_gurobi_solver(gap = 0, verbose = FALSE)
#'
#' # generate solution
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
#'   add_gurobi_solver(gap = 0, start_solution = s1, verbose = FALSE)
#'
#' # generate solution
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2, main = "solution with boundary penalties", axes = FALSE)
#' }
#' @name add_gurobi_solver
NULL

#' @rdname add_gurobi_solver
#' @export
add_gurobi_solver <- function(x, gap = 0.1, time_limit = .Machine$integer.max,
                              presolve = 2, threads = 1, first_feasible = FALSE,
                              numeric_focus = FALSE, node_file_start = Inf,
                              start_solution = NULL, verbose = TRUE,
                              control = list()) {
  # assert that arguments are valid (except start_solution)
  assert_required(x)
  assert_required(gap)
  assert_required(time_limit)
  assert_required(presolve)
  assert_required(threads)
  assert_required(first_feasible)
  assert_required(numeric_focus)
  assert_required(node_file_start)
  assert_required(start_solution)
  assert_required(verbose)
  assert_required(control)
  assert(
    is_generic_conservation_problem(x),
    assertthat::is.number(gap),
    all_finite(gap),
    gap >= 0,
    assertthat::is.count(time_limit),
    all_finite(time_limit),
    assertthat::is.number(presolve),
    all_finite(presolve),
    is_match_of(presolve, c(-1, 0, 1, 2)),
    is_thread_count(threads),
    assertthat::is.flag(first_feasible),
    assertthat::noNA(first_feasible),
    assertthat::is.flag(numeric_focus),
    assertthat::noNA(numeric_focus),
    assertthat::is.number(node_file_start),
    assertthat::noNA(node_file_start),
    node_file_start >= 0,
    assertthat::is.flag(verbose),
    is.list(control),
    is_installed("slam"),
    is_installed("gurobi")
  )
  # additional checks for control
  if (length(control) > 0) {
    assert(
      !is.null(names(control)),
      all(nzchar(names(control))),
      msg = "all elements in {.arg control} must have a name."
    )
  }
  # extract start solution
  if (!is.null(start_solution)) {
    start_solution <- planning_unit_solution_status(x, start_solution)
  }
  # adjust node file start
  if (identical(node_file_start, Inf)) {
    node_file_start <- -1 # this is used later to indicate Inf
  }
  # add solver
  x$add_solver(
    R6::R6Class(
      "GurobiSolver",
      inherit = Solver,
      public = list(
        name = "gurobi solver",
        data = list(
          gap = gap,
          time_limit = time_limit,
          presolve = presolve,
          threads = threads,
          first_feasible = first_feasible,
          numeric_focus = numeric_focus,
          node_file_start = node_file_start,
          start_solution = start_solution,
          verbose = verbose,
          control = control
        ),
        calculate = function(x, ...) {
          # create problem
          model <- list(
            modelsense = x$modelsense(),
            vtype = x$vtype(),
            obj = x$obj(),
            A = x$A(),
            rhs = x$rhs(),
            sense = x$sense(),
            lb = x$lb(),
            ub = x$ub()
          )
          # create parameters
          p <- list(
            LogToConsole = as.numeric(self$get_data("verbose")),
            LogFile = "",
            Presolve = self$get_data("presolve"),
            MIPGap = self$get_data("gap"),
            TimeLimit = self$get_data("time_limit"),
            Threads = self$get_data("threads"),
            NumericFocus = as.numeric(self$get_data("numeric_focus")) * 2,
            NodeFileStart = self$get_data("node_file_start"),
            SolutionLimit = as.numeric(self$get_data("first_feasible"))
          )
          if (p$SolutionLimit == 0)
            p$SolutionLimit <- NULL
          if (p$NodeFileStart < 0) {
            p$NodeFileStart <- NULL
          }
          # specify custom parameters
          control <- self$get_data("control")
          if (length(control) > 0) {
            p[names(control)] <- control
          }
          # add extra parameters from portfolio if needed
          p2 <- list(...)
          for (i in seq_along(p2))
            p[[names(p2)[i]]] <- p2[[i]]
          # store internal model and parameters
          self$set_internal("model", model)
          self$set_internal("parameters", p)
          # return success
          invisible(TRUE)
        },
        set_variable_ub = function(index, value) {
          self$internal$model$ub[index] <- value
          invisible(TRUE)
        },
        set_variable_lb = function(index, value) {
          self$internal$model$lb[index] <- value
          invisible(TRUE)
        },
        set_constraint_rhs = function(index, value) {
          self$internal$model$rhs[index] <- value
          invisible(TRUE)
        },
        run = function() {
          # access internal model and parameters
          start <- self$get_data("start_solution")
          model <- self$get_internal("model")
          p <- self$get_internal("parameters")
          # add starting solution if specified
          if (!is.null(start) && !is.Waiver(start)) {
            n_extra <- max(length(model$obj) - length(start), 0)
            model$start <- c(c(start), rep(NA_real_, n_extra))
          }
          # solve problem
          rt <- system.time({
            x <- withr::with_locale(
              c(LC_CTYPE = "C"),
              gurobi::gurobi(model = model, params = p)
            )
          })
          # fix potential floating point arithmetic issues
          b <- model$vtype == "B"
          if (is.numeric(x$x)) {
            ## round binary variables because default precision is 1e-5
            x$x[b] <- round(x$x[b])
            ## truncate semi-continuous variables
            v <- model$vtype == "S"
            x$x[v] <- pmax(x$x[v], 0)
            x$x[v] <- pmin(x$x[v], 1)
            ## truncate variables to account for rounding issues
            x$x <- pmax(x$x, model$lb)
            x$x <- pmin(x$x, model$ub)
          }
          # set defaults to NA if missing
          ## this is because earlier versions of Gurobi didn't return this info
          if (is.null(x$mipgap)) {
            x$mipgap <- NA_real_
          }
          if (is.null(x$objbound)) {
            x$objbound <- NA_real_
          }
          # extract solutions
          out <- list(
            x = x$x,
            objective = x$objval,
            status = x$status,
            runtime = rt[[3]],
            gap = x$mipgap,
            objbound = x$objbound
          )
          # add pool if required
          if (!is.null(p$PoolSearchMode) &&
              is.numeric(x$x) &&
              isTRUE(length(x$pool) > 1)
          ) {
            out$pool <- x$pool[-1]
            # get bound for objective value for optimal solution
            optimal_obj <- x$objbound
            for (i in seq_len(length(out$pool))) {
              # fix binary variables for i'th solution in pool
              out$pool[[i]]$xn[b] <- round(out$pool[[i]]$xn[b])
              # calculate gap for i'th solution in pool
              i_gap <- ifelse(
                identical(model$modelsense, "min"),
                (out$pool[[i]]$objval - optimal_obj) / optimal_obj,
                (optimal_obj - out$pool[[i]]$objval) / optimal_obj
              )
              # if the solver has OPTIMAL status this means that main
              # solution is within optimality gap,
              # so now we need to set a separate status for each solution
              # in the solution pool
              if (identical(x$status, "OPTIMAL")) {
                out$pool[[i]]$status <- ifelse(
                  isTRUE(i_gap <= self$get_data("gap")),
                  "OPTIMAL",
                  "SUBOPTIMAL"
                )
              } else {
                # if solver has a status other than OPTIMAL,
                # then we just assign this status to each solution in
                # the solution  pool
                out$pool[[i]]$status <- x$status
              }
              # set remaining values for i'th solution
              out$pool[[i]]$objective <- out$pool[[i]]$objval
              out$pool[[i]]$gap <- i_gap
              out$pool[[i]]$objbound <- x$objbound
            }
          }
          out
        }
      )
    )$new()
  )
}
