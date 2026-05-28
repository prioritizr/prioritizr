#' @include internal.R
NULL

#' @export
if (!methods::isClass("Solver")) methods::setOldClass("Solver")
NULL

#' Solver class
#'
#' @description
#' This class is used to represent solvers for optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Solver-class
#'
#' @family classes
#'
#' @export
Solver <- R6::R6Class(
  "Solver",
  inherit = ConservationModifier,
  public = list(

    #' @description
    #' Run the solver to generate a solution.
    #' @return `list` of solutions.
    run = function() {
      # nocov start
      cli::cli_abort("No defined $run method.", .internal = TRUE)
      # nocov end
    },
    #' @description
    #' Perform computations that need to be completed before applying
    #' the object.
    #' @param x [optimization_problem()] object.
    #' @param ... Additional arguments.
    #' @return Invisible `TRUE`.
    calculate = function(...) {
      # nocov start
      cli::cli_abort("No defined $calculate method.", .internal = TRUE)
      # nocov end
    },

    #' @description
    #' Set the upper bound for a decision variable.
    #' @param index `integer` value indicating the index of the decision
    #' variable.
    #' @param value `numeric` new bound value.
    #' @details Note that this method should only be run after `$calculate()`.
    #' It can be used to overwrite values after ingesting an
    #' [optimization_problem()] object.
    #' It is designed to be used in [portfolios] and [importance] functions.
    #' @return Invisible `TRUE`.
    set_variable_ub = function(index, value) {
      # nocov start
      cli::cli_abort("No defined $set_variable_ub method.", .internal = TRUE)
      # nocov end
    },

    #' @description
    #' Set the lower bound for a decision variable.
    #' @param index `integer` value indicating the index of the decision
    #' variable.
    #' @param value `numeric` new bound value.
    #' @details Note that this method should only be run after `$calculate()`.
    #' It can be used to overwrite values after ingesting an
    #' [optimization_problem()] object.
    #' It is designed to be used in [portfolios] and [importance] functions.
    #' @return Invisible `TRUE`.
    set_variable_lb = function(index, value) {
      # nocov start
      cli::cli_abort("No defined $set_variable_lb method.", .internal = TRUE)
      # nocov end
    },

    #' @description
    #' Set the right-hand-side coefficient bound for a constraint.
    #' @param index `integer` value indicating the index of the decision
    #' variable.
    #' @param value `numeric` new value.
    #' @details Note that this method should only be run after `$calculate()`.
    #' It can be used to overwrite values after ingesting an
    #' [optimization_problem()] object.
    #' It is designed to be used in [portfolios] and [importance] functions.
    #' @return Invisible `TRUE`.
    set_constraint_rhs = function(index, value) {
      # nocov start
      cli::cli_abort("No defined $set_constraint_rhs method.", .internal = TRUE)
      # nocov end
    },

    #' @description
    #' Set the starting solution.
    #' @param value `numeric` vector.
    #' @param warn `logical` indicating if a warning should be displayed
    #' if the solver does not support starting solutions.
    #' @details This method is designed used in [portfolios] and [importance]
    #' functions.
    #' @return Invisible `TRUE`.
    set_start_solution = function(value, warn = TRUE) {
      if ("start_solution" %in% names(self$data)) {
        self$data$start_solution <- value
      } else if (isTRUE(warn)) {
        cli_warning("Solver does not support starting solutions.", call = NULL)
      }
      invisible(TRUE)
    },

    #' @description
    #' Remove the starting solution.
    #' @details This method is designed used in [portfolios] and [importance]
    #' functions.
    #' @return Invisible `TRUE`.
    remove_start_solution = function() {
      if ("start_solution" %in% names(self$data)) {
        self$data$start_solution <- NULL
      }
      invisible(TRUE)
    },

    #' @description
    #' Solve an optimization problem.
    #' @param x [optimization_problem()] object.
    #' @param ... Additional arguments passed to the `calculate()` method.
    #' @return A `list` object with the solution and additional information.
    solve = function(x, ...) {
      # build optimization problem
      self$calculate(x, ...)
      # run solver and get solution
      out <- self$run()
      # clear internal store to reduce memory consumption
      self$internal <- list()
      # return output
      out
    },
    #' @description
    #' Solve a multi-objective optimization problem using a hierarchical
    #' multi-objective optimization approach.
    #' Broadly speaking, this approach involves using multiple optimization
    #' procedures to solve objectives following a hierarchical (lexicographic)
    #' ordering, wherein those associated with a higher priority order are
    #' solved before those with a lower priority order. When implementing this
    #' approach, constraints are added after generating a given solution to
    #' ensure that subsequent solutions for lower priority objectives
    #' have adequate performance according to higher priority objectives.
    #' @param x `list` object with multi-objective optimization problem.
    #' Arguments must contain the following elements:
    #' (`"opt"`) [`OptimizationProblem-class`] object;
    #' (`"modelsense"`) `character` vector containing the model sense values
    #' for each objective; and (`"obj"`) numeric` matrix containing the
    #' coefficients for each of the objectives, wherein rows correspond to
    #' different objectives, columns to different decision variables and
    #' row names can be optionally specify names for the objectives.
    #' @param priority `numeric` vector with values indicating the
    #' priority for each objective. Greater values denote greater priority,
    #' and so objectives associated with greater values are optimized
    #' earlier in the multi-objective process.
    #' @param rel_tol `numeric` vector with relative tolerance values
    #' for each constraint. Greater values denote a greater degree of
    #' sub-optimality.
    #' @param ... Additional arguments passed to the `calculate()` method.
    #' @return A `list` object with the solution and additional information.
    default_solve_multiobj = function(x, priority, rel_tol, ...) {
      # assert arguments are valid
      assert(
        is.list(x),
        is.matrix(x$obj),
        is.character(x$modelsense),
        is.numeric(priority),
        is.numeric(rel_tol),
        nrow(x$obj) == length(priority),
        nrow(x$obj) == length(rel_tol) + 1,
        nrow(x$obj) == length(x$modelsense),
        assertthat::noNA(priority),
        assertthat::noNA(rel_tol),
        all(rel_tol >= 0),
        .internal = TRUE
      )

      # set objective names
      obj_names <- rownames(x$obj)
      if (is.null(obj_names)) {
        obj_names <- paste0("objective_", seq_len(nrow(x$obj)))
      }

      # store initial model components
      init_modelsense <- x$opt$modelsense()
      init_obj <- x$opt$obj()

      # initialization
      n_obj <- nrow(x$obj)
      n_dv <- ncol(x$obj)
      n_extra_constraints <- 0

      # find order according to priority
      solve_order <- order(priority, decreasing = TRUE)

      # perform optimization
      for (i in seq_along(solve_order)) {
        ## set current objective
        x$opt$set_obj(x$obj[solve_order[[i]], ])
        x$opt$set_modelsense(x$modelsense[[solve_order[[i]]]])
        ## solve problem
        sol <- self$solve(x$opt, ...)
        ## if feasible solution found and there are remaining objectives,
        ## then add linear constraint for next iteration
        if (!is.null(sol) && !is.null(sol$x) && !identical(i, n_obj)) {
          ## increment counter
          n_extra_constraints <- n_extra_constraints + 1
          ## calculate values for rhs constraint for next objective
          rhs <-
            sum(x$obj[solve_order[[i]], ] * sol$x) *
            ifelse(
              x$modelsense[[solve_order[[i]]]] == "min",
              1 + rel_tol[[i]],
              1 - rel_tol[[i]]
            )
          ## add constraint
          x$opt$append_linear_constraints(
            rhs = rhs,
            sense = ifelse(
              x$modelsense[[solve_order[[i]]]] == "min",
              "<=",
              ">="
            ),
            A = Matrix::drop0(Matrix::sparseMatrix(
              i = rep(1, n_dv),
              j = seq_len(n_dv),
              x = x$obj[solve_order[[i]], ],
              dims = c(1, n_dv)
            )),
            row_ids = "h"
          )
          ## set start solution
          self$set_start_solution(sol$x, warn = FALSE)
        } else {
          ## otherwise, if solution is not feasible, then exit the loop
          break
        }
      }

      # clean up
      ## reset obj
      x$opt$set_obj(init_obj)
      ## reset modelsense
      x$opt$set_modelsense(init_modelsense)
      ## remove all linear constraints that were added to x$opt
      for (i in seq_len(n_extra_constraints)) {
        x$opt$remove_last_linear_constraint()
      }

      # compute objective value for each objective
      if (!is.null(sol$x)) {
        sol$objective <- stats::setNames(
          rowSums(
            x$obj * matrix(sol$x, ncol = n_dv, nrow = n_obj, byrow = TRUE)
          ),
          rownames(x$obj)
        )
      }

      # return solution
      sol
    },
    #' @description
    #' Solve a multi-objective optimization problem using a hierarchical
    #' multi-objective optimization approach.
    #' Broadly speaking, this approach involves using multiple optimization
    #' procedures to solve objectives following a hierarchical (lexicographic)
    #' ordering, wherein those associated with a higher priority order are
    #' solved before those with a lower priority order. When implementing this
    #' approach, constraints are added after generating a given solution to
    #' ensure that subsequent solutions for lower priority objectives
    #' have adequate performance according to higher priority objectives.
    #' @param x `list` object with multi-objective optimization problem.
    #' Arguments must contain the following elements:
    #' (`"opt"`) [`OptimizationProblem-class`] object;
    #' (`"modelsense"`) `character` vector containing the model sense values
    #' for each objective; and (`"obj"`) numeric` matrix containing the
    #' coefficients for each of the objectives, wherein rows correspond to
    #' different objectives, columns to different decision variables and
    #' row names can be optionally specify names for the objectives.
    #' @param priority `numeric` vector with values indicating the
    #' priority for each objective. Greater values denote greater priority,
    #' and so objectives associated with greater values are optimized
    #' earlier in the multi-objective process.
    #' @param rel_tol `numeric` vector with relative tolerance values
    #' for each constraint. Greater values denote a greater degree of
    #' sub-optimality.
    #' @param ... Additional arguments passed to the `calculate()` method.
    #' @return A `list` object with the solution and additional information.
    solve_multiobj = function(x, priority, rel_tol, ...) {
      # assert valid arguments
      assert(
        is.numeric(priority),
        is.numeric(rel_tol),
        .internal = TRUE
      )
      # solve multi-objective optimization problem
      self$default_solve_multiobj(x, priority, rel_tol, ...)
    }
  )
)
