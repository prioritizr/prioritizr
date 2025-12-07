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
        cli_warning("Solver does not support starting solutions.", call = FALSE)
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
    #' @return Invisible `TRUE`.
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
    #' Solve an optimization problem.
    #' @param x [optimization_problem()] object.
    #' @param priority numeric vector of the order of the supplied problems TODO: also allow a character vector with problem names
    #' @param rel_tol numeric vector of coefficients
    #' @param ... Additional arguments passed to the `calculate()` method.
    #' @return Invisible `TRUE`.
    solve_multiobj = function(x, priority = NULL, rel_tol = NULL, ...) {
      # assert arguments are valid
      ## TODO
      # initialization
      n_obj <- nrow(x$obj)
      n_dv <- ncol(x$obj)
      init_modelsense <- x$opt$modelsense()
      init_obj <- x$opt$obj()
      n_extra_constraints <- 0
      if (is.null(rel_tol)) {
        rel_tol <- rep(0, nrow(x$obj) - 1)
      }
      if (is.null(priority)) {
        priority <- seq(nrow(x$obj), 1)
      }
      
      # Reorder according to priority 
      solve_order <- order(priority, decreasing = TRUE)
      mobj <- x$obj[solve_order, , drop = FALSE]
      mmodelsense <- x$modelsense[solve_order]

      # perform optimization
      for (i in seq_len(n_obj)) {
        ## set current objective
        x$opt$set_obj(mobj[i, ])
        x$opt$set_modelsense(mmodelsense[[i]])
        ## solve problem
        sol <- self$solve(x$opt, ...)
        ## if feasible solution found and there are remaining objectives,
        ## then add linear constraint for next iteration
        if (!is.null(sol) && !is.null(sol$x) && !identical(i, n_obj)) {
          ## increment counter
          n_extra_constraints <- n_extra_constraints + 1
          print(rel_tol[[i]])
          ## calculate values for rhs constraint for next objective
          rhs <-
            sum(x$obj[i, ] * sol$x) *
            ifelse(
              x$modelsense[[i]] == "min",
              1 + rel_tol[[i]],
              1 - rel_tol[[i]]
            )
          ## add constraint
          x$opt$append_linear_constraints(
            rhs = rhs,
            sense = ifelse(x$modelsense[[i]] == "min", "<=", ">="),
            A = Matrix::drop0(Matrix::sparseMatrix(
              i = rep(1, n_dv),
              j = seq_len(n_dv),
              x = x$obj[i, ],
              dims = c(1, n_dv)
            )),
            row_ids = "h"
          )
          ## set start solution
          self$set_start_solution(sol$x, warn = FALSE)
        } else {
          ## otherwise, exit the loop
          break
        }
      }
      
      # clean up
      ## reset obj
      x$opt$set_obj(init_obj)
      ## reset modelsense
      x$opt$set_modelsense(init_modelsense)
      ## remove linear constraints that were added to x$opt
      for (i in seq_len(n_extra_constraints)) {
        x$opt$remove_last_linear_constraint()
      }
      
      # compute objective value for each objective
      if (!is.null(sol$x)) {
        sol$objective <- stats::setNames(
          rowSums(
            x$obj *
              matrix(sol$x, ncol = n_dv, nrow = n_obj, byrow = TRUE)
          ),
          rownames(x$obj)
        )
      }
      
      # return solution
      sol
    }
  )
)
