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
    #' @param rel_tol numeric vector of coefficients
    #' @param ... Additional arguments passed to the `calculate()` method.
    #' @return Invisible `TRUE`.
    solve_multiobj = function(x, rel_tol, ...) {

      mobj <- x$obj
      mmodelsense <- x$modelsense
      mopt <- x$opt

      rel_tol <- as.matrix(rel_tol)

      sols_inter <- vector("list", length = nrow(mobj))

      for (i in seq_len(nrow(mobj))) {
        #if (verbose) cli::cli_alert_info("Solving objective {i}/{nrow(mobj)}")
        
        # set current objective
        mopt$set_obj(mobj[i, ])
        mopt$set_modelsense(mmodelsense[i])

        # solve problem directly
        sols_inter[i] <- list(self$solve(mopt))
       # sols_inter[[i]] <- self$solve(mopt)
        #browser()

        if (i != nrow(mobj)) {
          # calculate hierarchical constraint for next objective
          print( sum(mobj[i, ] * sols_inter[[i]]$x))
          print(sols_inter[[i]]$objective )
          
          rhs <- 
            #sols_inter[[i]]$objective *
            sum(mobj[i, ] * sols_inter[[i]]$x) * #what we had previously. Smaller than what is returned by solver
            ifelse(mmodelsense[i] == "min", 1 + rel_tol[1, i], 1 - rel_tol[1, i])
          
          print(rhs)
          sense <- ifelse(mmodelsense[i] == "min", "<=", ">=")

      #  if (verbose) cli::cli_alert_info("Adding hierarchical constraint for next objective: rhs={rhs}, sense={mmodelsense}")
          mopt$append_linear_constraints(
            rhs = rhs,
            sense = sense,
            A = Matrix::drop0(Matrix::sparseMatrix(
              i = rep(1, length(mobj[i, ])),
              j = seq_along(mobj[i, ]),
              x = mobj[i, ],
              dims = c(1, length(mobj[i, ]))
            )),
            row_ids = "h"
          )
        }
      }

      sol <- sols_inter[[nrow(mobj)]] # only get last solution

      ### compute and store objective values for each objective
      if (!is.null(sol$x)) {
        sol$objective <- stats::setNames(
          rowSums(
            x$obj *
              matrix(
                sol$x,
                ncol = ncol(x$obj),
                nrow = nrow(x$obj), byrow = TRUE
              )
          ),
          rownames(x$obj)
        )
      }

      sol
    }
  )
)
