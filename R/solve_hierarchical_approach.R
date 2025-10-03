#' @include internal.R
NULL

#' Solve problems with hierarchical multi-objective optimization
#'
#' @param x `list` of [problem()] objects.
#'
#' @param rel_tol `numeric` vector with optimality values.
#' The number of values in `rel_tol` should be equal to the length of `x`
#' minus one.
#'
#' @param method `character` value indicating name of the method to use
#' for solving the problem. Available options include performing the
#' optimization process using (`"gurobi`) Gurobi's built-in functionality
#' (`"manual"`) a manual implementation.
#'
#' @inherit solve return
#'
#' @noRd
solve_hierarchical_approach <- function(x, rel_tol, method = "gurobi") {
  # assert that arguments are valid
  assert_required(x)
  assert_required(rel_tol)
  assert(
    is.list(x),
    all_elements_inherit(x, "ConservationProblem"),
    is.numeric(rel_tol),
    all_positive(rel_tol),
    assertthat::is.string(method),
    assertthat::noNA(method),
    is_match_of(method, c("gurobi", "manual"))
  )

  # compile multi-objective problem
  mopt_list <- compile_hierarchical_approach(x = x)

  # extract components
  mopt <- mopt_list$mopt               # OptimizationProblem
  mobj <- mopt_list$obj                # matrix with the obj for each problem
  mmodelsense <- mopt_list$modelsense  # vector with modelsense for each problem

  # initialize variables
  prev_sol <- NULL

  # solve the problem using the specified method
  if (identical(method, "gurobi")) {
    # TODO
    stop("TODO")
  } else {
    # iterate over each objective and solve the problem hierarchically
    for (i in seq_len(nrow(mobj))) {
      ## set the problem to the i'th objective
      mopt$set_obj(mobj[i, ])
      mopt$set_modelsense(mmodelsense[[i]])
      ## solve the problem
      curr_sol <- x[[1]]$portfolio$run(mopt, x[[1]]$solver)
      assert(
        is_valid_raw_solution(s, time_limit = x[[1]]$solver$data$time_limit)
      )
      ## calculate rhs value for new constraint
      curr_rhs <-
        sum(mobj[i, ] * curr_sol[[1]]$x) *
        ifelse(
          mmodelsense$modelsense[[1]] == "min",
          1 + rel_tol[[1]],
          1 - rel_tol[[1]]
        )
      ## apply new constraint
      mult$append_linear_constraints(
        rhs = curr_rhs,
        sense = ifelse(mmodelsense$modelsense[[1]] == "min", "<=", ">="),
        A = Matrix::drop0(
          Matrix::sparseMatrix(
            i = rep(1, ncol(mobj)),
            j = seq_len(ncol(mobj)),
            x = main_obj,
            dims = c(1, length(main_obj))
          )
        ),
        row_ids = "h"
      )
      ## if possible, update the starting solution for the solver
      if (
        !is.null(x$solver$data) &&
        isTRUE("start_solution" %in% names(x$solver$data))
      ) {
        x$solver$data$start_solution <- curr_sol[[1]]$x
      }
    }
  }

  # return solution
  solve_solution_format(
     x = planning_unit_solution_format(
      x = x[[1]],
      status = lapply(sol, convert_raw_solution_to_solution_status, x = x[[1]]),
      prefix = paste0("solution_", seq_along(sol)),
      append = TRUE
    ),
    raw_solution = sol
  )
}

#' Compile problems for hierarchical multi-objective optimization
#'
#' @inheritParams solve_hierarchical_approach
#'
#' @return
#' A `list` containing the following elements: (`ptr`)
#' [OptimizationProblem-class] object, (`obj`) `numeric` matrix with the
#' objective coefficients (each row is a different objective, and each
#' column is a different variable), (`modelsense`) `character` vector
#' with the model sense for each objective.
#'
#' @noRd
compile_hierarchical_approach <- function(x) {
  # assert that arguments are valid
  assert_required(x)
  assert(
    ## x
    is.list(x),
    length(x) >= 2,
    all_elements_inherit(x, "ConservationProblem"),
    msg = paste(
      "{.arg x} must be a {.cls list} of ",
      "{.fn prioritizr::problem} objects."
    ),
    all_problems_comparable(x) # TODO
  )

  # compile problems and extract pointers
  opt <- lapply(x, compile)
  ptr <- lapply(opt, function(p) p$ptr)

  # generate optimization problem
  out <- rcpp_apply_hierarchical_approach(ptr)

  # convert ptr to optimization problem
  out$mopt <- OptimizationProblem$new(out$mopt)

  # return result
  out
}
