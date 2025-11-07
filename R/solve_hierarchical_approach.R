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
  mopt <- mopt_list$mopt # OptimizationProblem
  mobj <- mopt_list$obj # matrix with the obj for each problem
  mmodelsense <- mopt_list$modelsense # vector with modelsense for each problem

  # initialize variables
  prev_sol <- NULL
  
  browser()
  
  # solve the problem using the specified method
  if (identical(method, "gurobi")) {
    # assert first problem has Gurobi
    if (x[[1]]$solver$name != "gurobi solver") {
      stop('Gurobi solver is needed in the problem with highest priority for the method "gurobi"')
    }
    
    # warn if other problems have different solvers
    other_solvers <- vapply(x[-1], function(p) p$solver$name, character(1))
    if (any(other_solvers != "gurobi solver")) {
      warning("While the first problem has Gurobi, other problems seem to have other solvers specified.")
    }
    
    # extract parameters directly from the first problem's solver
    solver_data <- x[[1]]$solver$data

    p <- list(
      LogToConsole = as.numeric(solver_data$verbose),
      LogFile = "",
      Presolve = solver_data$presolve,
      MIPGap = solver_data$gap,
      TimeLimit = solver_data$time_limit,
      Threads = solver_data$threads,
      NumericFocus = as.numeric(solver_data$numeric_focus) * 2,
      NodeFileStart = solver_data$node_file_start,
      SolutionLimit = as.numeric(solver_data$first_feasible)
    )

    # clean up missing or invalid ones
    if (p$SolutionLimit == 0) p$SolutionLimit <- NULL
    if (p$NodeFileStart < 0) p$NodeFileStart <- NULL

    # add custom control parameters, if present
    control <- solver_data$control
    if (length(control) > 0) {
      p[names(control)] <- control
    }

    # build model
    model <- list(
      A = mopt$A(),
      rhs = mopt$rhs(),
      sense = mopt$sense(),
      vtype = mopt$vtype(),
      lb = mopt$lb(),
      ub = mopt$ub(),
      modelsense = "min",
      multiobj = lapply(seq_len(nrow(mobj)), function(i) {
        list(
          objn = if (mmodelsense[i] == "min") mobj[i, ] else -mobj[i, ],
          priority = nrow(mobj) - i + 1,
          weight = 1.0,
          reltol = if (i < nrow(mobj)) rel_tol[i] else NULL,
          name = paste0("Objective_", i)
        )
      })
    )

    # run gurobi with these parameters
    res <- withr::with_locale(
      c(LC_CTYPE = "C"),
      gurobi::gurobi(model = model, params = p)
    )

    # fix potential floating point rounding issues for binary vars
    b <- model$vtype == "B"
    if (is.numeric(res$x)) res$x[b] <- round(res$x[b])

    # extract solution
    out <- list(
      x = res$x,
      objective = res$objval,
      status = res$status,
      gap = res$mipgap,
      objbound = res$objbound,
      runtime = res$runtime
    )

    curr_sol <- list(
      list(
        x = out$x,
        objective = out$objective[length(out$objective)],
        status = if (!is.null(out$status)) out$status else "UNKNOWN",
        gap = if (!is.null(out$mipgap)) out$mipgap else NA_real_,
        objbound = if (!is.null(out$objbound)) out$objbound else NA_real_,
        runtime = if (!is.null(out$runtime)) out$runtime else NA_real_
      )
    )
  } else {
    # iterate over each objective and solve the problem hierarchically
    for (i in seq_len(nrow(mobj))) {
      ## set the problem to the i'th objective
      mopt$set_obj(mobj[i, ])
      mopt$set_modelsense(mmodelsense[[i]])
      ## solve the problem
      curr_sol <- x[[1]]$portfolio$run(mopt, x[[1]]$solver)
      assert(
        is_valid_raw_solution(curr_sol, time_limit = x[[1]]$solver$data$time_limit)
      )

      if (i != nrow(mobj)) { # otherwise we try to do the same calcs for the last problem where we wont have a rel_tol
        ## calculate rhs value for new constraint
        curr_rhs <-
          sum(mobj[i, ] * curr_sol[[1]]$x) *
            ifelse(
              mmodelsense[i] == "min", # get current modelsense so we can adapt next problem
              1 + rel_tol[i],
              1 - rel_tol[i]
            )
        ## apply new constraint
        mopt$append_linear_constraints(
          rhs = curr_rhs,
          sense = ifelse(mmodelsense[i] == "min", "<=", ">="), # $modelsense[[1]] == "min", "<=", ">="),
          A = Matrix::drop0(
            Matrix::sparseMatrix(
              i = rep(1, ncol(mobj)),
              j = seq_len(ncol(mobj)),
              x = mobj[i, ], # main_obj,
              dims = c(1, length(mobj[i, ])) # (main_obj))
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
  }

  # return solution
  solve_solution_format(
    x = planning_unit_solution_format(
      x = x[[1]],
      status = lapply(curr_sol, convert_raw_solution_to_solution_status, x = x[[1]]),
      prefix = paste0("solution_", seq_along(curr_sol)),
      append = TRUE
    ),
    raw_solution = curr_sol
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
