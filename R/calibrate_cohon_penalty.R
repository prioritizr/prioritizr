#' @include internal.R
NULL

#' Calibrate penalties with Cohon's method
#'
#' Identify a penalty value that represents a suitable compromise between the
#' primary objective and a penalty for a conservation planning problem.
#' This is accomplished following the multi-objective algorithm developed by
#' Cohon *et al.* (1979) that was later adapted for
#' systematic conservation planning
#' (Ardron *et al.* 2010; Fischer and Church 2005).
#'
#' @param x [problem()] object with a penalty.
#'
#' @param approx `logical` value indicating if an approximation method should be
#' used for the calculations. Defaults to `TRUE` to reduce computational burden.
#' See Details section for more information.
#'
#' @inheritParams add_gurobi_solver
#'
#' @details
#' This function provides a routine for implementing Cohon's method
#' (1979) to identify a suitable penalty value.
#' It can be used calibrate a broad range of penalties,
#' including boundary penalties ([add_boundary_penalties()]),
#' connectivity penalties ([add_connectivity_penalties()]),
#' asymmetric connectivity penalties ([add_asym_connectivity_penalties()]),
#' and linear penalties ([add_linear_penalties()]).
#' Note that the penalty value identified by this function is calculated
#' in a manner that reflects the overall problem formulation (per `x`).
#' Thus if you are considering multiple scenarios that consider different
#' objectives, constraints, penalties, targets, decision types, or underlying
#' datasets, then you will likely need to re-run the calibration process
#' to identify a suitable penalty value for each scenario (separately).
#'
#' The suitability of the resulting penalty value depends on the optimality
#' gap used during optimization, as well as whether the approximation method
#' is used or not. In particular, a gap of zero will result in the best
#' estimate, and a gap greater than zero may result in worse estimates.
#' It is recommended to keep the optimality gap low (e.g., between 0 and 0.1),
#' and a relatively small gap may be needed in some cases
#' (e.g., 0, 0.01 or 0.05).
#' Additionally, the approximation method (i.e., with `approx = TRUE`)
#' may result in penalty values that do not represent a suitable compromise
#' between the objective and the penalty. Although this will happen if
#' there are multiple optimal solutions to the primary objective or the
#' penalty; in practice, this is unlikely to be an issue when considering a
#' moderate number of features and planning units. If this is an issue,
#' then a more robust approach can be employed
#' (i.e., by setting `approx = FALSE`) that uses additional
#' optimization routines to potentially obtain a better
#' estimate of a suitable penalty value.
#' As such, it is recommended to try running the function with default
#' settings and see if the resulting penalty value is suitable. If not,
#' then try running it with a smaller optimality gap or
#' the robust approach.
#'
#' @section Mathematical formulation:
#' A suitable penalty value is identified using the following procedure.
#'
#' 1. The optimal value for the primary objective is calculated
#' (referred to as `solution_1_objective` in the output). This
#' is accomplished by solving the problem without the penalty. For example,
#' if considering a minimum set problem with boundary penalties, then this value
#' would correspond to the cost of the solution that has the
#' smallest cost (whilst meeting all the targets).
#' 2. The best possible penalty value given that the solution must be optimal
#' according to the primary objective is calculated
#' (referred to as `solution_1_penalty` in the output).
#' For example, if considering a minimum set problem with boundary penalties,
#' then this value would correspond to the smallest possible total boundary
#' length that is possible when a solution must have minimum cost
#' (whilst meeting all the targets).
#' If using the approximation method (per `approx = TRUE`), this value is
#' estimated based on the penalty value of the solution produced in the
#' previous step.
#' Otherwise, if using the robust method (per `approx = FALSE`), this
#' value is calculated by performing an additional optimization routine to
#' ensure that this value is correct when there are multiple
#' optimal solutions.
#' 3. The optimal value for the penalty is calculated
#' (referred to as `solution_2_penalty` in the output). This is
#' accomplished by modifying the problem so that it only focuses on
#' minimizing the penalty as much as possible
#' (i.e., ignoring the primary objective) and solving it.
#' For example, if considering a minimum set problem with boundary penalties,
#' then this value would correspond to the total boundary length of the
#' solution that has the smallest total boundary length (while still meeting
#' all the targets).
#' 4. The best possible objective value given that the solution must be optimal
#' according to the penalties is calculated
#' (referred to as `solution_2_objective` in the output).
#' For example, if considering a minimum set problem with boundary penalties,
#' then this value would correspond to the smallest possible cost
#' that is possible when a solution must have the minimum boundary length
#' (whilst meeting all the targets).
#' If using the approximation method (per `approx = TRUE`), this value is
#' estimated based on the objective value of the solution produced in the
#' previous step.
#' Otherwise, if using the robust method (per `approx = FALSE`), this
#' value is calculated by performing an additional optimization routine to
#' ensure that this value is correct when there are multiple
#' optimal solutions.
#' 5. After completing the previous calculations, the suitable penalty value
#' is calculated using the following equation. Here, the values calculated in
#' steps 1, 2, 3, and 4 correspond to \eqn{a}{a}, \eqn{b}{b}, \eqn{c}{c}, and
#' \eqn{d}{d} (respectively).
#' \deqn{ \left| \frac{(a - c)}{(b - d)} \right|}{|(a - c) / (b - d)|}
#'
#' @return
#' A `numeric` value corresponding to the calibrated penalty value.
#' Additionally, this value has attributes that contain the values used to
#' calculate the calibrated penalty value. These attributes include the
#' (`solution_1_objective`) optimal objective value, (`solution_1_penalty`)
#' best possible penalty value given that the solution must be optimal
#' according to the primary objective, (`solution_2_penalty`) optimal
#' penalty value, and (`solution_2_objective`) best possible objective value
#' given that a solution must be optimal according to the penalties.
#'
#' @seealso
#' See [penalties] for an overview of all functions for adding penalties.
#'
#' @references
#' Ardron JA, Possingham HP, and Klein CJ (eds) (2010) Marxan Good Practices
#' Handbook, Version 2. Pacific Marine Analysis and Research Association,
#' Victoria, BC, Canada.
#'
#' Cohon JL, Church RL, and Sheer DP (1979) Generating multiobjective
#' trade-offs: An algorithm for bicriterion problems.
#' *Water Resources Research*, 15: 1001--1010.
#'
#' Fischer DT and Church RL (2005) The SITES reserve selection system: A
#' critical review. *Environmental Modeling and Assessment*, 10: 215--228.
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create problem with boundary penalties
#' ## note that we use penalty = 1 as a place-holder
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_boundary_penalties(penalty = 1) %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # find calibrated boundary penalty using Cohon's method
#' cohon_penalty <- calibrate_cohon_penalty(p1, verbose = FALSE)
#'
#' # create a new problem with the calibrated boundary penalty
#' p2 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_boundary_penalties(penalty = cohon_penalty) %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2, main = "solution", axes = FALSE)
#' }
#' @export
calibrate_cohon_penalty <- function(x, approx = TRUE, verbose = TRUE) {
  # assert valid argument
  assert_required(x)
  assert_required(approx)
  assert_required(verbose)
  assert(
    is_conservation_problem(x),
    assertthat::is.flag(approx),
    assertthat::noNA(approx),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose),
    any_solvers_installed()
  )

  # preliminary calculations for indices
  weights_idx <- vapply(x$penalties, inherits, logical(1), "FeatureWeights")
  penalty_idx <- which(!weights_idx)
  weights_idx <- which(weights_idx)

  # additional checks
  assert(
    length(penalty_idx) > 0,
    msg = c(
      "{.arg x} must have a single penalty function.",
      "i" = "Note that this does not include feature weights.",
      "i" = "See {.help penalties} for penalty functions."
    )
  )
  assert(
    identical(length(penalty_idx), 1L),
    msg = c(
      "{.arg x} must not have multiple penalty functions.",
      "i" = "Note that this does not include feature weights."
    )
  )

  # copy object to ensure no changes made to global environment
  x <- x$clone(deep = TRUE)

  # ensure that the penalties have a penalty value of 1
  x$penalties[[penalty_idx]]$data$penalty <- 1

  # overwrite portfolio
  if (!inherits(x$portfolio, "DefaultPortfolio")) {
    x <- add_default_portfolio(x)
  }

  # overwrite verbose parameter in solver
  if (
    !is.null(x$solver$data) &&
    isTRUE("verbose" %in% names(x$solver$data)) &&
    is.logical(x$solver$data$verbose) &&
    identical(length(x$solver$data$verbose), 1L)
  ) {
    x$solver$data$verbose <- verbose
  }

  # compile the problem with the penalty
  ## note that error handling is here in case the problem cannot be compiled
  o1 <- try(compile(x), silent = TRUE)
  if (inherits(o1, "try-error")) {
    call <- attr(o1, "condition")$call
    if (is.null(call)) {
      call <- "error:" # nocov
    } else {
      call <- paste0("{.code ", deparse(substitute(call)), "}:")
    }
    cli::cli_abort(
      c(
        "{.arg x} failed to compile.",
        paste0("Caused by ", call),
        "!" = attr(o1, "condition")$message
      )
    )
  }

  # preliminary calculations
  o2 <- compile(x$remove_all_penalties(retain = "FeatureWeights"))
  n_extra_dv <- o1$ncol() - o2$ncol()
  main_obj <- c(o2$obj(), rep(0, n_extra_dv))
  penalties_obj <- o1$obj() - c(o2$obj(), rep(0, n_extra_dv))
  penalties_obj[abs(penalties_obj) < 1e-10] <- 0
  o1_original_lb <- o1$lb()
  o1_original_ub <- o1$ub()

  # find the optimal value for the primary objective
  ## solve the problem without the penalties (i.e., o2)
  if (verbose) cli::cli_h1("Optimizing main objective")
  s1 <- x$portfolio$run(o2, x$solver)
  assert(is_valid_raw_solution(s1, time_limit = x$solver$data$time_limit))

  # clean up
  rm(o2)
  invisible(gc())

  # if possible, update the starting solution for the solver to reduce run time
  if (
    !is.null(x$solver$data) &&
    isTRUE("start_solution" %in% names(x$solver$data))
  ) {
    x$solver$data$start_solution <- s1[[1]]$x
  }

  # find the optimal value for penalties, when constrained
  # to be optimal according to the primary objective
  if (isTRUE(approx)) {
    ## if using approximation method...
    ## we will just calculate the penalty value for the optimal solution
    ## according to the primary objective, and to do this we will
    ## create a new optimization problem that has the penalties and
    ## is constrained based on the previous solution - we will use this later to
    ## calculate the penalty values (e.g., if using boundary penalties, then
    ## this would be used to calculate the total boundary length of s1)
    o1$set_obj(penalties_obj)
    o1$set_ub(
      replace(
        o1_original_ub,
        seq_along(s1[[1]]$x),
        s1[[1]]$x
      )
    )
    o1$set_lb(
      replace(
        o1_original_lb,
        seq_along(s1[[1]]$x),
        s1[[1]]$x
      )
    )

    ## solve the constrained problem with penalties
    if (verbose) {
      cli::cli_h1("Approximating penalty values")
    }
    s2 <- x$portfolio$run(o1, x$solver)
    assert(is_valid_raw_solution(s2, time_limit = x$solver$data$time_limit))

    ## reset problem
    o1$set_lb(o1_original_lb)
    o1$set_ub(o1_original_ub)
  } else {
    ## if NOT using approximation method...
    ## create new problem with additional constraint based on the objective
    o1$set_obj(penalties_obj)
    o1$append_linear_constraints(
      rhs = sum(main_obj * c(s1[[1]]$x, rep(0, n_extra_dv))),
      sense = ifelse(o1$modelsense() == "min", "<=", ">="),
      A = Matrix::drop0(
        Matrix::sparseMatrix(
          i = rep(1, length(main_obj)),
          j = seq_along(penalties_obj),
          x = main_obj,
          dims = c(1, length(main_obj))
        )
      ),
      row_ids = "cohon"
    )

    ## if possible, update the starting solution for the solver
    if (
      !is.null(x$solver$data) &&
      isTRUE("start_solution" %in% names(x$solver$data))
    ) {
      x$solver$data$start_solution <- s1[[1]]$x
    }

    ## solve
    if (verbose) {
      cli::cli_h1("Optimizing penalties, constrained by objective")
    }
    s2 <- x$portfolio$run(o1, x$solver)
    assert(is_valid_raw_solution(s2, time_limit = x$solver$data$time_limit))

    ## reset problem
    o1$remove_last_linear_constraint()
  }

  # find the optimal value for the penalties
  ## note that we have previously set the objective to be based on the
  ## penalties objective so we don't need to modify the problem

  # if required, clear the starting solution from the solver
  if (
    !is.null(x$solver$data) &&
    isTRUE("start_solution" %in% names(x$solver$data))
  ) {
    x$solver$data$start_solution <- NULL
  }

  # solve the problem with penalties
  if (verbose) {
    cli::cli_h1("Optimizing penalties")
  }
  s3 <- x$portfolio$run(o1, x$solver)
  assert(is_valid_raw_solution(s3, time_limit = x$solver$data$time_limit))

  # find the optimal value for main objective, when constrained
  # to be optimal according to the penalties
  if (identical(approx, FALSE)) {
    ## if NOT using approximation method...
    ## we will create new problem with additional constraint based on the
    ## penalties
    o1$set_obj(main_obj)
    o1$append_linear_constraints(
      rhs = sum(penalties_obj * s3[[1]]$x),
      sense = ifelse(o1$modelsense() == "min", "<=", ">="),
      A = Matrix::drop0(
        Matrix::sparseMatrix(
          i = rep(1, length(penalties_obj)),
          j = seq_along(penalties_obj),
          x = penalties_obj,
          dims = c(1, length(penalties_obj))
        )
      ),
      row_ids = "cohon"
    )

    ## if possible, update the starting solution for the solver
    if (
      !is.null(x$solver$data) &&
      isTRUE("start_solution" %in% names(x$solver$data))
    ) {
      x$solver$data$start_solution <- s3[[1]]$x
    }

    ## solve the problem
    if (verbose) {
      cli::cli_h1("Optimizing main objective, constrained by penalties")
    }
    s3 <- x$portfolio$run(o1, x$solver)
    assert(is_valid_raw_solution(s3, time_limit = x$solver$data$time_limit))
  }

  # calculate data for Cohon's method
  s2_main_obj <- sum(main_obj * s2[[1]]$x)
  s2_penalty_obj <- sum(penalties_obj * s2[[1]]$x)
  s3_main_obj <- sum(main_obj * s3[[1]]$x)
  s3_penalty_obj <- sum(penalties_obj * s3[[1]]$x)

  # round to 6 decimal places precision,
  ## this is to avoid floating point issues
  s2_main_obj <- round(s2_main_obj, 6)
  s2_penalty_obj <- round(s2_penalty_obj, 6)
  s3_main_obj <- round(s3_main_obj, 6)
  s3_penalty_obj <- round(s3_penalty_obj, 6)

  # calculate penalty value
  out <-
    abs(s2_main_obj - s3_main_obj) /
    abs(s2_penalty_obj - s3_penalty_obj)

  # add attributes
  attr(out, "solution_1_objective") <- s2_main_obj
  attr(out, "solution_1_penalty") <- s2_penalty_obj
  attr(out, "solution_2_objective") <- s3_main_obj
  attr(out, "solution_2_penalty") <- s3_penalty_obj

  # check validity of result
  assert(
    all_finite(out),
    msg = c(
      "Couldn't find trade-offs between objective and penalty.",
      "i" =
        "This could happen due to strong positive correlations between them.",
      "i" = "This could also happen if the problem is highly constrained."
    )
  )

  # return result
  out
}
