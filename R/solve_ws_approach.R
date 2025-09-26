#' @include internal.R
NULL

#' Solve problems with weighted sum multi-objective optimization
#'
#' @param x `list` of [problem()] objects.
#'
#' @param weights `numeric` vector with weight values.
#' The argument should contain a value for each element in `x`.
#'
#' @param rescale_weights `logical` indicating if the `weights` should
#' be rescaled so that they sum to a total of 1. Defaults to `FALSE`.
#'
#' @inherit solve return
#'
#' @noRd
solve_ws_approach <- function(x, weights, rescale_weights = FALSE) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(weights)
  assert_required(rescale_weights)

  # compile multi-objective problem
  mopt <- ws_approach_compile(
    x = x, weights = weights, rescale_weights = rescale_weights
  )

  # solve problem
  sol <- x[[1]]$portfolio$run(mopt, x[[1]]$solver)

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

#' Compile problems with weighted sum multi-objective optimization
#'
#' @inheritParams solve_ws_approach
#'
#' @inherit solve return
#'
#' @noRd
compile_ws_approach <- function(x, weights, rescale_weights = FALSE) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(weights)
  assert_required(rescale_weights)
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
  assert(
    ## weights
    is.numeric(weights),
    all_finite(weights),
    all_positive(weights),
    length(weights) == length(x),
    ## rescale_weights
    assertthat::is.flag(rescale_weights),
    assertthat::noNA(rescale_weights)
  )

  # if needed, then rescale weights
  if (isTRUE(rescale_weights)) {
    weights <- weights / sum(weights)
  }

  # compile problems and extract pointers
  opt <- lapply(x, compile)
  ptr <- lapply(opt, function(p) p$ptr)

  # return result
  OptimizationProblem$new(
    ptr = rcpp_apply_ws_approach(ptr, weights)
  )
}
