#' Solve with a fixed seed
#'
#' This function is a wrapper for [prioritizr::solve()] that imposes
#' a fixed seed when solving the problem.
#'
#' @param x [problem()] object.
#'
#' @param ... passed to [prioritizr::solve()].
#'
#' @param seed `integer` denoting the random number state. Defaults to 500.
#'
#' @details
#' This function is useful for reproducibility when using the shuffle
#' portfolio ([add_shuffle_portfolio()]).
#'
#' @return The output from running [prioritizr::solve()] on the `x`.
solve_fixed_seed <- function(x, ..., seed = 500) {
  withr::with_seed(500, solve(x, ...))
}
