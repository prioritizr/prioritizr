#' Skip if no fast solvers
#'
#' Skip a test if no fast solvers are installed.
#'
#' @param packages `character` vector containing the package dependencies
#'   for fast solvers. Defaults to `"gurobi"`, `"cplexAPI"`, and `"rcbc"`.
#'
#' @return `logical` indicating success.
skip_if_no_fast_solvers_installed <- function(
  packages = c("gurobi", "cplexAPI", "rcbc")) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(packages), assertthat::noNA(packages))
  # check if any dependencies present
  result <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  # skip if none installed
  if (any(result)) {
    return(invisible(TRUE))
  }
  testthat::skip("No fast solvers installed")
}

#' Skip if no commercial solvers
#'
#' Skip a test if no commercial solvers are installed.
#'
#' @param packages `character` vector containing the package dependencies
#'   for commercial solvers. Defaults to `"gurobi"` and `"cplexAPI"`.
#'
#' @return `logical` indicating success.
skip_if_no_commercial_solvers_installed <- function(
  packages = c("gurobi", "cplexAPI")) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(packages), assertthat::noNA(packages))
  # check if any dependencies present
  result <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  # skip if none installed
  if (any(result)) {
    return(invisible(TRUE))
  }
  testthat::skip("No commercial solvers installed")
}

#' Skip if no fast solvers
#'
#' Skip a test if no fast solvers are installed.
#'
#' @param packages `character` vector containing the package dependencies
#'   for fast solvers.
#'
#' @return `logical` indicating success.
skip_if_no_fast_solvers_installed <- function(
  packages = c("gurobi", "cplexAPI", "rcbc")) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(packages), assertthat::noNA(packages))
  # check if any dependencies present
  result <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  # skip if none installed
  if (any(result)) {
    return(invisible(TRUE))
  }
  testthat::skip("No fast solvers installed")
}

#' Skip if no solvers installed at all
#'
#' Skip a test if no solvers are installed.
#'
#' @return `logical` indicating success.
skip_if_no_solvers_installed <- function() {
  # check if any dependencies present
  result <- any_solvers_installed()
  # skip if none installed
  if (result) {
    return(invisible(TRUE))
  }
  testthat::skip("No solvers installed")
}
