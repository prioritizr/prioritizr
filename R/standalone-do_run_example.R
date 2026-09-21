# ---
# repo: prioritizr/prioritizr
# file: standalone-do_run_example.R
# imports: [cli (>= 3.6.0), rlang (>= 1.1.0)]
# ---

#' Do run example?
#'
#' Determine if the session is suitable for executing long-running a
#' example.
#'
#' @param packages `character` vector containing the names of packages
#' required for the example to complete successfully.
#' Defaults to `NULL` such that no dependencies are required.
#'
#' @details
#' This function will return `TRUE` if the session is interactive.
#' Otherwise, it will only return `TRUE` if the session does not
#' have system environmental variables that indicate that the session
#' is being used for package checks, or for building documentation.
#' Note that this function will always return `FALSE` if any of the specified
#' `packages` are not installed.
#'
#' @return A `logical` value.
#'
#' @examples
#' # should examples be run in current environment?
#' do_run_example()
#'
#' @noRd
do_run_example <- function(packages = NULL) {
  # if any dependencies are not installed, return FALSE.
  if (!is.null(packages)) {
    if (!is.character(packages)) {
      cli::cli_abort("{.arg packages} must be a {.cls character} vector.")
    }
    all_installed <- all(
      vapply(
        packages,
        function(x) requireNamespace(x, quietly = TRUE),
        FUN.VALUE = logical(1)
      )
    )
    if (!isTRUE(all_installed)) return(FALSE)
  }
  # if interactive, always return TRUE
  if (rlang::is_interactive()) return(TRUE)
  # if check environment, then initially set to TRUE
  is_check <-
    ("CheckExEnv" %in% search()) ||
    any(c("_R_CHECK_TIMINGS_", "_R_CHECK_LICENSE_") %in% names(Sys.getenv())) ||
    !identical(Sys.getenv("MY_UNIVERSE"), "") ||
    any(c("CI", "GITHUB_ACTIONS", "GITHUB_SHA") %in% names(Sys.getenv()))
  # however, if in environment for building package website,
  # then override with FALSE
  if (
    identical(Sys.getenv("PKGDOWN_BUILD"), "true") ||
    identical(Sys.getenv("IN_PKGDOWN"), "true")
  ) {
    is_check <- FALSE
  }
  # return result
  return(!is_check)
}
