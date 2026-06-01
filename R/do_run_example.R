#' Do run example?
#'
#' Determine if the session is suitable for executing long-running a
#' example.
#'
#' @details
#' This function will return `TRUE` if the session is interactive.
#' Otherwise, it will only return `TRUE` if the session does not
#' have system environmental variables that indicate that the session
#' is being used for package checks, or for building documentation.
#'
#' @return A `logical` value.
#'
#' @examples
#' # should examples be run in current environment?
#' do_run_example()
#'
#' @export
do_run_example <- function() {
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
