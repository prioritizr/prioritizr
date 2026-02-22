#' @include internal.R presolve_check.R
NULL

#' All conservation planning problems comparable?
#'
#' @param x set of [problem()] objects.
#'
#' @return A `logical` value.
#'
#' @noRd
all_comparable_problem <- function(...) {
  x <- list(...)
  assert(
    is.list(x),
    all_elements_inherit(x, "ConservationProblem"),
    .internal = TRUE
  )
  isTRUE(
    all(vapply(
      lapply(x, zone_names), identical,
      logical(1), x[[1]]$zone_names()
    )) &&
    all(vapply(
      lapply(x, number_of_zones), identical,
      logical(1), x[[1]]$number_of_zones()
    )) &&
    all(vapply(
      lapply(x, number_of_planning_units), identical,
      logical(1), x[[1]]$number_of_planning_units()
    )) &&
    all(vapply(
      lapply(x, number_of_total_units), identical,
      logical(1), x[[1]]$number_of_total_units()
    )) &&
    all(vapply(
      lapply(x, function(z) z$planning_unit_class()), identical,
      logical(1), x[[1]]$planning_unit_class()
    )) &&
    all(vapply(
      lapply(x, function(z) z$planning_unit_indices()), identical,
      logical(1), x[[1]]$planning_unit_indices()
    ))
  )
}

# assertthat::on_failure(all_comparable_problem) <- function(call, env) {
#   x <- eval(call[[2]], env)  # get the list of problems passed to all_comparable_problem()
#   
#   # replicate the same checks as the main function, individually
#   checks <- list(
#     zone_names = isTRUE(all(vapply(
#       lapply(x, zone_names), identical,
#       logical(1), x[[1]]$zone_names()
#     ))),
#     number_of_zones = isTRUE(all(vapply(
#       lapply(x, number_of_zones), identical,
#       logical(1), x[[1]]$number_of_zones()
#     ))),
#     number_of_pu = isTRUE(all(vapply(
#       lapply(x, number_of_planning_units), identical,
#       logical(1), x[[1]]$number_of_planning_units()
#     ))),
#     number_of_total = isTRUE(all(vapply(
#       lapply(x, number_of_total_units), identical,
#       logical(1), x[[1]]$number_of_total_units()
#     ))),
#     pu_class = isTRUE(all(vapply(
#       lapply(x, function(z) z$planning_unit_class()), identical,
#       logical(1), x[[1]]$planning_unit_class()
#     ))),
#     pu_indices = isTRUE(all(vapply(
#       lapply(x, function(z) z$planning_unit_indices()), identical,
#       logical(1), x[[1]]$planning_unit_indices()
#     )))
#   )
#   
#   # find the first failed check
#   failed <- names(checks)[!unlist(checks)][1]
#   
#   # cli-style messages for each type of failure
#   messages <- list(
#     zone_names = c("{.arg x} has mismatched zone names.",
#                    "i" = "All problems must have identical zone names."),
#     number_of_zones = c("{.arg x} has mismatched number of zones.",
#                         "i" = "All problems must have the same number of zones."),
#     number_of_pu = c("{.arg x} has mismatched number of planning units.",
#                      "i" = "All problems must have the same number of planning units."),
#     number_of_total = c("{.arg x} has mismatched total units.",
#                         "i" = "All problems must have the same total units."),
#     pu_class = c("{.arg x} has mismatched planning unit classes.",
#                  "i" = "All problems must have identical planning unit classes."),
#     pu_indices = c("{.arg x} has mismatched planning unit indices.",
#                    "i" = "All problems must have identical planning unit indices.")
#   )
#   
#   # throw the error using cli_abort
#   cli::cli_abort(messages[[failed]], call = call)
# }

#' Assert passes presolve checks.
#'
#' Check if an optimization problem object (or list thereof) passes
#' presolve checks and throw an error if they fail.
#'
#' @param a [`OptimizationProblem-class`] object or `list` of
#' [`OptimizationProblem-class`] objects.
#'
#' @param show_bypass_message `logical` should the error message contain
#' information on bypassing presolve checks? Defaults to `FALSE`.
#'
#' @param call [environment()] for call. Defaults to `fn_caller_env()`.
#'
#' @return A `logical` value indicating success.
#'
#' @noRd
assert_pass_presolve_check <- function(a, show_bypass_message = FALSE,
                                       call = fn_caller_env()) {
  # assert arguments are valid
  assert(
    inherits(a, c("list", "OptimizationProblem")),
    assertthat::is.flag(show_bypass_message),
    .internal = TRUE
  )
  # run presolve checks
  if (is.list(a)) {
    res <- run_multi_presolve_check(a)
  } else {
    res <- run_presolve_check(a)
  }
  # assert that checks pass
  if (!isTRUE(res$pass)) {
    ## display presolve check results as message
    rlang::inform(
      message = c(
        presolve_check_header(),
        res$msg,
        presolve_check_results(res$pass),
        presolve_check_footer()
      ),
      call = call
    )
    ## prepare error message
    msg <- "{.arg a} failed presolve check."
    if (isTRUE(show_bypass_message)) {
      msg <- c(
        msg,
        c(
          "i" = paste(
            "To ignore checks and attempt optimization anyway,",
            "use {.code solve(force = TRUE)}."
          )
        )
      )
    }
    ## throw error message
    cli::cli_abort(message = msg, call = call)
  }

  # return success
  invisible(TRUE)
}

#' Verify passes presolve checks.
#'
#' Check if an optimization problem object (or list thereof) passes
#' presolve checks and throw a warning if they fail.
#'
#' @inheritParams assert_pass_presolve_checks
#'
#' @inherit assert_pass_presolve_checks return
#'
#' @noRd
verify_pass_presolve_check <- function(a, call = fn_caller_env()) {
  # assert arguments are valid
  assert(
    inherits(a, c("list", "OptimizationProblem")),
    .internal = TRUE
  )
  # run presolve checks
  if (is.list(a)) {
    res <- run_multi_presolve_check(a)
  } else {
    res <- run_presolve_check(a)
  }
  # assert that checks pass
  if (!isTRUE(res$pass)) {
    ## display presolve check results as message
    rlang::inform(
      message = c(
        presolve_check_header(),
        res$msg,
        presolve_check_results(res$pass),
        presolve_check_footer()
      ),
      call = call
    )
    ## throw error message
    cli_warning(
      "{.arg a} failed presolve check.",
      call = call
    )
  }
  # return success
  invisible(TRUE)
}
