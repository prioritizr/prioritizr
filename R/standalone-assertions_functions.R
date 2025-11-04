# ---
# repo: prioritizr/prioritizr
# file: standalone-assertions_functions.R
# imports: [assertthat (>= 0.2.0), cli (>= 3.6.0), rlang (>= 1.1.0)]
# --

#' Assert dots are empty
#'
#' Assert that `...` are empty.
#'
#' @param env passed to [rlang::check_dots_empty()].
#'
#' @param error passed to [rlang::check_dots_empty()].
#'
#' @param call [environment()] for call. Defaults to `fn_caller_env()`.
#'
#' @param action passed to [rlang::check_dots_empty()].
#'
#' @details
#' This function is essentially a wrapper for [rlang::check_dots_empty()].
#'
#' @noRd
assert_dots_empty <- function(env = rlang::caller_env(),
                              error = NULL,
                              call = fn_caller_env(),
                              action = rlang::abort) {
  rlang::check_dots_empty(env = env, call = call)
}

#' Assert required
#'
#' Assert that an argument is required.
#'
#' @param x passed to [rlang::check_required()].
#'
#' @param arg passed to [rlang::check_required()].
#'
#' @param call [environment()] for call. Defaults to `fn_caller_env()`.
#'
#' @param .internal `logical` value indicating if internal error.
#' Defaults to `FALSE`.
#'
#' @details
#' This function is essentially a wrapper for [rlang::check_required()].
#'
#' @noRd
assert_required <- function(x,
                            arg = rlang::caller_arg(x),
                            call = fn_caller_env(),
                            .internal = FALSE) {
  # check that argument supplied
  rlang::check_required(x = x, arg = arg, call = call)
  # check that argument yields valid expression
  res <- try(identical(x, 1), silent = TRUE)
  # if it doesn't, then throw an error message
  if (inherits(res, "try-error")) {
    ## if the error message is a simpleError,
    ## then this means that assert_required() is being called in pipe-chain
    ## where the error is happening
    if (inherits(attr(res, "condition"), "simpleError")) {
      cond_call <- deparse(attr(res, "condition")$call)[[1]]
      cond_msg <- trimws(attr(res, "condition")$message)[[1]]
      err_msg <- c(
        "i" = "In argument to {.arg {arg}}.",
        ifelse(
          identical(cond_call, "NULL") || (
            startsWith(cond_msg, "object ") && endsWith(cond_msg, "not found")
          ),
          "{.strong Caused by error:}",
          paste0("{.strong Caused by {.code ", cond_call, "}:}")
        ),
        "!" = cond_msg
      )
      cli::cli_abort(
        message = err_msg,
        parent = attr(res, "condition")$parent,
        call = call,
        .internal = .internal
      )
    } else {
      ## if not,
      ## then this means that assert_required() is being called in later
      ## in the pipe chain after the error has happened, so we want to
      ## throw the error embedded in the try()
      cli::cli_abort(
        message = c(
          attr(res, "condition")$message,
          attr(res, "condition")$body
        ),
        parent = attr(res, "condition")$parent,
        trace = attr(res, "condition")$trace,
        .internal = .internal,
        call = attr(res, "condition")$call
      )
    }
  }
  invisible(TRUE)
}

#' Assert that an object is a valid first argument for a method function.
#'
#' Throw an error for a function to indicate that it should not be
#' used with [problem()] directly.
#'
#' @param x Object.
#'
#' @param call Caller environment.
#'
#' @return An invisible `TRUE` indicating success.
#'
#' @noRd
assert_valid_method_arg <- function(x, call = fn_caller_env()) {
  # assemble error message
  m <- c(
    "!" = "This function can't add targets to a {.fn problem}.",
    "v" = "Use it with {.fn add_auto_targets} or {.fn add_group_targets}."
  )
  # run assertion
  assert(!is_conservation_problem(x), msg = m, call = call)
  # return success
  invisible(TRUE)
}
