#' @include internal.R all_binary.R all_columns_any_finite.R all_columns_any_finite.R all_columns_inherit.R all_finite.R all_positive.R all_proportion.R any_nonNA.R any_nonzero.R assertions_vector.R assertions_raster.R assertions_misc.R assertions_sf.R assertions_class.R
NULL

fn_caller_env <- function(n = 1) {
  call <- parent.frame(n + 1)
  nm <- as.character(rlang::frame_call(call))
  if (
    (length(nm) > 0) &&
    identical(nm[[1]], ".local")
  ) {
    call <- parent.frame(n = n + 2)
  }
  call
}

fn_current_env <- function() {
  call <- parent.frame()
  nm <- as.character(rlang::frame_call(call))
  if (
    (length(nm) > 0) &&
    identical(nm[[1]], ".local")
  ) {
    call <- parent.frame(n = 2)
  }
  call
}

#' Verify if a condition is met
#'
#' Verify if an condition is met and throw a [base::warning()] if it
#' is not. This function is equivalent to [assertthat::assert_that()]
#' except that it uses [rlang::warn()].
#'
#' @param x `logical` condition.
#'
#' @param env `environment` passed to [assertthat::validate_that].
#'   Defaults to [parent.frame()].
#'
#' @param call `environment` for call. Defaults to `fn_caller_env()`.
#'
#' @details
#' The function will throw warnings if any of the conditions are not met.
#'
#' @return A `logical` value.
#'
#' @noRd
verify <- function(..., env = parent.frame(), call = fn_caller_env()) {
  # check if assertions met
  res <- assertthat::validate_that(..., env = env)
  # if res is TRUE, then return success
  if (isTRUE(res)) return(invisible(TRUE))
  # if not TRUE, then res should be a character vector with the error message,
  # so now we will format it following tidyverse style guide
  res <- format_assertthat_msg(res)
  # replace "must" with descriptions,
  # because verify() is used to indicate valid -- but likely mistaken -- inputs
  res <- gsub("must not have", "has", res, fixed = TRUE)
  res <- gsub("must have", "does not have", res, fixed = TRUE)
  # if first warning message does not have a symbol,
  # then give it one by default
  if (is.null(names(res)) || !nzchar(names(res)[[1]])) {
    names(res)[[1]] <- ">"
  }
  # throw warning
  cli_warning(res, .envir = call, call = call)
  # return result
  invisible(FALSE)
}

#' Assert that a condition is met
#'
#' Assert if an assertion is met and throw a [base::warning()] if it
#' is not. This function is equivalent to [assertthat::assert_that()]
#' except that it uses [rlang::abort()].
#'
#' @param x `logical` condition.
#'
#' @param env `environment` passed to [assertthat::validate_that].
#'   Defaults to [parent.frame()].
#'
#' @param call `environment` for call. Defaults to `fn_caller_env()`.
#'
#' @param .internal `logical` passed to [cli::cli_abort].
#'
#' @details
#' The function will throw warnings if any of the conditions are not met.
#'
#' @return A `logical` value.
#'
#' @noRd
assert <- function(..., env = parent.frame(), call = fn_caller_env(),
                   .internal = FALSE) {
  # check if assertions met
  res <- assertthat::validate_that(..., env = env)
  # if res is TRUE, then return success
  if (isTRUE(res)) return(TRUE)
  # if not TRUE, then res should be a character vector with the error message,
  # so now we will format it following tidyverse style guide
  res <- format_assertthat_msg(res)
  # throw error
  cli::cli_abort(res, call = call, .envir = env, .internal = .internal)
}

format_assertthat_msg <- function(x) {
  # add a full stop to end of message if needed
  idx <-
    !endsWith(x, ".") &
    (nchar(x) > 0) &
    !endsWith(x, "\n") &
    !endsWith(x, "\f") &
    !endsWith(x, "?") &
    !endsWith(x, "!") &
    !endsWith(x, ":")
  if (any(idx)) {
    x[idx] <- paste0(x[idx], ".")
  }
  # format the arguments for cli
  if (
    !isTRUE(grepl("{", x, fixed = TRUE)) &&
    (
      isTRUE(grepl(" is not a ", x, TRUE)) ||
      isTRUE(grepl(" does not have ", x, TRUE))
    )
  ) {
    x <- paste0("{.arg ", sub(" ", "} ", x, fixed = TRUE))
  }
  # return result
  x
}

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
      cli::cli_abort(message = err_msg, .internal = .internal, call = call)
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
        trace = attr(res, "condition")$trace,
        .internal = .internal,
        call = attr(res, "condition")$call
      )
    }
  }
  invisible(TRUE)
}

#' Error prefix handler
#'
#' Wrap an error message.
#'
#' @param expr Expression.
#'
#' @param prefix `character` vector with error prefix.
#'
#' @inheritParams assert
#'
#' @return An invisible `logical` value.
#'
#' @noRd
error_prefix_handler <- function(expr, prefix, call = fn_caller_env()) {
  # try to evaluate expression
  x <- rlang::try_fetch(expr, error = function(cnd) cnd)
  # if success, then return result
  if (!inherits(x, "error")) return(x)
  # otherwise, extract the error message
  x <- x$message
  # add in bullet point formatting to error message
  if (is.null(names(x))) {
    names(x) <- rep("x", length(x))
  }
  # nocov start
  if (anyNA(names(x))) {
    names(x)[is.na(x)] <- "x"
  }
  # nocov end
  # throw error message
  cli::cli_abort(c(prefix, x), call = call)
}
