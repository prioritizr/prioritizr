#' @include internal.R all_binary.R all_columns_any_finite.R all_columns_any_finite.R all_columns_inherit.R all_finite.R all_positive.R all_proportion.R all_rows_any_finite.R any_nonNA.R any_nonzero.R assertions_vector.R assertions_raster.R assertions_misc.R assertions_sf.R assertions_class.R
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
  if (isTRUE(res)) return(TRUE)
  # if not TRUE, then res should be a character vector with the error message,
  # so now we will format it following tidyverse style guide
  res <- format_assertthat_msg(res)
  # replace "must" with descriptions,
  # because verify() is used to indicate valid -- but likely mistaken -- inputs
  res <- gsub("must not have", "has", res, fixed = TRUE)
  res <- gsub("must have", "does not have", res, fixed = TRUE)
  # throw warning
  cli_warning(res, .envir = call)
  # return result
  FALSE
}

#' Assert that a condition is met
#'
#' Assert if an assertion is met and throw a [base::warning()] if it
#' is not. This function is equivalent to [assertthat::assert_that()]
#' except that it uses [rlang::abort()].
#'
#' @param x `logical` condition.
#'
#' @details
#' The function will throw warnings if any of the conditions are not met.
#'
#' @return A `logical` value.
#'
#' @noRd
assert <- function(..., env = parent.frame(), call = fn_caller_env()) {
  # check if assertions met
  res <- assertthat::validate_that(..., env = env)
  # if res is TRUE, then return success
  if (isTRUE(res)) return(TRUE)
  # if not TRUE, then res should be a character vector with the error message,
  # so now we will format it following tidyverse style guide
  res <- format_assertthat_msg(res)
  # throw error
  cli::cli_abort(res, call = call)
  FALSE
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