#' Expect tidy error
#'
#' This function is a wrapper for [testthat::expect_error] that contains
#' additional checks to ensure that the output follows error guidelines
#' for the tidyverse.
#'
#' @param object Object.
#'
#' @param regexp `character` value. See [testthat::expect_error] documentation.
#'
#' @param name `character` name of the function that should appear in the
#'  error message. If `NA`, then the error message should display a
#'  function name, and any function name is acceptable.
#'  If `NULL`, then the function name is not checked.
#'  Defaults to `NA`.
#'
#' @return The `object` value.
#'
#' @noRd
expect_tidy_error <- function(object, regexp = NULL, name = NA) {
  # test for regexp
  testthat::expect_error(object, regexp = regexp)

  # capture error
  act <- quasi_capture(
    rlang::enquo(object),
    NULL,
    testthat::capture_error,
    entrace = TRUE
  )
  err_msg <- as.character(act$cap)
  err_msg <- cli::ansi_strip(err_msg)

  # assert that error message contains a ! character
  if (!any(grepl("!", err_msg, fixed = TRUE))) {
    testthat::expect(
      FALSE,
      trace = act$cap[["trace"]],
      failure_message = "Error message not generated with `cli::cli_abort()`."
    )
    return(invisible(act$val %||% act$cap))
  }

  # assert that error message refers to a function
  if (!is.null(name)) {
    if (
      any(grepl(":", err_msg, fixed = TRUE)) &&
      any(grepl("Error:", err_msg, fixed = TRUE))
    )
    testthat::expect(
      FALSE,
      trace = act$cap[["trace"]],
      failure_message = "Error message does not refer to a function."
    )
    return(invisible(act$val %||% act$cap))
  }

  # assert error contains function name
  if (!is.null(name) && !is.na(name)) {
    if (
      !any(
        grepl(
          name,
          strsplit(err_msg, ":", fixed = TRUE)[[1]][[1]],
          fixed = TRUE
        )
      )
    ) {
      testthat::expect(
        FALSE,
        trace = act$cap[["trace"]],
        failure_message = paste0(
          "Error message does not contain function name \"", name, "\"."
        )
      )
      return(invisible(act$val %||% act$cap))
    }
  }

  # assert error message doesn't contain words that suggest incorrect
  # error message handling
  if (
    grepl(".local", err_msg, fixed = TRUE) ||
      grepl("eval(assertion, env)", err_msg, fixed = TRUE) ||
      grepl("assert_that", err_msg, fixed = TRUE) ||
      grepl("Could not evaluate", err_msg, fixed = TRUE) ||
      grepl("Error in glue", err_msg, fixed = TRUE)
  ) {
    testthat::expect(
      FALSE,
      trace = act$cap[["trace"]],
      failure_message = "Failed to parse error message."
    )
    return(invisible(act$val %||% act$cap))
  }
  if (grepl("internal", err_msg, fixed = TRUE)) {
    testthat::expect(
      FALSE,
      trace = act$cap[["trace"]],
      failure_message = "Error message refers to internal function."
    )
  }

  # return result
  invisible(act$val %||% act$cap)
}

# internal testthat function
quasi_capture <- function (.quo, .label, .capture, ...) {
  act <- list()
  act$lab <- .label %||% rlang::quo_label(.quo)
  act$cap <- .capture(
    act$val <- rlang::eval_bare(
      rlang::quo_get_expr(.quo), rlang::quo_get_env(.quo)
    ),
    ...
  )
  act
}

`%||%` <- rlang::`%||%`

#' Expect inherits
#'
#' This function replicates [testthat::expect_is()] for use in the 3rd
#' edition of the \pkg{testthat}.
#'
#' @param object Object.
#'
#' @param what `character` value.
#'
#' @return The `object` value.
#'
#' @noRd
expect_inherits <- function(object, what) {
  # capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  # run expectation
  act$what <- class(act$val)
  testthat::expect(
    inherits(act$val, what),
    failure_message = sprintf(
      "%s is a '%s', not a '%s' object.",
      act$lab, act$what, what
    )
  )
  invisible(act$val %||% act$cap)
}
