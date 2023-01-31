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

  # assert that error message refers to a function
  if (!is.null(name)) {
    testthat::expect(
      any(grepl(":", err_msg, fixed = TRUE)),
      trace = act$cap[["trace"]],
      failure_message = "Error message does not refer to a function."
    )
    testthat::expect(
      !any(grepl("Error:", err_msg, fixed = TRUE)),
      trace = act$cap[["trace"]],
      failure_message = "Error message does not refer to a function."
    )
  }
  # assert error contains function name
  if (!is.null(name) && !is.na(name)) {
    testthat::expect(
      any(
          grepl(
          name,
          strsplit(err_msg, ":", fixed = TRUE)[[1]][[1]],
          fixed = TRUE
        )
      ),
      trace = act$cap[["trace"]],
      failure_message = paste0(
        "Error message does not contain function name \"", name, "\"."
      )
    )
  }
  # assert error message doesn't contain words that suggest incorrect
  # error message handling
  testthat::expect(
    (!grepl(".local", err_msg, fixed = TRUE)) &
      (!grepl("eval(assertion, env)", err_msg, fixed = TRUE)) &
      (!grepl("assert_that", err_msg, fixed = TRUE)),
    trace = act$cap[["trace"]],
    failure_message = "Failed to parse error message."
  )
  testthat::expect(
    !grepl("internal", err_msg, fixed = TRUE),
    trace = act$cap[["trace"]],
    failure_message = "Error message refers to internal function."
  )

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
