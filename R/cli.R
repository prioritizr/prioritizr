#' @include internal.R
NULL

#' Package theme
#'
#' Theme for the package.
#'
#" @details This function is designed to be used with [cli::cli_div].
#'
#' @return `list` object.
#'
#' @noRd
cli_pkg_theme <- function() {
  list(
    .gray = list(color = "gray"),
    .val = list(digits = 4),
    # .h = list("font-weight" = "bold")
    .h = list(color = "cyan")
  )
}

#' Box characters
#'
#' Determine characters for creating boxes.
#'
#' @details This function is designed to be used with [cli::cli_text].
#'
#' @return `list` object.
#'
#' @noRd
cli_box_chars <- function() {
  if (cli::is_utf8_output()) {
    list(
      "h" = cli::col_cyan("\u2500"),                   # horizontal
      "v" = cli::col_cyan("\u2502"),                   # vertical
      "l" = cli::col_cyan("\u2514"),                   # leaf
      "j" = cli::col_cyan("\u251C"),                   # junction
      "b" = cli::col_cyan("\u2022")                    # bullet
    )
  } else {
    list(
      "h" = cli::col_cyan("-"),                        # horizontal
      "v" = cli::col_cyan("|"),                        # vertical
      "l" = cli::col_cyan("\\"),                       # leaf
      "j" = cli::col_cyan("+"),                        # junction
      "b" = cli::col_cyan("@")                         # bullet
    )
  }
}

#' Verbatim text
#'
#' Print text verbatim.
#'
#' @param ... `character` values to print.
#'
#' @param .envir `environment` for evaluating expressions.
#'
#' @details
#' This function is a wrapper for [cli::cli_verbatim] that supports
#' \pkg{glue} expressions.
#'
#' @return None.
#'
#' @noRd
cli_vtext <- function(..., .envir = parent.frame()) {
  cli::cli_verbatim(
    cli::format_inline(..., keep_whitespace = TRUE, .envir = .envir)
  )
}

#' Tree component
#'
#' Print text that is designed to appear in a tree.
#'
#' @param x `character` vector with text to print.
#'
#' @param header `character` value displayed before first element in `x`.
#'
#' @param subheader `character` value displayed before subsequent elements in
#'   `x`.
#'
#' @param width `integer` extra spaces to insert between `subheader` and `x`.
#'
#' @param .envir `environment` for evaluating expressions.
#'
#' @return None.
#'
#' @noRd
cli_tree_component <- function(x,
                               header = "",
                               subheader = "",
                               padding = "",
                               width = 0,
                               .envir = parent.frame()) {
  ch <- cli_box_chars()
  out <- paste0(
    padding, header, x[1]
  )
  if (length(x) > 1) {
    if (length(x) > 2) {
      out <- c(
        out,
        paste0(
          padding, subheader, ch$j, ch$b,
          paste(rep(" ", width), collapse = ""),
          x[c(-1, -length(x))]
        )
      )
    }
    out <- c(
      out,
      paste0(
        padding, subheader, ch$l, ch$b,
        paste(rep(" ", width), collapse = ""),
        x[length(x)]
      )
    )
  }
  for (x in out) cli_vtext(x, .envir = .envir)
  invisible(TRUE)
}

#' Warning
#'
#' Throw a warning.
#'
#' @param message `character` value with warning to display.
#'
#' @param immediate `logical` value indicating if warning should be
#'   displayed immediately. Defaults to `TRUE`.
#'
#' @param call `logical` value indicating if the warning should contain
#'   the function call. Defaults to `FALSE`.
#'
#' @return None.
#'
#' @examples
#' cli_warning("{.strong Danger! Danger!} My hooks are flailing wildly!")
#'
#' @noRd
cli_warning <- function(message, immediate = TRUE, call = FALSE,
                        .envir = parent.frame()) {
  warning(
    cli::format_warning(message, .envir = .envir),
    immediate. = immediate, call. = call
  )
}

#' Defunct
#'
#' Throw an error indicating that a function is defunct.
#'
#' @param old `character` value with name of defunct function.
#'
#' @param new `character` value with name of replacement function.
#'   Defaults to `NULL` such that no replacement function is detailed.
#'
#' @return None.
#'
#' @examples
#' cli_defunct(old = "greg", new = "gred")
#'
#' @noRd
cli_defunct <- function(old, new = NULL) {
  msg <- c("{.fn {old}} is defunct.")
  if (!is.null(new)) {
    msg <- c(msg, "v" = "Use {.fn {new}} instead.")
  }
  cli::cli_abort(msg, call = NULL)
}

#' Deprecated
#'
#' Throw an error indicating that a function is deprecated.
#'
#' @param old `character` value with name of deprecated function.
#'
#' @param new `character` value with name of replacement function.
#'   Defaults to `NULL` such that no replacement function is detailed.
#'
#' @return None.
#'
#' @examples
#' cli_deprecated(old = "greg", new = "gred")
#'
#' @noRd
cli_deprecated <- function(old, new = NULL) {
  msg <- c("{.fn {old}} is deprecated.")
  if (!is.null(new)) {
    msg <- c(msg, "i" = "Use {.fn {new}} instead.")
  }
  cli_warning(msg, call = FALSE)
}
