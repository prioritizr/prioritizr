#' @include internal.R
NULL

#' Print an object for knitr package.
#'
#' This function is used to ensure that [problem()] and
#' [new_optimization_problem()] objects are displayed correctly in
#' \pkg{rmarkdown} reports.
#'
#' @param x Object.
#'
#' @param ... Not used.
#'
#' @details
#' This function should not be called directly. It is intended to be used
#' by the \pkg{knitr} package when displaying objects.
#'
#' @return A `character` vector for knitting.
#'
#' @name knit_print
NULL

#' @rdname knit_print
#' @export
knit_print.ConservationProblem <- function(x, ...) {
  internal_knit_print(x)
}

#' @rdname knit_print
#' @export
knit_print.OptimizationProblem <- function(x, ...) {
  internal_knit_print(x)
}

internal_knit_print <- function(x) {
  # assert knitr package installed
  assert(is_installed("knitr"))
  # use a knitr subprocess to generate the text to display
  res <- knitr::knit_child(
    system.file("knitr", "print.Rmd", package = "prioritizr"),
    envir = list2env(
      list(name = rlang::as_name(rlang::enquo(x)), x = x),
      parent = parent.frame()),
    quiet = TRUE,
  )
  # format the output
  structure(
    paste(res, collapse = "\n"),
    class = "knit_asis"
  )
}
