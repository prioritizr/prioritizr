#' @include internal.R
NULL

#' List text
#'
#' Create a list of items from a vector.
#'
#' @param x vector.
#'
#' @param last_sep `character` value to separate the last item in the list.
#'   Defaults to `"and"`.
#'
#' @param quote `logical` indicating if the items should be encapsulated in
#'  quotes?
#'
#' @return A `character` value.
#'
#' @noRd
list_text <- function(x, last_sep = "and", quote = FALSE) {
  assert(
    is.atomic(x),
    assertthat::is.string(last_sep),
    assertthat::is.flag(quote)
  )
  if (quote) x <- paste0("\"", x, "\"")
  if (length(x) == 1) return(x)
  if (length(x) == 2) return(paste(x[1], last_sep, x[2]))
  paste0(
    paste(x[-length(x)], collapse = ", "),
    ", ",
    last_sep, " ",
    x[length(x)]
  )
}
