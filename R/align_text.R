#' @include internal.R
NULL

#' Align text
#'
#' Format text by adding a certain number of spaces after new line characters.
#'
#' @param x `character` value.
#'
#' @param n `integer` number of spaces.
#'
#' @return A `character` value.
#'
#' @examples
#' # make some text
#' original_text <- "animals: horse\npig\nbear"
#'
#' # print text
#' message(original_text)
#'
#' # this look really ugly so we will align it
#' aligned_text <- align_text(original_text, 9)
#'
#' # print aligned text
#' message(aligned_text)
#'
#' @noRd
align_text <- function(x, n) {
  assert(
    assertthat::is.string(x),
    assertthat::is.count(n)
  )
  if (!grepl("\n", x)) return(x)
  gsub("\n", paste0("\n", paste(rep(" ", n), collapse = "")), x, fixed = TRUE)
}
