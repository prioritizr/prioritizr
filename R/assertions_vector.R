#' @include internal.R
NULL

#' Is a match of?
#'
#' Check if a value matches an element in a `vector.
#'
#' @param x value.
#'
#' @param choices vector.
#'
#' @return A `logical` value.
#'
#' @noRd
is_match_of <- function(x, choices) {
  assertthat::assert_that(
    assertthat::is.scalar(x),
    is.vector(choices)
  )
  isTRUE(x %in% choices)
}

assertthat::on_failure(is_match_of) <- function(call, env) {
  w <- eval(call$choices, envir = env)
  if (length(w) < 5) {
    w <- list_text(w, last_sep = "or")
  } else {
    w <- deparse(call$choices)
  }
  paste(deparse(call$x), "is not one of", w)
}

#' All a match of?
#'
#' Check if all elements in a vector match elements in a vector.
#'
#' @param x value.
#'
#' @param choices vector.
#'
#' @return A `logical` value.
#'
#' @noRd
all_match_of <- function(x, choices) {
  assertthat::assert_that(
    is.vector(x) || is.factor(x),
    is.vector(choices)
  )
  all(x %in% choices)
}

assertthat::on_failure(all_match_of) <- function(call, env) {
  w <- eval(call$choices, envir = env)
  if (length(w) < 5) {
    w <- list_text(w, last_sep = "or")
  } else {
    w <- deparse(call$choices)
  }
  paste(
    deparse(call$x),
    "contains a value is not one of",
    w
  )
}

#' No duplicates
#'
#' Check if all elements in a vector are unique.
#'
#' @param x vector object.
#'
#' @return A `logical` value.
#'
#' @noRd
no_duplicates <- function(x) {
  assertthat::assert_that(is.vector(x))
  anyDuplicated(x) == 0L
}

assertthat::on_failure(no_duplicates) <- function(call, env) {
  paste(deparse(call$x), "contains duplicate values")
}

#' Is count vector?
#'
#' Check if a vector contains count values.
#'
#' @param x object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_count_vector <- function(x) {
  assertthat::assert_that(is.numeric(x))
  all(x >= 1, na.rm = TRUE) && all(x == round(x), na.rm = TRUE)
}

assertthat::on_failure(is_count_vector) <- function(call, env) {
  paste(
    deparse(call$x),
    "contains zeros, negative, or non-integer values"
  )
}

#' Is integer?
#'
#' Check if a vector contains integer values.
#'
#' @param x object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_integer <- function(x) {
  assertthat::assert_that(is.numeric(x))
  all(x == round(x), na.rm = TRUE)
}

assertthat::on_failure(is_count_vector) <- function(call, env) {
  paste(
    deparse(call$x),
    "contains non-integer values"
  )
}
