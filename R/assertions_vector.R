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
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
is_match_of <- function(x, choices) {
  assert(
    assertthat::is.scalar(x),
    is.atomic(choices),
    .internal = TRUE
  )
  isTRUE(x %in% choices)
}

assertthat::on_failure(is_match_of) <- function(call, env) {
  w <- eval(call$choices, envir = env)
  if (length(w) <= 5) {
    if (is.numeric(w)) {
      w <- paste0("{", w, "}")
    }
    w <- list_text(paste0("{.val ", w, "}"), last_sep = "or")
  } else {
    w <- paste0("one of the values in {.code ", deparse(call$choices), "}")
  }
  paste0("{.arg ", deparse(call$x), "} must be ", w, ".")
}

#' All a match of?
#'
#' Check if all elements in a vector match elements in a vector.
#'
#' @param x value.
#'
#' @param choices vector.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
all_match_of <- function(x, choices) {
  assert(
    is.atomic(x) || is.factor(x),
    is.atomic(choices),
    .internal = TRUE
  )
  all(x %in% choices)
}

assertthat::on_failure(all_match_of) <- function(call, env) {
  w <- eval(call$choices, envir = env)
  if (length(w) <= 5) {
    if (is.numeric(w)) {
      w <- paste0("{", w, "}")
    }
    w <- list_text(paste0("{.val ", w, "}"), last_sep = "or")
  } else {
    w <- paste0("one of the values in {.code ", deparse(call$choices), "}")
  }
  paste0(
    "All values in {.arg ", deparse(call$x), "} must be ", w, "."
  )
}

#' No duplicates
#'
#' Check if all elements in a vector are unique.
#'
#' @param x vector object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
no_duplicates <- function(x) {
  assert(is_inherits(x, c("numeric", "character", "factor")), .internal = TRUE)
  anyDuplicated(x) == 0L
}

assertthat::on_failure(no_duplicates) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x), "} must not contain duplicate values."
  )
}

#' Is count vector?
#'
#' Check if a vector contains count values.
#'
#' @param x object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
is_count_vector <- function(x) {
  assert(is.numeric(x), .internal = TRUE)
  all(x >= 1, na.rm = TRUE) && all(x == round(x), na.rm = TRUE)
}

assertthat::on_failure(is_count_vector) <- function(call, env) {
  paste(
    "{.arg ", deparse(call$x),
    "} must not contain zeros, negative, or non-integer values."
  )
}
