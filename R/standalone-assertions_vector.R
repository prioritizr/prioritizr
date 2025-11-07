# ---
# repo: prioritizr/prioritizr
# file: standalone-assertions_vector.R
# dependencies: standalone-assertions_handlers.R
# imports: [assertthat (>= 0.2.0), cli (>= 3.6.0), rlang (>= 1.1.0)]
# ---

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
    w <- cli::cli_vec(w, list("vec-last" = " or "))
    w <- cli::format_inline("{.val {w}}")
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
    w <- cli::cli_vec(w, list("vec-last" = " or "))
    w <- cli::format_inline("{.val {w}}")
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
  rlang::is_integerish(x)
}

assertthat::on_failure(is_integer) <- function(call, env) {
  paste(
    "{.arg ", deparse(call$x), "} must contain integer values."
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
  is.numeric(x) &&
  all(x >= 1, na.rm = TRUE) &&
  rlang::is_integerish(x)
}

assertthat::on_failure(is_count_vector) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x),
    "} must not contain zeros, negative, or non-integer values."
  )
}
