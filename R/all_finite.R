#' @include internal.R
NULL

#' All finite?
#'
#' Check if an object only has finite values?
#'
#' @param object.
#'
#' @return A `logical` value.
#'
#' @noRd
all_finite <- function(x) UseMethod("all_finite")

assertthat::on_failure(all_finite) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x),
    "} must not have missing or non-finite values ",
    "(e.g., {.val {NaN}}, {.val {NA}}, {.val {Inf}})."
  )
}

#' @export
all_finite.default <- function(x) {
  cli::cli_abort("{.arg x} is not a recognized class.")
}

#' @export
all_finite.numeric <- function(x) {
  all(is.finite(x))
}

#' @export
all_finite.logical <- function(x) {
  all(is.finite(x))
}

#' @export
all_finite.character <- function(x) {
  all(!is.na(x))
}

#' @export
all_finite.factor <- function(x) {
  all(is.finite(x))
}

#' @export
all_finite.matrix <- function(x) {
  all(is.finite(c(x)))
}

#' @export
all_finite.array <- function(x) {
  all(is.finite(c(x)))
}

#' @export
all_finite.Matrix <- function(x) {
  all(is.finite(x@x))
}

#' @export
all_finite.data.frame <- function(x) {
  all(vapply(x, all_finite, logical(1)))
}

#' @export
all_finite.Spatial <- function(x) {
  all(vapply(x@data, all_finite, logical(1)))
}

#' @export
all_finite.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), all_finite, logical(1)))
}
