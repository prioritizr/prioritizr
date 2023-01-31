#' @include internal.R
NULL

#' All rows have any finite values?
#'
#' Check if all rows have at least one finite value.
#'
#' @param x object.
#'
#' @param what `character` name of class.
#'
#' @return A `logical` value.
#'
#' @noRd
all_rows_any_finite_failure <- function(x)
  UseMethod("all_rows_any_finite_failure")

assertthat::on_failure(all_rows_any_finite_failure) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x),
    "} must not have rows that only contain missing or non-finite values",
    "(e.g., {.val {NaN}}, {.val {NA}}, {.val {Inf}})."
  )
}

all_rows_any_finite_failure.default <- function(x, what) {
  stop("{.arg x} is not a recognized class")
}

.S3method(
  "all_rows_any_finite_failure", "default",
  all_rows_any_finite_failure.default
)

all_rows_any_finite_failure.data.frame <- function(x, what) {
  assert(
    is.data.frame(x),
    is.character(what)
  )
  all(rowSums(!vapply(x, is.finite, logical(nrow(x)))) > 0)
}

.S3method(
  "all_rows_any_finite_failure", "data.frame",
  all_rows_any_finite_failure.data.frame
)

all_rows_any_finite_failure.matrix <- function(x, what) {
  assert(
    is.matrix(x),
    is.character(what)
  )
  all(rowSums(!is.finite(x)) > 0)
}

.S3method(
  "all_rows_any_finite_failure", "matrix",
  all_rows_any_finite_failure.matrix
)

all_rows_any_finite_failure.Spatial <- function(x, what) {
  all_rows_any_finite_failure(x@data)
}

.S3method(
  "all_rows_any_finite_failure", "Spatial",
  all_rows_any_finite_failure.Spatial
)

all_rows_any_finite_failure.sf <- function(x, what) {
  all_rows_any_finite_failure(sf::st_drop_geometry(x))
}

.S3method(
  "all_rows_any_finite_failure", "sf",
  all_rows_any_finite_failure.sf
)
