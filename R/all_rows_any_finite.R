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
  paste(
    deparse(call$x),
      "has some rows only contain missing or non-finite values",
      "(e.g., NaN, NA, Inf)"
  )
}

all_rows_any_finite_failure.default <- function(x, what) {
  stop("argument to x is not a recognized class")
}

.S3method(
  "all_rows_any_finite_failure", "default",
  all_rows_any_finite_failure.default
)

all_rows_any_finite_failure.data.frame <- function(x, what) {
  assertthat::assert_that(
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
  assertthat::assert_that(
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
