#' @include internal.R
NULL

#' All columns have any finite values?
#'
#' Check if all columns have at least one finite value.
#'
#' @param x object.
#'
#' @return A `logical` value.
#'
#' @noRd
all_columns_any_finite <- function(x)
  UseMethod("all_columns_any_finite")

assertthat::on_failure(all_columns_any_finite) <- function(call, env) {
  paste(
    deparse(call$x),
      "has some columns only containing missing or non-finite values",
      "(e.g., NaN, NA, Inf)"
  )
}

all_columns_any_finite.default <- function(x) {
  stop("argument to x is not a recognized class")
}

.S3method("all_columns_any_finite", "default", all_columns_any_finite.default)

all_columns_any_finite.data.frame <- function(x) {
  assertthat::assert_that(
    is.data.frame(x)
  )
  all(colSums(vapply(x, is.finite, logical(nrow(x)))) > 0)
}

.S3method(
  "all_columns_any_finite", "data.frame",
  all_columns_any_finite.data.frame
)

all_columns_any_finite.matrix <- function(x) {
  assertthat::assert_that(
    is.matrix(x)
  )
  all(colSums(is.finite(x)) > 0)
}

.S3method("all_columns_any_finite", "matrix",  all_columns_any_finite.matrix)

all_columns_any_finite.Spatial <- function(x) {
  all_columns_any_finite(x@data)
}

.S3method("all_columns_any_finite", "Spatial", all_columns_any_finite.Spatial)

all_columns_any_finite.sf <- function(x) {
  all_columns_any_finite(sf::st_drop_geometry(x))
}

.S3method("all_columns_any_finite", "sf", all_columns_any_finite.sf)
