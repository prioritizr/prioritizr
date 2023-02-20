#' @include internal.R
NULL

#' Is numeric values?
#'
#' Check if an object has numeric values?
#'
#' @param object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_numeric_values <- function(x) UseMethod("is_numeric_values")

assertthat::on_failure(is_numeric_values)  <- function(call, env) {
  paste(deparse(call$x), "has non-numeric values")
}

is_numeric_values.default <- function(x) {
  FALSE
}

.S3method("is_numeric_values", "default", is_numeric_values.default)

is_numeric_values.numeric <- function(x) {
  is.numeric(x)
}

.S3method("is_numeric_values", "numeric", is_numeric_values.numeric)

is_numeric_values.Matrix <- function(x) {
  is.numeric(x@x)
}

.S3method("is_numeric_values", "Matrix", is_numeric_values.Matrix)

is_numeric_values.matrix <- function(x) {
  is.numeric(c(x))
}

.S3method("is_numeric_values", "matrix", is_numeric_values.matrix)

is_numeric_values.data.frame <- function(x) {
  all(vapply(x, is_numeric_values, logical(1)))
}

.S3method("is_numeric_values", "data.frame", is_numeric_values.data.frame)

is_numeric_values.Spatial <- function(x) {
  all(vapply(x@data, is_numeric_values, logical(1)))
}

.S3method("is_numeric_values", "Spatial", is_numeric_values.Spatial)

is_numeric_values.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), is_numeric_values, logical(1)))
}

.S3method("is_numeric_values", "sf", is_numeric_values.sf)
