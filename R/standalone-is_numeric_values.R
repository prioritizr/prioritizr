# ---
# repo: prioritizr/prioritizr
# file: standalone-is_numeric_values.R
# imports: [assertthat (>= 0.2.0), terra (>= 1.8-54), sf (>= 1.0-12)]
# ---

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

#' @export
is_numeric_values.default <- function(x) {
  FALSE
}

#' @export
is_numeric_values.numeric <- function(x) {
  is.numeric(x)
}

#' @export
is_numeric_values.Matrix <- function(x) {
  is.numeric(x@x)
}

#' @export
is_numeric_values.matrix <- function(x) {
  is.numeric(c(x))
}

#' @export
is_numeric_values.data.frame <- function(x) {
  all(vapply(x, is_numeric_values, logical(1)))
}

#' @export
is_numeric_values.Spatial <- function(x) {
  all(vapply(x@data, is_numeric_values, logical(1)))
}

#' @export
is_numeric_values.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), is_numeric_values, logical(1)))
}

#' @export
is_numeric_values.SpatRaster <- function(x) {
  all(!terra::is.factor(x))
}

#' @export
is_numeric_values.Raster <- function(x) {
  TRUE
}
