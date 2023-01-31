#' @include internal.R
NULL

#' All binary?
#'
#' Check if an object has only binary values?
#'
#' @param object.
#'
#' @return A `logical` value.
#'
#' @noRd
all_binary <- function(x) UseMethod("all_binary")

assertthat::on_failure(all_binary) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x), "} ",
    "must have binary ({.val {0}} or {.val {1}}) values."
  )
}

all_binary.default <- function(x) {
  stop("{.arg x} is not a recognized class.")
}

.S3method("all_binary", "default", all_binary.default)

all_binary.numeric <- function(x) {
  all(x[!is.na(x)] %in% c(0, 1))
}

.S3method("all_binary", "numeric", all_binary.numeric)

all_binary.Matrix <- function(x) {
  all_binary(x@x)
}

.S3method("all_binary", "Matrix", all_binary.Matrix)

all_binary.matrix <- function(x) {
  all_binary(c(x))
}

.S3method("all_binary", "matrix", all_binary.matrix)

all_binary.data.frame <- function(x) {
  all(vapply(x, all_binary, logical(1)))
}

.S3method("all_binary", "data.frame", all_binary.data.frame)

all_binary.Spatial <- function(x) {
  all(vapply(x@data, all_binary, logical(1)))
}

.S3method("all_binary", "Spatial", all_binary.Spatial)

all_binary.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), all_binary, logical(1)))
}

.S3method("all_binary", "sf", all_binary.sf)

all_binary.SpatRaster <- function(x) {
  all(c(terra::values(x)) %in% c(0, 1, NA))
}

.S3method("all_binary", "SpatRaster", all_binary.SpatRaster)
