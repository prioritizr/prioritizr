# ---
# repo: prioritizr/prioritizr
# file: standalone-all_binary.R
# imports: [assertthat (>= 0.2.0), cli (>= 3.6.0), terra (>= 1.8-54), sf (>= 1.0-12)]
# ---

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

#' @export
all_binary.default <- function(x) {
  cli::cli_abort("{.arg x} is not a recognized class.")
}

#' @export
all_binary.numeric <- function(x) {
  all(x[is.finite(x)] %in% c(0, 1))
}

#' @export
all_binary.Matrix <- function(x) {
  all_binary(x@x)
}

#' @export
all_binary.matrix <- function(x) {
  all_binary(c(x))
}

#' @export
all_binary.data.frame <- function(x) {
  all(vapply(x, all_binary, logical(1)))
}

#' @export
all_binary.Spatial <- function(x) {
  all(vapply(x@data, all_binary, logical(1)))
}

#' @export
all_binary.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), all_binary, logical(1)))
}

#' @export
all_binary.SpatRaster <- function(x) {
  all(terra::values(x, mat = FALSE, na.rm = TRUE) %in% c(0, 1))
}

#' @export
all_binary.Raster <- function(x) {
  all_binary.SpatRaster(terra::rast(x))
}
