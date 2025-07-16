#' @include internal.R
NULL

#' Any non-NA?
#'
#' Check if an object has any non-missing (`NA`) values?
#'
#' @param object.
#'
#' @param names `character` vector of column names.
#'
#' @return A `logical` value.
#'
#' @noRd
any_nonNA <- function(x) UseMethod("any_nonNA")

assertthat::on_failure(any_nonNA) <- function(call, env) {
  x <- eval(call$x, envir = env)
  if (inherits(x, c("SpatRaster", "Raster"))) {
    msg <- paste0(
      "{.arg ",
      deparse(call$x),
      "} must not have a layer with only missing ({.val {NA}}) values."
    )
  } else if (inherits(x, c("data.frame", "sf", "Spatial"))) {
    msg <- paste0(
      "{.arg ",
      deparse(call$x),
      "} must not have a column with only missing ({.val {NA}}) values."
    )
  } else {
    msg <- paste0(
      "{.arg ",
      deparse(call$x),
      "} must not have only missing ({.val {NA}}) values."
    )
  }
  msg
}

#' @export
any_nonNA.default <- function(x) {
  cli::cli_abort("{.arg x} is not a recognized class.")
}

#' @export
any_nonNA.numeric <- function(x) {
  any(!is.na(x))
}

#' @export
any_nonNA.logical <- function(x) {
  any(!is.na(x))
}

#' @export
any_nonNA.character <- function(x) {
  any(!is.na(x))
}

#' @export
any_nonNA.factor <- function(x) {
  any(!is.na(x))
}

#' @export
any_nonNA.Matrix <- function(x) {
  any(!is.na(x@x))
}

#' @export
any_nonNA.matrix <- function(x) {
  any(!is.na(c(x)))
}

#' @export
any_nonNA.data.frame <- function(x) {
  all(vapply(x, any_nonNA, logical(1)))
}

#' @export
any_nonNA.Spatial <- function(x) {
  all(vapply(x@data, any_nonNA, logical(1)))
}

#' @export
any_nonNA.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), any_nonNA, logical(1)))
}

#' @export
any_nonNA.Raster <- function(x) {
  any_nonNA(terra::rast(x))
}

#' @export
any_nonNA.SpatRaster <- function(x) {
  all(terra::global(x, "anynotNA")[[1]])
}

#' @export
any_nonNA.ZonesRaster <- function(x) {
  any_nonNA(terra::rast(raster::stack(raster::as.list(x))))
}

#' @export
any_nonNA.ZonesSpatRaster <- function(x) {
  any_nonNA(terra::rast(terra::as.list(x)))
}
