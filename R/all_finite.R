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

all_finite.default <- function(x) {
  stop("{.arg x} is not a recognized class.")
}

.S3method("all_finite", "default", all_finite.default)

all_finite.numeric <- function(x) {
  all(is.finite(x))
}

.S3method("all_finite", "numeric", all_finite.numeric)

all_finite.logical <- function(x) {
  all(is.finite(x))
}

.S3method("all_finite", "logical", all_finite.logical)

all_finite.character <- function(x) {
  all(is.finite(x))
}

.S3method("all_finite", "character", all_finite.character)

all_finite.factor <- function(x) {
  all(is.finite(x))
}

.S3method("all_finite", "factor", all_finite.factor)

all_finite.matrix <- function(x) {
  all(is.finite(c(x)))
}

.S3method("all_finite", "matrix", all_finite.matrix)

all_finite.array <- function(x) {
  all(is.finite(c(x)))
}

.S3method("all_finite", "array", all_finite.array)

all_finite.Matrix <- function(x) {
  all(is.finite(x@x))
}

.S3method("all_finite", "Matrix", all_finite.Matrix)

all_finite.data.frame <- function(x) {
  all(vapply(x, all_finite, logical(1)))
}

.S3method("all_finite", "data.frame", all_finite.data.frame)

all_finite.Spatial <- function(x) {
  all(vapply(x@data, all_finite, logical(1)))
}

.S3method("all_finite", "Spatial", all_finite.Spatial)

all_finite.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), all_finite, logical(1)))
}


.S3method("all_finite", "sf", all_finite.sf)
