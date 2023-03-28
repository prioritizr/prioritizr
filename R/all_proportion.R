#' @include internal.R
NULL

#' All proportion?
#'
#' Check if an object has only proportion values?
#'
#' @param object.
#'
#' @return A `logical` value.
#'
#' @noRd
all_proportion <- function(x) UseMethod("all_proportion")

assertthat::on_failure(all_proportion) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x),
    "} must have values between {.val {0}} and {.val {1}}."
  )
}

all_proportion.default <- function(x) {
  cli::cli_abort("{.arg x} is not a recognized class.")
}

.S3method("all_proportion", "default", all_proportion.default)

all_proportion.numeric <- function(x) {
  suppressWarnings(all(x >= 0 & x <= 1, na.rm = TRUE))
}

.S3method("all_proportion", "numeric", all_proportion.numeric)

all_proportion.Matrix <- function(x) {
  all_proportion.numeric(x@x)
}

.S3method("all_proportion", "Matrix", all_proportion.Matrix)

all_proportion.matrix <- function(x) {
  all_proportion.numeric(c(x))
}

.S3method("all_proportion", "matrix", all_proportion.matrix)

all_proportion.data.frame <- function(x) {
  all(vapply(x, all_proportion, logical(1)))
}

.S3method("all_proportion", "data.frame", all_proportion.data.frame)

all_proportion.Spatial <- function(x) {
  all(vapply(x@data, all_proportion, logical(1)))
}

.S3method("all_proportion", "Spatial", all_proportion.Spatial)

all_proportion.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), all_proportion, logical(1)))
}

.S3method("all_proportion", "sf", all_proportion.sf)
