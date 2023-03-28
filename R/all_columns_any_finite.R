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
  paste0(
    "{.arg ", deparse(call$x),
      "} must have columns that do not only contain missing or non-finite",
      " values (e.g., {.val {NaN}}, {.val {NA}}, {.val {Inf}})."
  )
}

all_columns_any_finite.default <- function(x) {
  cli::cli_abort("{.arg x} is not a recognized class.")
}

.S3method("all_columns_any_finite", "default", all_columns_any_finite.default)

all_columns_any_finite.data.frame <- function(x) {
  assert(
    is.data.frame(x)
  )
  all(colSums(vapply(x, is.finite, logical(nrow(x)))) > 0)
}

.S3method(
  "all_columns_any_finite", "data.frame",
  all_columns_any_finite.data.frame
)

all_columns_any_finite.matrix <- function(x) {
  assert(
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
