# ---
# repo: prioritizr/prioritizr
# file: standalone-all_columns_inherit.R
# imports: [assertthat (>= 0.2.0), cli (>= 3.6.0), sf (>= 1.0-12)]
# ---

#' All columns inherit?
#'
#' Check if all columns inherit from a particular class.
#'
#' @param x object.
#'
#' @param what `character` name of class.
#'
#' @return A `logical` value.
#'
#' @noRd
all_columns_inherit <- function(x, what) UseMethod("all_columns_inherit")

assertthat::on_failure(all_columns_inherit) <- function(call, env) {
  w <- eval(call$what, envir = env)
  paste0(
    "{.arg ", deparse(call$x),
    "} must have ", cli::format_inline("{.cls {w}}"),
    " values in all columns."
  )
}

#' @export
all_columns_inherit.default <- function(x, what) {
  cli::cli_abort("{.arg x} is not a recognized class.")
}

#' @export
all_columns_inherit.data.frame <- function(x, what) {
  # assert valid arguments
  assert(
    inherits(x, "data.frame"),
    is.character(what)
  )
  # account for the fact that inherits(x, "numeric") != is.numeric(x),
  # due to non-standardized way of handling integer values
  if (identical(what, "numeric")) {
    w <- c("numeric", "integer")
  } else {
    w <- what
  }
  # checks
  all(vapply(x, inherits, logical(1), what = w))
}

#' @export
all_columns_inherit.Spatial <- function(x, what) {
  all_columns_inherit(x@data, what)
}

#' @export
all_columns_inherit.sf <- function(x, what) {
  all_columns_inherit(sf::st_drop_geometry(x), what)
}
