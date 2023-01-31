#' @include internal.R
NULL

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
    "} has some columns that do not contain {.val {w}} values."
  )
}

all_columns_inherit.default <- function(x, what) {
  stop("{.arg x} is not a recognized class.")
}

.S3method("all_columns_inherit", "default", all_columns_inherit.default)

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

.S3method("all_columns_inherit", "data.frame", all_columns_inherit.data.frame)

all_columns_inherit.Spatial <- function(x, what) {
  all_columns_inherit(x@data, what)
}

.S3method("all_columns_inherit", "Spatial", all_columns_inherit.Spatial)

all_columns_inherit.sf <- function(x, what) {
  all_columns_inherit(sf::st_drop_geometry(x), what)
}

.S3method("all_columns_inherit", "sf", all_columns_inherit.sf)
