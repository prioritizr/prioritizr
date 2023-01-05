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
  if (length(w) > 1) {
    w <- paste0(paste(w[-length(w)], collapse = ", "), ", or ", w[length(w)])
  }
  paste(deparse(call$x), "has some columns that are not a", w)
}

all_columns_inherit.default <- function(x, what) {
  stop("argument to x is not a recognized class")
}

.S3method("all_columns_inherit", "default", all_columns_inherit.default)

all_columns_inherit.data.frame <- function(x, what) {
  assertthat::assert_that(
    inherits(x, "data.frame"),
    is.character(what)
  )
  all(vapply(x, inherits, logical(1), what = what))
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
