#' @include internal.R
NULL

#' Create a new pproto object
#'
#' Construct a new object with \code{pproto}. This object system is inspired
#' from the \code{\link[ggplot2]{ggproto}} system used in the
#' \code{\link[ggplot2]{ggplot2}} package.
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. This is optional: if \code{NULL} (the default),
#'   no class name will be added to the object.
#'
#' @param _inherit ggproto object to inherit from. If \code{NULL}, don't
#'   inherit from any object.
#'
#' @param ... A list of members in the pproto object.
#'
#' @examples
#' Adder <- pproto('Adder',
#'   x = 0,
#'   add = function(self, n) {
#'     self$x <- self$x + n
#'     self$x
#'   }
#' )
#'
#' Adder$add(10)
#' Adder$add(10)
#'
#' Abacus <- pproto('Abacus', Adder,
#'   subtract = function(self, n) {
#'     self$x <- self$x - n
#'     self$x
#'   }
#' )
#' Abacus$add(10)
#' Abacus$subtract(10)
#'
#' @export
pproto <- function(`_class` = NULL, `_inherit` = NULL, ...) {
  assertthat::assert_that(assertthat::is.string(`_class`) || is.null(`_class`),
    inherits(`_inherit`, 'pproto') || is.null(`_inherit`))
  if (!is.null(`_inherit`)) {
    x <- `_inherit`$proto(...)
    class(x) <- class(`_inherit`)
  } else {
    x <- proto::proto(...)
    class(x) <- c('pproto', class(x))
  }
  if (!is.null(`_class`))
    class(x) <- c(`_class`, class(x))
  x
}

