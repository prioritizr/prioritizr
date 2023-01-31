#' @include internal.R
NULL

#' Create a new `pproto` object
#'
#' Construct a new object with `pproto`. This object system is inspired
#' from the `ggproto` system used in the `ggplot2` package.
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. This is optional: if `NULL` (the default),
#'   no class name will be added to the object.
#'
#' @param _inherit `pproto` object to inherit from. If `NULL`, don"t
#'   inherit from any object.
#'
#' @param ... A list of members to add to the new `pproto` object.
#'
#' @examples
#' Adder <- pproto("Adder",
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
#' Abacus <- pproto("Abacus", Adder,
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
  assert(assertthat::is.string(`_class`) || is.null(`_class`),
    inherits(`_inherit`, "pproto") || is.null(`_inherit`))
  # copy objects from one proto to another proto
  assign_fields <- function(p1, p2) {
    if (!inherits(p2, "proto")) return()
    for (i in p2$ls()) {
      if (inherits(p2[[i]], "proto")) {
        p1[[i]] <- proto::proto()
        class(p1[[i]]) <- class(p2[[i]])
        assign_fields(p1[[i]], p2[[i]])
      } else {
        p1[[i]] <- p2[[i]]
      }
    }
    assign_fields(p1, p2$.super)
  }
  # create new proto
  p <- proto::proto()
  if (!is.null(`_inherit`)) {
    # assign inherited members
    assign_fields(p, `_inherit`)
    # assign inherited classes
    class(p) <- class(`_inherit`)
  } else {
    # assign pproto class
    class(p) <- c("pproto", class(p))
  }
  # assign members to new proto
  assign_fields(p, proto::proto(...))
  # assign new class if specified
  if (!is.null(`_class`))
    class(p) <- c(`_class`, class(p))
  # return value
  p
}
