#' @include internal.R
NULL

#' Print
#'
#' Display information about an object.
#'
#' @param x Any object.
#'
#' @param ... not used.
#'
#' @return None.
#'
#' @seealso [base::print()].
#'
#' @name print
#'
#' @aliases print,Id-method print,tbl_df-method
#'
#' @examples
#' a <- 1:4
#' print(a)
NULL

#' @rdname print
#'
#' @method print ConservationProblem
#'
#' @export
#'
print.ConservationProblem <- function(x, ...) x$print()

#' @rdname print
#'
#' @method print ConservationModifier
#'
#' @export
#'
print.ConservationModifier <- function(x, ...) x$print()

#' @rdname print
#'
#' @method print Id
#'
#' @export
#'
print.Id <- function(x, ...) message("id: ", x)

#' @name print
#'
#' @rdname print
#'
#' @usage \S4method{print}{Id}(x)
#'
methods::setMethod("print", "Id", function(x, ...) print.Id(x))

#' @rdname print
#'
#' @method print OptimizationProblem
#'
#' @export
#'
print.OptimizationProblem <- function(x, ...) x$print()

#' @rdname print
#'
#' @method print ScalarParameter
#'
#' @export
#'
print.ScalarParameter <- function(x, ...) x$print()

#' @rdname print
#'
#' @method print ArrayParameter
#'
#' @export
#'
print.ArrayParameter <- function(x, ...) x$print()

#' @rdname print
#'
#' @method print Solver
#'
#' @export
#'
print.Solver <- function(x, ...) x$print()

#' @rdname print
#'
#' @method print Zones
#'
#' @export
#'
print.Zones <- function(x, ...) {
  message("Zones",
          "\n  zones: ", repr_atomic(zone_names(x), "zones"),
          "\n  features: ", repr_atomic(feature_names(x), "features"),
          "\n  data type: ", class(x[[1]])[[1]])
}

#' @name print
#'
#' @rdname print
#'
#' @usage \S4method{print}{tbl_df}(x)
#'
methods::setMethod("print", "tbl_df", function(x, ...) base::print(x, ...))
