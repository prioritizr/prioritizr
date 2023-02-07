#' @include internal.R
NULL

#' @method print ConservationProblem
#'
#' @export
print.ConservationProblem <- function(x, ...) x$print()

#' @method print ConservationModifier
#'
#' @export
print.ConservationModifier <- function(x, ...) x$print()

#' @method print Id
#'
#' @export
print.Id <- function(x, ...) message("id: ", x)

#' @method print OptimizationProblem
#'
#' @export
print.OptimizationProblem <- function(x, ...) x$print()

#' @method print Solver
#'
#' @export
print.Solver <- function(x, ...) x$print()

#' @method print Zones
#'
#' @export
print.Zones <- function(x, ...) {
  message("Zones",
          "\n  zones: ", repr_atomic(zone_names(x), "zones"),
          "\n  features: ", repr_atomic(feature_names(x), "features"),
          "\n  data type: ", class(x[[1]])[[1]])
}
