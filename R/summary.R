#' @include internal.R
NULL

#' @method summary ConservationProblem
#'
#' @export
summary.ConservationProblem <- function(object, ...) {
  cli::cli({object$summary()})
}

#' @method summary MultiConservationProblem
#'
#' @export
summary.MultiConservationProblem <- function(object, ...) {
  object$summary(...)
}
