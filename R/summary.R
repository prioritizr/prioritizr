#' @include internal.R
NULL

#' @method summary ConservationProblem
#'
#' @export
summary.ConservationProblem <- function(object, ...) {
  cli::cli({object$summary()})
}

#' @method summary MultiObjConservationProblem
#'
#' @export
summary.MultiObjConservationProblem <- function(object, ...) {
  object$summary(...)
}
