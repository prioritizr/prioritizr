#' @include internal.R
NULL

#' @method as.list Zones
#' @export
as.list.Zones <- function(x, ...) {
  attributes(x) <- NULL
  class(x) <- "list"
  x
}

#' @method as.list OptimizationProblem
#' @export
as.list.OptimizationProblem <- function(x, ...) {
  rcpp_optimization_problem_as_list(x$ptr)
}
