 #' @include internal.R OptimizationProblem-proto.R
NULL

#' Optimization problem
#'
#' Generate a new empty [`OptimizationProblem-class`] object.
#'
#' @return [`OptimizationProblem-class`] object.
#'
#' @seealso [OptimizationProblem-methods]
#'
#' @examples
#' # create empty OptimizationProblem object
#' x <- new_optimization_problem()
#'
#' # print new object
#' print(x)
#' @export
new_optimization_problem <- function() {
  pproto(NULL, OptimizationProblem, ptr = rcpp_new_optimization_problem())
}
