 #' @include internal.R OptimizationProblem-proto.R
NULL

#' Optimization problem
#'
#' Generate a new empty \code{\link{OptimizationProblem-class}} object.
#'
#' @return \code{\link{OptimizationProblem-class}} object.
#'
#' @seealso \code{\link{OptimizationProblem-methods}}
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
