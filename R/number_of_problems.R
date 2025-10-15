#' @include internal.R
NULL

#' Number of problems
#'
#' Extract the number of conservation problems in an object.
#'
#' @param x A [problem()],or [multi_problem()] object.
#'
#' @param ... not used.
#'
#' @return An `integer` number of problems.
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#' @export
number_of_problems <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  UseMethod("number_of_problems")
}

#' @rdname number_of_problems
#'
#' @export
number_of_problems.ConservationProblem <- function(x, ...) {
  1L
}

#' @rdname number_of_features
#'
#' @export
number_of_problems.MultiObjConservationProblem <- function(x, ...) {
  x$number_of_problems()
}
