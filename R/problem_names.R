#' @include internal.R
NULL

#' Problem names
#'
#' Extract the names of the problems in an object.
#'
#' @param x [multi_problem()] object.
#'
#' @param ... not used.
#'
#' @return A `character` vector of names.
#'
#' @name problem_names
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#' @export
problem_names <- function(x, ...) {
  assert_required(x)
  UseMethod("problem_names")
}

#' @rdname feature_names
#'
#' @export
problem_names.MultiObjConservationProblem <- function(x, ...) {
  rlang::check_dots_empty()
  x$problem_names()
}
