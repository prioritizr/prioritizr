#' @include internal.R
NULL

#' Deprecated functions in the \pkg{prioritizr} package
#'
#' The functions listed below are deprecated and will be defunct in the
#' near future. Where applicable, alternative functions with similar
#' functionality are also listed below.
#'
#' @param ... not used.
#'
#' @name prioritizr-deprecated
#'
#' @keywords internal
NULL

#' @rdname prioritizr-deprecated
#' @section  `add_connected_constraints`: this function has been renamed
#'   to the [add_contiguity_constraints()] function.
#' @export
add_connected_constraints <- function(...) {
  .Deprecated("add_contiguity_constraints")
  paste("this function has been renamed to the add_contiguity_constraints()",
    "function")
}

#' @rdname prioritizr-deprecated
#' @section  `add_corridor_constraints`: this function has been
#'   reimagined as the [add_feature_contiguity_constraints()]
#'   function.
#' @export
add_corridor_constraints <- function(...) {
  .Deprecated("add_feature_contiguity_constraints")
  paste0("this function has been renamed to the",
    "add_feature_contiguity_constraints() function")
}

#' @rdname prioritizr-deprecated
#' @section  `set_number_of_threads`: this function is no longer needed now that superior data extraction methods are implemented.
#'   function.
#' @export
set_number_of_threads <- function(...) {
  .Deprecated(msg = "this function is no longer needed now that superior data extraction methods are implemented.")
}

#' @rdname prioritizr-deprecated
#' @section  `get_number_of_threads`: this function is no longer needed now that superior data extraction methods are implemented.
#'   function.
#' @export
get_number_of_threads <- function(...) {
  .Deprecated(msg = "this function is no longer needed now that superior data extraction methods are implemented.")
}

#' @rdname prioritizr-deprecated
#' @section  `is.parallel`: this function is no longer needed now that superior data extraction methods are implemented.
#'   function.
#' @export
is.parallel <- function(...) {
  .Deprecated(msg = "this function is no longer needed now that superior data extraction methods are implemented.")
}

#' @rdname prioritizr-deprecated
#' @section  `add_pool_portfolio`: this function is replaced by [add_extra_portfolio()] and [add_top_portfolio()].
#'   function.
#' @export
add_pool_portfolio <- function(...) {
  .Deprecated(msg = paste("this function is deprecated and replaced by the",
    "add_extra_portfolio() and add_top_portfolio() functions"))
}

#' @rdname prioritizr-deprecated
#' @section  `connected_matrix`: this function has been renamed as
#' the [adjacency_matrix()] function.
#' @export
connected_matrix <- function(...) {
  .Deprecated("adjacency_matrix")
  paste0("this function has been renamed to the adjacency_matrix() function.")
}

#' @rdname prioritizr-deprecated
#' @section  `feature_representation`: this function has been replaced by
#'  the [eval_feature_representation()] function for consistency with
#'  other functions for evaluating solutions. Note that the
#'  [eval_feature_representation()] function has a slightly different
#   output format to the now deprecated `feature_representation` function.
#' @export
add_connected_constraints <- function(...) {
  .Deprecated("feature_representation")
  paste0("this function has been replaced by the eval_feature_representation()",
    "function. Note that the eval_feature_representation() outputs feature",
    "representation data in a slightly different format.")
}
