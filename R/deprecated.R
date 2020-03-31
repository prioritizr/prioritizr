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
#' @section  \code{add_connected_constraints}: this function has been renamed
#'   to the \code{\link{add_contiguity_constraints}} function.
#' @export
add_connected_constraints <- function(...) {
  .Deprecated("add_contiguity_constraints")
  paste0("this function has been renamed as the add_contiguity_constraints",
    "function")
}

#' @rdname prioritizr-deprecated
#' @section  \code{add_corridor_constraints}: this function has been
#'   reimagined as the \code{\link{add_feature_contiguity_constraints}}
#'   function.
#' @export
add_corridor_constraints <- function(...) {
  .Deprecated("add_feature_contiguity_constraints")
  paste0("this function has been renamed as the",
    "add_feature_contiguity_constraints function")
}

#' @rdname prioritizr-deprecated
#' @section  \code{set_number_of_threads}: this function is no longer needed now that superior data extraction methods are implemented.
#'   function.
#' @export
set_number_of_threads <- function(...) {
  .Deprecated(msg = "this function is no longer needed now that superior data extraction methods are implemented.")
}

#' @rdname prioritizr-deprecated
#' @section  \code{get_number_of_threads}: this function is no longer needed now that superior data extraction methods are implemented.
#'   function.
#' @export
get_number_of_threads <- function(...) {
  .Deprecated(msg = "this function is no longer needed now that superior data extraction methods are implemented.")
}

#' @rdname prioritizr-deprecated
#' @section  \code{is.parallel}: this function is no longer needed now that superior data extraction methods are implemented.
#'   function.
#' @export
is.parallel <- function(...) {
  .Deprecated(msg = "this function is no longer needed now that superior data extraction methods are implemented.")
}

#' @rdname prioritizr-deprecated
#' @section  \code{add_pool_portfolio}: this function is replaced by \code{\link{add_extra_portfolio}} and \code{\link{add_top_portfolio}}.
#'   function.
#' @export
add_pool_portfolio <- function(...) {
  .Deprecated(msg = "this function is deprecated and replaced by add_extra_portfolio and add_top_portfolio")
}

#' @rdname prioritizr-deprecated
#' @section  \code{connected_matrix}: this function has been renamed as
#' the \code{\link{adjacency_matrix}} function.
#' @export
connected_matrix <- function(...) {
  .Deprecated("adjacency_matrix")
  paste0("this function has been renamed as the adjacency_matrix function.")
}
