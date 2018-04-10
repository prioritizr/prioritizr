#' @include internal.R
NULL

#' Deprecated functions in the \pkg{prioritizr} package
#'
#' The functions listed below are deprecated and will be defunct in the
#' near future. When possible, alternative functions with similar functionality
#' are also listed below.
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
  .Deprecated("add_connected_constraints")
  paste0("this function has been renamed to the add_contiguity_constraints
    function")
}
