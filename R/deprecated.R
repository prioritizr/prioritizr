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
  .Deprecated("add_connected_constraints")
  paste0("this function has been renamed as the add_contiguity_constraints",
    "function")
}

#' @rdname prioritizr-deprecated
#' @section  \code{add_corridor_constraints}: this function has been
#'   reimagined as the \code{\link{add_feature_contiguity_constraints}}
#'   function.
#' @export
add_corridor_constraints <- function(...) {
  .Deprecated("add_corridor_constraints")
  paste0("this function has been renamed as the",
    "add_feature_contiguity_constraints function")
}
