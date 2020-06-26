#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Add default decisions
#'
#' This function adds the default decision types to a conservation planning
#' [problem()]. The default types are binary and are added using
#' the [add_binary_decisions()] function.
#'
#' @param x [ConservationProblem-class()] object.
#'
#' @seealso [decisions()].
#'
#' @export
add_default_decisions <- function(x) {
  add_binary_decisions(x)
}
