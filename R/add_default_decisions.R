#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Add default decisions
#'
#' This function adds the default decision types to a conservation planning
#' \code{\link{problem}}. The default types are binary and  are added using
#' the \code{\link{add_binary_decisions}} function.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @seealso \code{\link{decisions}}.
#'
#' @export
add_default_decisions <- function(x) {
  add_binary_decisions(x)
}
