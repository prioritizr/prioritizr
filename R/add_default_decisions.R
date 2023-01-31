#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Add default decisions
#'
#' This function adds the default decision types to a conservation planning
#' [problem()]. The default types are binary and are added using
#' the [add_binary_decisions()] function.
#'
#' @param x [problem()] object.
#'
#' @inherit add_binary_decisions return
#'
#' @seealso
#' See [decisions] for an overview of all functions for adding decisions.
#'
#' @family decisions
#'
#' @export
add_default_decisions <- function(x) {
  rlang::check_required(x, call = NULL)
  add_binary_decisions(x)
}
