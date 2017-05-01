#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Default decisions
#'
#' TODO
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @seealso \code{\link{solvers}}.
#'
#' @export
add_default_decisions <- function(x) {
  add_binary_decisions(x)
}
