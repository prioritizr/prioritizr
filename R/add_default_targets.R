#' @include internal.R pproto.R ConservationProblem-proto.R
NULL

add_default_targets <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(is_conservation_problem(x))
  # throw error because targets must be chosen by the user
  stop("the specified objective requires targets, see ?targets for guidance")
}
