#' @include internal.R pproto.R Objective-proto.R
NULL

add_default_objective <- function(x) {
  # assert argument is valid
  assertthat::assert_that(is_conservation_problem(x))
  # throw error because objectives must be explicitly defined
  stop("problem is missing an objective, see ?objectives for guidance")
}
