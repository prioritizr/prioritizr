#' @include internal.R pproto.R ConservationProblem-proto.R
NULL

add_default_targets <- function(x) {
  # assert arguments are valid
  assert(is_conservation_problem(x))
  # throw error because targets must be chosen by the user
  cli::cli_abort(
    "{.fn problem} is missing targets.",
    "i" = "see {.topic prioritizr::targets} for guidance on selecting targets."
  )
}
