#' @include internal.R Objective-class.R
NULL

add_default_objective <- function(x) {
  # assert argument is valid
  rlang::check_required(x)
  assert(is_conservation_problem(x), call = NULL)
  # throw error because objectives must be explicitly defined
  cli::cli_abort(
    "{.fn problem} is missing an objective.",
    "i" = paste(
      "see {.topic prioritizr::objectives} for guidance on selecting",
      "an objective."
    )
  )
}
