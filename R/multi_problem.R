#' @include internal.R MultiObjConservationProblem-class.R ConservationProblem-class.R 
NULL

#' Multi-objective conservation planning problem
#'
#' Create a multi-objective systematic conservation planning problem.
#'
#' @param ... [problem()] objects.
#'
#' @param problem_names `character` vector with a name for each problem
#' in `...`. Defaults to `NULL`, such that the problem names are defined
#' automatically.
#'
#' @details
#' TODO.
#'
#' @seealso
#' TODO.
#'
#' @references
#' TODO.
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#' @export
multi_problem <- function(..., problem_names = NULL) {
  # parse arguments
  x <- list(...)

  # if needed, create default names
  if (is.null(names(x)) && is.null(problem_names)) {
    problem_names <- paste("Problem", seq_along(x))
  }

  # if need, assign names
  if (is.null(names(x)) && !is.null(problem_names)) {
    ## assert arguments are valid
    assert(
      is.character(problem_names),
      assertthat::noNA(problem_names),
      no_duplicates(problem_names)
    )
    assert(
      identical(length(problem_names), length(x)),
      msg = c(
        "{.arg problem_names} must have a value each object in {.arg ...}.",
        "x" = "{.arg problem_names} has {length(problem_names)} element{?s}.",
        "x" = "{.arg ...} has {length(x)} object{?s}."
      )
    )
    ## assign names
    names(x) <- problem_names
  }

  # assert that arguments are valid
  assert(
    length(x) >= 2,
    msg = "{.arg ...} must contain at least two {.fn problem} objects."
  )
  # assert(de
  #   all(vapply(x, FUN.VALUE = logical(1), is_conservation_problem)),
  #   msg = "{.arg ...} must contain only {.fn problem} objects."
  # )
  assert(all_comparable_problem(...))

  # if any of the problems in x contain a portfolio that is different
  # from the default portfolio, then throw a warning
  if (!all(vapply(x, function(x) isTRUE(x$defaults$portfolio), logical(1)))) {
    cli_warning(
      c(
        "{.fn multi_problem} does not work with portfolios.",
        "i" = paste(
          "If multiple solutions are required,",
          "then use one of the {.topic approaches} functions."
        )
      ),
      call = NULL
    )
  }
  # TODO: throw warning if any of problems have a non default solver specified

  # return object
  new_multi_obj_conservation_problem(x)
}
