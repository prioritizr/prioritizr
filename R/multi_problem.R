#' @include internal.R MultiObjConservationProblem-class.R ConservationProblem-class.R
NULL

#' Multi-objective conservation planning problem
#'
#' Create a multi-objective systematic conservation planning problem. This
#' function is used to combine multiple single-objective
#' [problem()] objects into a multi-objective optimization formulation.
#'
#' @param ... [problem()] objects. Each argument represents an individual
#' single-objective conservation planning problem that will be combined into a
#' multi-objective problem. All supplied problems must share the same
#' planning units and zones, as well as locked-in/out areas, but may differ in 
#' their objectives, targets, additional constraints, or penalties.
#'
#' @param problem_names `character` vector with a name for each problem
#' in `...`. Defaults to `NULL`, such that the problem names are defined
#' automatically.
#'
#' @details
#' A systematic conservation planning exercise frequently requires balancing
#' multiple, often competing objectives. For example, planners may want to 
#' minimize cost, maximize habitat representation or minimizing shortfalls of as 
#' many targets as possible. Although each of these objectives can be formulated 
#' independently using [problem()], achieving them jointly requires a framework 
#' for multi-objective optimization.
#'
#' The `multi_problem()` function provides this framework by creating an
#' object that strategically combines a collection of single-objective conservation planning
#' problems. Each sub-problem contains its own planning units, features,
#' costs, targets, constraints, and penalties, exactly as if it were solved
#' independently.
#'
#' @seealso
#' See [problem()] for constructing single-objective problems.
#' Also see [approaches()] for multi-objective methods.
#' Finally, see [solve()] for details on generating solutions.
#'
#' @references
#' Williams PJ and Kendall WL (2017) A guide to multi-objective optimization 
#' for ecological problems with an application to cackling goose management.
#' Ecological Modelling, 343: 54-67
#' 
#' TODO add more
#'
#' @examples
#' \dontrun{
#' TODO add example from weighted-sum approach after updating it with some more context
#' # import data
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_features <- get_sim_features()
#'
#' weights <- runif(2)
#'
#' # create multi-object problem
#' p <-
#'   multi_problem(
#'     obj1 = problem(sim_zones_pu_raster[[1]], sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
#'       add_binary_decisions(),
#'     obj2 = problem(sim_zones_pu_raster[[2]], sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
#'       add_binary_decisions()
#'   ) %>%
#'   add_weighted_sum_approach(weights = weights, verbose = FALSE) %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s <- solve(p)
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
