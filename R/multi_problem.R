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
#' @examples
#' \dontrun{
#' # In this example we select a set of planning units under a conservation 
#' # budget, aiming to meet representation targets for two species groups:
#' # (1) keystone species (higher ecological priority) and
#' # (2) iconic species (high social or cultural value).
#' 
#' # import data
#' con_cost <- get_sim_pu_raster()
#' keystone_spp <- get_sim_features()[[1:3]]
#' iconic_spp <- get_sim_features()[[4:5]]
#' 
#' # define a total conservation budget (30% of total cost)
#' budget <- terra::global(con_cost, "sum", na.rm = TRUE)[[1]] * 0.3
#'
#' # now create multi-objective problem
#' mp1 <- multi_problem(
#'   keystone_obj = problem(con_cost, keystone_spp) %>%
#'     add_min_shortfall_objective(budget) %>%
#'     add_relative_targets(0.4) %>%
#'     add_binary_decisions() %>%
#'     add_default_solver(),
#'   iconic_obj = problem(con_cost, iconic_spp) %>%
#'     add_min_shortfall_objective(budget) %>%
#'     add_relative_targets(0.4) %>%
#'     add_binary_decisions() %>%
#'     add_default_solver()
#' ) %>%
#'   add_rel_constraint_approach(rel_tol = 0.01, verbose = FALSE) %>%
#'   add_gurobi_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' ms1 <- solve(mp1)
#'
#' plot(ms1, main = "Low degradation")
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
