#' @include internal.R MultiObjConservationProblem-class.R ConservationProblem-class.R
NULL

#' Multi-objective conservation planning problem
#'
#' Create a multi-objective systematic conservation planning problem. This
#' function is used to combine multiple single-objective
#' [problem()] objects together for subsequent multi-objective optimization.
#' After constructing this object, it can be customized by specifying
#' a multi-objective optimization approach
#' for generating solutions (see [approaches]).
#' A solver (see [solvers]) can also be added customize optimization solver
#' software and settings.
#' After building the problem, the [solve()] function can be used to identify
#' solutions.
#'
#' @param ... [problem()] objects. Each argument corresponds to a
#' single-objective conservation planning problem that will be combined into a
#' multi-objective problem. All of these problems must share the same
#' planning units, zones, and decision variable types. They may have different
#' their objectives, targets, constraints, or penalties.
#'
#' @param problem_names `character` vector with a name for each problem
#' in `...`. Defaults to `NULL`, such that the problem names are defined
#' automatically.
#'
#' @details
#' Systematic conservation planning often involves balancing
#' multiple, often competing, objectives (Giakoumi *et al.* 2025).
#' For example, planners may want to
#' minimize costs, maximize provisioning of ecosystem services, and
#' minimize representation shortfalls for a set of threatened species.
#' Additionally, in multi-use planning, land use decisions may need to conserve
#' biodiversity, meet food demands, and provide adequate housing supply
#' (Neubert *et al.* 2025).
#' Although each of these objectives can be manually formulated
#' (i) independently using separate [problem()] objects or
#' (ii) as a single [problem()] using multiple linear constraints
#' (see [add_linear_constraints()]), multi-objective optimization provides a
#' framework for jointly optimizing all of them together
#' (Williams and Kendall 2017).
#' Here, each objective is formulated separately as a [problem()] object,
#' and then combined together with the `multi_problem()` function.
#' Although each of these [problem()] objects must have exactly the same
#' planning units, zones, and decision types, they can have different
#' objectives, features, targets, feature weights, and penalties.
#' Note that any constraint specified in one of the [problem()] objects
#' will be met at all stages during the multi-objective optimization process.
#' Since budgets under budget-limited objectives (e.g.,
#' [add_min_shortfall_objective()] and targets under the
#' minimum set objective (i.e., [add_min_set_objective()]) are treated
#' as constraints, they will also be met at all stages during the multi-
#' objective optimization process.
#'
#' @seealso
#' See [problem()] for constructing single-objective problems.
#' Also see [approaches()] for multi-objective methods.
#' Finally, see [solve()] for details on generating solutions.
#'
#' @references
#' Giakoumi S, Richardson AJ, Doxa A, Moro S, Andrello M, Hanson JO, Hermoso V,
#' Mazor T, McGowan J, Kujala H, Law E, Álvarez Romero JG, Magris RA, Gissi E,
#' Arafeh-Dalmau N, Metaxas A, Virtanen EA, Ban NC, Runya RM, Dunn DC,
#' Fraschetti S, Galparsoro I, Smith RJ, Bastardie F, Stelzenmüller V,
#' Possingham HP, and Katsanevakis S (2025) Advances in systematic conservation
#' planning to meet global biodiversity goals.
#' _Trends in Ecology and Evolution_, **40**: 395--410.
#'
#' Williams PJ and Kendall WL (2017) A guide to multi-objective optimization
#' for ecological problems with an application to cackling goose management.
#' _Ecological Modelling_, **343**: 54-67.
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
#' mp <-
#'   multi_problem(
#'     keystone_obj = problem(con_cost, keystone_spp) %>%
#'       add_min_shortfall_objective(budget) %>%
#'       add_relative_targets(0.6) %>%
#'       add_binary_decisions(),
#'     iconic_obj = problem(con_cost, iconic_spp) %>%
#'       add_min_shortfall_objective(budget) %>%
#'       add_relative_targets(0.4) %>%
#'       add_binary_decisions()
#'   ) %>%
#'   add_rel_constraint_approach(rel_tol = c(0.1, 0), verbose = FALSE) %>%
#'   add_gurobi_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' ms <- solve(mp)
#'
#' plot(ms, main = "Solution", axes = FALSE)
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
  assert(
    all(vapply(x, FUN.VALUE = logical(1), is_conservation_problem)),
    msg = "{.arg ...} must contain only {.fn problem} objects."
  )
  assert(all_comparable_problem(...))

  # if any of the problems in x have a non-default portfolio,
  # then throw a warning
  n_problems <- sum(
    !vapply(x, function(x) isTRUE(x$defaults$portfolio), logical(1))
  )
  verify(
    identical(n_problems, 0L),
    msg = c(
      paste(
        "{.arg ...} contains {cli::qty(", n_problems, ")}",
        "{?a/multiple} {.fn problem} object{?s} that {?has a/have}",
        "{.topic portfolios} added to {?it/them}."
      ),
      "i" = paste(
        "{.topic portfolios} will be ignored when generating",
        "solutions for a {.fn multi_problem} object."
      )
    ),
    call = NULL
  )

  # if any of the problems in x have a non-default solver,
  # then throw a warning
  n_problems <- sum(
    !vapply(x, function(x) isTRUE(x$defaults$solver), logical(1))
  )
  verify(
    identical(n_problems, 0L),
    msg = c(
      paste(
        "{.arg ...} contains {cli::qty(", n_problems, ")}",
        "{?a/multiple} {.fn problem} object{?s} that {?has a/have}",
        "{.topic solvers} added to {?it/them}."
      ),
      "i" = paste(
        "To specify solver settings for multi-objective optimization,",
        "the solver should be added to the resulting {.fn multi_problem}."
      )
    ),
    call = NULL
  )

  # return object
  new_multi_obj_conservation_problem(x)
}
