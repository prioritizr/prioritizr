#' @include internal.R MultiObjApproach-class.R
NULL

#' Add a hierarchical approach
#'
#' Add a hierarchical (lexicographic) approach for multi-objective optimization
#' to a multi-objective conservation planning problem (Jaimes *et al.* 2009).
#' Broadly speaking, this approach involves using multiple optimization
#' procedures to solve each [problem()] in a
#' [multi_problem()] object following a hierarchical (lexicographic) ordering,
#' wherein those associated with a higher priority order are solved before
#' those with a lower priority order. When implementing this approach,
#' constraints are added after generating a given solution to ensure that
#' subsequent solutions for lower priority [problem()] objects have
#' adequate performance according to higher priority [problem()] objects.
#'
#' @param x [multi_problem()] object.
#'
#' @param rel_tol `numeric` vector or matrix denoting a set of the relative
#' tolerance values for each constraint added during the hierarchical
#' approach.
#' For example, if `x` has two problems, then the hierarchical approach
#' will involve adding one constraint and, in turn, require one
#' `rel_tol` value.
#' Alternatively, if `x` has three problems, then the hierarchical approach
#' will involve adding two constraints and, in turn, require two
#' `rel_tol` values.
#' Given this, a vector can be used to specify a set of values for
#' generating a single solution, where each value corresponds to a different
#' constraint.
#' Alternatively, a matrix can be used to specify
#' multiple sets of values for generating multiple solutions,
#' where each column corresponds to a different constraint and each row
#' corresponds to a different solution.
#' These `rel_tol` values specify how much each objective can be degraded
#' in subsequent optimization procedures (in other words, how much
#' wiggle room is allowed when optimizing other [problem()] objects with a
#' lower priority).
#' Greater `rel_tol` values denote a greater degree of sub-optimality
#' (similar to the `gap` parameters in the [solvers], such as
#' [add_gurobi_solver()]). For example, a value of 0
#' corresponds to zero reduction in quality,
#' and a value of 0.05 allows for up to a 5% reduction in quality.
#' In other words, if a problem in `x` with the highest `priority` value had the
#' minimum set objective (per [add_min_set_objective()]),
#' then the solution resulting from this
#' approach would cost no more than 105% of the total cost associated
#' with a solution that just involved minimizing cost (assuming that all
#' [problem()] objects in `x` had the same constraints).
#' See Details section below for more information.
#'
#' @param priority `numeric` vector or matrix denoting the priority order for
#' each [problem()] in `x`. A vector
#' can be used to specify a set of values for generating a single solution,
#' wherein each value corresponds to a different [problem()] in `x`.
#' Alternatively, a matrix can be used to specify multiple sets of values for
#' generating multiple solutions, wherein each column corresponds to a different
#' [problem()] in `x` and each row corresponds to a different solution.
#' Note that `priority` and `rel_tol` must have the
#' same format (e.g., both must be a vector, or both must be a matrix).
#' With the `priority` values, [problem()] objects in `x` that
#' are associated with a higher value will be optimized before those
#' with a lower value. See Details section below for
#' more information. Defaults to `NULL` such that each [problem()] in `x`
#' is assigned a priority reflecting their order in `x` (i.e., the first
#' [problem()] is assigned the highest priority value, and subsequent
#' [problem()] objects are assigned decreasing priority values).
#'
#' @param verbose `logical` should progress on generating multiple solutions
#' be displayed? Defaults to `TRUE`.
#'
#' @details
#' This multi-objective optimization approach is especially useful when there
#' is a well-defined order of importance among objectives in a planning
#' exercise (Williams and Kendall 2017; Schuster *et al.* 2023).
#' In general, we recommend using this approach because it is highly
#' flexible and can better characterize trade-offs than alternative
#' approaches.
#' By specifying an explicit priority order for each objective
#' (per `priority`) and acceptable tolerances for degradation
#' (per `rel_tol`), the parameters for this approach
#' are highly transparent. Additionally, the approach
#' is not sensitive to differences in scale among
#' different objectives (unlike the weighted sum approach,
#' [add_wtd_sum_approach()]; see Das and Dennis 1997 for details), and so it
#' can be readily applied to a wide range of objectives.
#'
#' The hierarchical approach involves solving the [problem()] objects
#' in `x` based on a pre-defined order of priority
#' (per `priority`).
#' In particular, it involves the following steps:
#' (i) the problem with the highest priority is selected (per `priority`);
#' (ii) a solution is generated to this problem;
#' (ii) the performance of the solution is measured
#' based on its ability to achieve the objective for this problem
#' (i.e., the objective value);
#' (iii) the objective value and the relative tolerance parameter for this
#' problem (per `rel_tol`) are used to constrain
#' subsequent optimization analyses (i.e., wherein a greater `rel_tol` value
#' means that subsequent solutions do not have to achieve such a good
#' level of performance according to the objective for this problem);
#' (iv) the problem with the next highest priority is selected (per `priority`);
#' (v) steps (ii) -- (iv) are repeated until a solution has been generated
#' to the problem with the lowest priority (per `priority`); and
#' (vi) the solution obtained from solving the problem with the lowest priority
#' (per `priority`) is returned.
#' Note that any constraints specified for any of the
#' [problem()] objects in `x` will be considered during any of the
#' optimization analyses. For example, if `x` has three [problem()] objects and
#' the second problem has locked in constraints (per
#' [add_locked_in_constraints()], then these constraints will be considered
#' when generating solutions to each of the three problems.
#' Additionally, if any of the [problem()] objects in `x` are based
#' on the minimum set formulation of the reserve selection problem
#' (per [add_min_set_objective()]), then the targets will be considered
#' when generating solutions to any of the problems in `x`. This is because
#' the targets in a minimum set formulation are treated as (hard) constraints,
#' and solutions must always meet them.
#'
#' The `priority` and `rel_tol` parameters specify how much influence each
#' [problem()] in `x` has over the multi-objective optimization process.
#' For example, let's consider an example where `x` has three problems,
#' `priority = c(2, 4, 1)`, and `rel_tol = c(0, 0.2)`.
#' In this example, the second [problem()] in `x` will be optimized first
#' because it has the highest `priority` value (i.e., 4) .
#' After generating a solution to the second problem, subsequent optimization
#' analyses will be constrained to ensure that all subsequent solutions
#' perform no worse than optimality according to the objective
#' of the second problem (because the first `rel_tol` value is 0).
#' The first [problem()] in `x` will then be optimized next, because it has the
#' next highest `priority` value (i.e., 2).
#' After generating a solution based on the first problem, subsequent
#' optimization analyses will be constrained to ensure that all subsequent
#' solutions perform  (i) no worse than optimality according to the objective
#' of the second problem (because the first `rel_tol` value is 0),
#' and (ii) no worse than a 20% reduction in performance according to the
#' objective of the first problem (because the second `rel_tol` value is 0.2).
#' Note that, because this new solution was generated with constraints
#' to ensure optimal performance according to the second problem,
#' this solution would likely have worse performance according to the objective
#' of the first problem than a solution that was generated by
#' solving the first problem directly.
#' The third [problem()] in `x` will be optimized next, because
#' it has the next highest `priority` value (i.e., 1).
#' Since the problems optimized previously had relatively low relative
#' tolerance parameters (i.e., 0 and 0.2), the performance of this new solution
#' according to the objective of the third problem would probably have much
#' worse than a solution that was generated by solving the third problem
#' directly.
#' Finally, the solution obtained by optimizing the third problem will be
#' returned as the resulting solution from the multi-objective optimization
#' approach.
#'
#' @section Mathematical formulation:
#' This approach can be expressed mathematically for a set of
#' objectives associated with the [problem()] objects in `x`.
#' Let \eqn{O}{O} denote the set of objectives (indexed by \eqn{o}{o}).
#' For brevity, we will assume that all of the objectives should ideally be
#' maximized and have been sorted in order of
#' priority (per `priority`), such that the objective with the highest priority
#' is \eqn{o=1}{o=1}, objective with the second highest priority is
#' \eqn{o=2}{o=2}, and so on.
#' Also, let \eqn{f_o(x)}{fo(x)} denote the objective function for each
#' objective \eqn{o \in O}{o in O}, where \eqn{x} represents all the decision
#' variables for calculating the objective values (e.g., planning unit selection
#' status values).
#' Additionally, let \eqn{r_o}{ro} denote the relative tolerance (per
#' `rel_tol`) parameter for each objective \eqn{o \in O}{o in O}.
#' Furthermore, let \eqn{S}{S} represent the set (region) of feasible
#' values for \eqn{x} based on the constraints for all of the objectives
#' (e.g., if the first problem in `x` has locked in constraints and the
#' second problem has locked out constraints, then \eqn{S}{S} would
#' account for both the locked in and locked out constraints).
#' Given this terminology, the approach starts by solving the following
#' optimization problem based on the first objective.
#'
#' \deqn{
#' \mathit{Maximize} \space f_1(x) \\
#' \mathit{subject \space to \space} x \in S
#' }{
#' Maximize f1(x), subject to x in S
#' }
#'
#' After solving this problem, let \eqn{v_1}{v1} denote the optimal objective
#' value for the solution. Next, the approach involves solving the following
#' optimization problem based on the second objective, along with a constraint
#' based on \eqn{v_1}{v1} and the relative tolerance parameter for the first
#' objective (i.e., \eqn{r_1}{r1}).
#' \deqn{
#' \mathit{Maximize} \space f_2(x) \\
#' \mathit{subject \space to \space} x \in S \\
#' f_1(x) \geq v_1 \times (1 - r_1)
#' }{
#' Maximize f2(x) subject to x in S & f1(x) & f1(x) >= v1 * (1 - r1)
#' }
#'
#' Similar to the previous step, let \eqn{v_2}{v2}
#' denote the optimal objective value for the solution.
#' The approach then involves solving the following
#' optimization problem based on the third objective, along with
#' constraints based on \eqn{v_1}{v1} and \eqn{v_2}{v2} and the relative
#' tolerance parameters for the first and second objectives (i.e.,
#' \eqn{r_1}{r1} and \eqn{r_2}{r2}).
#' \deqn{
#' \mathit{Maximize} \space f_3(x) \\
#' \mathit{subject \space to \space} x \in S \\
#' f_1(x) \geq v_1 \times (1 - r_1) \\
#' f_2(x) \geq v_2 \times (1 - r_2)
#' }{
#' Maximize f2(x) subject to x in S & f1(x) &
#' f1(x) >= v1 * (1 - r1) &
#' f2(x) >= v2 * (1 - r2)
#' }
#'
#' In this manner, the approach involves iteratively formulating and solving
#' optimization problems until all of the objectives \eqn{o \in O}{o in O} have
#' been considered. After a solution has been generated based on the last
#' objective (i.e., lowest priority objective), then this solution is returned.
#'
#' @return
#' An updated `multi_problem()` object with the approach
#' added to it.
#'
#' @seealso
#' See [approaches] for an overview of all functions for adding an approach.
#' Also, see [approach_rel_tol_matrix()] to automatically create a matrix
#' for `rel_tol`.
#'
#' @references
#' Das I and Dennis JE (1997) A closer look at drawbacks of minimizing weighted
#' sums of objectives for Pareto set generation in multicriteria optimization
#' problems. _Structural Optimization_, **14**: 63--69.
#'
#' Jaimes AL, Saúl ZM, and Coello Coello CA (2009) *An introduction to
#' multiobjective optimization techniques* in Optimization in Polymer
#' Processing. Eds Gaspar-Cunha A and Covas JA. Nova Science Publishers Inc,
#' New York, United States.
#'
#' Schuster R, Buxton R, Hanson JO, Binley AD, Pittman J, Tulloch V, La Sorte
#' FA, Roehrdanz PR, Verburg PH, Rodewald AD, Wilson S, Possingham HP, and
#' Bennett JR (2023) Protected area planning to conserve biodiversity in an
#' uncertain future. _Conservation Biology_, **37**: e14048.
#'
#' Williams PJ and Kendall WL (2017) A guide to multi-objective optimization
#' for ecological problems with an application to cackling goose management.
#' _Ecological Modelling_, **343**: 54-67.
#'
#' @family approaches
#'
#' @examples
#' \dontrun{
#' # in this example, we aim to identify a set of planning units that will
#' # not exceed a particular budget and meet objectives for
#' # (i) representing species that are important for ecosystem
#' # functioning (hereafter, keystone species) and (ii) representing species
#' # that have high social or cultural value (hereafter, iconic species)
#'
#' # import data
#' con_cost <- get_sim_pu_raster()
#' keystone_spp <- get_sim_features()[[1:3]]
#' iconic_spp <- get_sim_features()[[4:5]]
#'
#' # define a total conservation budget (30% of total cost)
#' budget <- terra::global(con_cost, "sum", na.rm = TRUE)[[1]] * 0.3
#'
#' # define a single-objective problem for the keystone species objective
#' p1 <-
#'   problem(con_cost, keystone_spp) %>%
#'   add_min_shortfall_objective(budget) %>%
#'   add_relative_targets(0.4) %>%
#'   add_binary_decisions()
#'
#' # define a single-objective problem for the iconic species objective
#' p2 <-
#'   problem(con_cost, iconic_spp) %>%
#'   add_min_shortfall_objective(budget) %>%
#'   add_relative_targets(0.45) %>%
#'   add_binary_decisions()
#'
#' # solve the single-objective problems
#' s1 <-
#'   p1 %>%
#'   add_default_solver(verbose = FALSE) %>%
#'   solve()
#' s2 <-
#'   p2 %>%
#'   add_default_solver(verbose = FALSE) %>%
#'   solve()
#'
#' # plot the solutions to the single-objective problems
#' plot(s1, main = "Keystone species", axes = FALSE)
#' plot(s2, main = "Iconic species", axes = FALSE)
#'
#' # now we will a create multi-objective problem that simultaneously
#' # considers both of these objectives
#'
#' # the first objective for keystone species will have a higher order of
#' # priority than the second objective for iconic species -- because
#' # the long-term persistence of iconic species depends on ecosystem
#' # functioning -- and we will specify a very small relative tolerance
#' # parameter so that the solution has a relatively high performance according
#' # to the first objective (i.e., relatively low representation shortfalls for
#' # keystone species)
#' mp1 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_hier_approach(
#'     rel_tol = 0.01,
#'     priority = c(2, 1),
#'     verbose = FALSE
#'   ) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve multi-objective problem
#' ms1 <- solve(mp1)
#'
#' # plot solution to multi-objective problem
#' plot(ms1, main = "multi-objective solution", axes = FALSE)
#'
#' # we will explore trade-offs between the two objectives, by generating
#' # multiple solutions using multi-objective optimization
#'
#' # create a matrix with 40 different combinations of relative tolerance values
#' # that can be used to generate 40 solutions
#' rel_tol_matrix <- approach_rel_tol_matrix(
#'   n_problems = 2, n_values = 40, max = 1.2
#' )
#'
#' # preview matrix with relative tolerance values
#' head(rel_tol_matrix)
#'
#' # create a multi-objective problem with the matrix of relative tolerance
#' # values and - because we do not specify values for priority - the
#' # optimization process will assume that the objectives are already
#' # specified in order of priority
#' mp2 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_hier_approach(
#'     rel_tol = rel_tol_matrix,
#'     verbose = FALSE
#'   ) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve multi-objective problem and generate 40 solutions
#' ms2 <- solve(mp2)
#'
#' # plot multiple solutions
#' plot(terra::rast(ms2), axes = FALSE)
#'
#' # extract objective values for the solutions
#' obj_matrix <- attributes(ms2)$objective
#'
#' # preview the objective values
#' head(obj_matrix)
#'
#' # plot the objectives values to visualize trade-offs
#' # (note that smaller values are better because these objectives seek to
#' # minimize representation shortfalls)
#' plot(
#'   obj_matrix,
#'   main = "Trade-offs between objectives",
#'   xlab = "Keystone objective (shortfall)",
#'   ylab = "Iconic objective (shortfall)"
#' )
#' }
#' @export
add_hier_approach <- function(x, rel_tol, priority = NULL, verbose = TRUE) {
  # assert arguments are valid
  assert_required(x)
  assert_required(rel_tol)
  assert_required(priority)
  assert_required(verbose)
  assert(
    is_multi_conservation_problem(x),
    is.numeric(rel_tol),
    all_positive(rel_tol),
    all_finite(rel_tol),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

  # additional checks for priority
  if (is.null(priority) && is.matrix(rel_tol)) {
    ## if priority not specified and rel_tol is matrix, then create matrix
    priority <- matrix(
      seq(number_of_problems(x), 1L), byrow = TRUE,
      ncol = number_of_problems(x), nrow = nrow(rel_tol)
    )
  } else if (is.null(priority) && is.vector(rel_tol)) {
    ## if priority not specified and rel_tol is vector, then create vector
    priority <- seq(number_of_problems(x), 1L)
  } else {
    ## otherwise, check that priority has valid values
    assert(
      is.numeric(priority),
      all_positive(priority),
      all_finite(priority)
    )
  }

  # additional checks for rel_tol and priority
  assert(
    (is.matrix(rel_tol) && is.matrix(priority)) ||
    (!is.matrix(rel_tol) && !is.matrix(priority)),
    msg = c(
      paste(
        "{.arg priority} and {.arg rel_tol} must both have the same class."
      ),
      "x" = "{.arg priority} is a {.cls {class(priority)}} object.",
      "x" = "{.arg rel_tol} is a {.cls {class(rel_tol)}} object."
    )
  )
  if (is.matrix(rel_tol)) {
    ## if rel_tol and priority are matrices, then perform additional checks
    assert(
      identical(number_of_problems(x) - 1L, ncol(rel_tol)),
      msg = c(
        paste(
          "{.arg rel_tol} must have a column for each constraint added during",
          "the approach."
        ),
        "i" = "{.arg x} has {number_of_problems(x)} problem{?s}.",
        "i" = paste(
          "The approach will involve adding {number_of_problems(x) - 1}",
          "constraint{?s}."
        ),
        "x" = "{.arg rel_tol} has {ncol(rel_tol)} column{?s}."
      )
    )
    assert(
      identical(number_of_problems(x), ncol(priority)),
      msg = c(
        "{.arg priority} must have a column for each problem in {.arg x}.",
        "i" = "{.arg x} has {number_of_problems(x)} problem{?s}.",
        "x" = "{.arg priority} has {ncol(priority)} column{?s}."
      )
    )
    assert(
      identical(nrow(rel_tol), nrow(priority)),
      msg = c(
        "{.arg priority} and {.arg rel_tol} must have the same number of rows.",
        "x" = "{.arg priority} has {nrow(priority)} row{?s}.",
        "x" = "{.arg rel_tol} has {nrow(rel_tol)} row{?s}."
      )
    )
    assert(nrow(rel_tol) > 0)
  } else {
    ## if rel_tol and priority are vectors, then perform additional checks
    assert(
      identical(number_of_problems(x) - 1L, length(rel_tol)),
      msg = c(
        paste(
          "{.arg rel_tol} must have a value for each constraint added during",
          "the approach."
        ),
        "i" = "{.arg x} has {number_of_problems(x)} problem{?s}.",
        "i" = paste(
          "The approach will involve adding {number_of_problems(x) - 1}",
          "constraint{?s}."
        ),
        "x" = "{.arg rel_tol} has {length(rel_tol)} value{?s}."
      )
    )
    assert(
      identical(number_of_problems(x), length(priority)),
      msg = c(
        "{.arg priority} must have a value for each problem in {.arg x}.",
        "i" = "{.arg x} has {number_of_problems(x)} problem{?s}.",
        "x" = "{.arg priority} has {length(priority)} value{?s}."
      )
    )

    ## standardize rel_tol and priority to matrix format
    rel_tol <- matrix(rel_tol, nrow = 1)
    priority <- matrix(priority, nrow = 1)
  }

  # add approach
  x$add_approach(
    R6::R6Class(
      "HierarchicalApproach",
      inherit = MultiObjApproach,
      public = list(
        name = "hierarchical approach",
        data = list(rel_tol = rel_tol, priority = priority, verbose = verbose),
        run = function(x, solver) {
          ## extract parameters
          rel_tol <- self$get_data("rel_tol")
          priority <- self$get_data("priority")
          params <- cbind(rel_tol, priority)
          ## initialize output
          sols <- vector("list", length = nrow(rel_tol))
          ## if needed, set up progress bar
          if (isTRUE(verbose)) {
            pb <- cli::cli_progress_bar(
              total = nrow(rel_tol),
              .envir = parent.frame()
            )
          }
          ## iterate over each different parameter set
          for (i in seq_len(nrow(rel_tol))) {
            ### generate solution,
            ### note that the solver$solve_multiobj() method will
            ### automatically calculate objective values
            s <- solver$solve_multiobj(
              x, priority = priority[i, ], rel_tol = rel_tol[i, ]
            )
            ### validate solution
            assert(
              is_valid_raw_solution(s, multiple = FALSE),
              call = rlang::expr(solve())
            )
            ### store solution
            sols[[i]] <- s
            ### if needed, update starting solution
            if (!identical(i, nrow(rel_tol))) {
              ### identify starting solution for next run
              j <- which_least_different(
                params[seq_len(i), , drop = FALSE],
                params[i + 1, , drop = FALSE]
              )
              ### update the starting solution for the solver
              solver$set_start_solution(sols[[j]]$x, warn = FALSE)
            }
            ### if needed, update progress bar
            if (isTRUE(verbose)) {
              cli::cli_progress_update(id = pb)
            }
          }
          ## if needed, clean up progress bar
          if (isTRUE(verbose)) {
            cli::cli_progress_done(id = pb)
          }
          ## return output
          sols
        }
      )
    )$new()
  )
}
