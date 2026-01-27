#' @include internal.R MultiObjApproach-class.R
NULL

#' Add a relative constraint approach
#'
#' Add a relative constraint (hierarchical) multi-objective optimization 
#' approach to a conservation planning problem.
#'
#' @param x [multi_problem()] object.
#'
#' @param priority `numeric` vector of the priority order of the supplied 
#' problems in `x`. Problems with higher order priorities will be optimized 
#' first. For example, if `x` has two [problem()] objects with specified 
#' `priority = c(2,1)`, the first problem will be optimized for first and the 
#' second problem second. When no priority is provided, the problems provided 
#' first will be assumed to have higher priority.
#'
#' @param rel_tol `numeric` vector or matrix containing the relative tolerances 
#' for each [problem()] in `x`. If `x` is a vector, a single solution will be 
#' generated. If `x`is a matrix, then multiple solutions based on different
#' combinations of tolerances will be generated where each row corresponds to a 
#' different solution and each column corresponds to a different problem. Each 
#' `rel_tol` value represents an optimality that is equivalent to the `gap` 
#' specified when adding a solver to the problem, for example 
#' [add_gurobi_solver()]. `rel_tol` is relative and expresses the acceptable 
#' deviance from the optimal objective of the previous problem. For example, 
#' a value of 0 means no deviation from the objective value of the previous
#' problem is allowed. In turn, a value of 0.1 denotes that the objective value
#' of the previous solution can degrade by a maximum of 10%, for example by 
#' being 10% more costly if the previous problem minimised cost.
#' The length/number of columns should be one less than the number of problems 
#' in `x`.
#'
#' @param method `character` specifying the solving method. Available options 
#' are: (`"gurobi"`) using the internal Gurobi methodology for solving 
#' multi-objective problems hierarchically (default) and (`"manual"`) using a
#' manual methodology for any of the other solvers available in `prioritizr`.
#' We recommend using `"gurobi"` if the [*Gurobi*](https://www.gurobi.com/) 
#' solver is available.
#'
#' @param verbose `logical` should progress on generating solutions
#' be displayed? Defaults to `TRUE`.
#'
#' @details
#' The relative constraint or hierarchical approach is a lexicographic 
#' multi-objective optimization approach that solves [problem()] objects 
#' sequentially based on a pre-defined order of priority. In this approach, the 
#' first problem is solved, and the solution obtained constrain subsequent 
#' problems using the relative tolerances (or level of degradation) specified
#' in `rel_tol`. This ensures that higher-priority objectives are satisfied
#' before lower-priority objectives are considered. 
#' 
#' When `rel_tol` is a matrix, each row is interpreted as an independent
#' hierarchical configuration. These will be solved sequentially when `solve()`.
#'
#' This approach is appropriate when there is a clear priority order among
#' objectives, and when it is important that higher-priority objectives
#' are not compromised while optimizing lower-priority objectives.In general, 
#' we recommend using this approach because it can better approximate the 
#' Pareto Front than alternative approaches.
#' 
#' @section Mathematical formulation:
#'
#' Let a set of objectives (\eqn{K}{K} indexed by \eqn{k}{k}) be ordered 
#' according to a predefined hierarchy, with objective \eqn{1}{1} having the 
#' highest priority. Let \eqn{y_k}{yk} denote the value of objective \eqn{k}{k}. 
#' The hierarchical approach proceeds by optimizing objectives sequentially.
#'
#' First, the highest-priority objective \eqn{y_1}{y1} is optimized independently
#' to obtain an optimal value \eqn{y_1^*}{y1*}. Each subsequent objective
#' \eqn{y_i}{yi} is then optimized subject to constraints that limit the
#' degradation of all higher-priority objectives. Specifically, for objective
#' \eqn{i}{i}, the optimization problem can be written as:
#'
#' \deqn{\mathit{Optimize} \space y_i \\
#' \mathit{subject \space to} \\
#' y_k \leq y_k^* (1 + \text{rel\_tol}_k) \quad \forall k < i}{
#' Optimize yi subject to
#' yk <= yk* (1 + rel_tolk) for all k < i}
#'
#' where \eqn{y_k^*}{yk*} denotes the optimal value of higher-priority objective
#' \eqn{k}{k} obtained in previous optimization steps, and
#' \eqn{\text{rel\_tol}_k}{rel_tolk} is the allowable relative tolerance for
#' objective \eqn{k}{k}. This process is repeated sequentially for all objectives
#' in the hierarchy.
#'
#' This formulation ensures that higher-priority objectives are preserved within
#' user-defined tolerances, while lower-priority objectives are optimized as
#' much as possible within those constraints.
#'
#' @return
#' A modified `multi_problem()` object with the relative constraint approach
#' added. 
#'
#' @seealso
#' See [approaches] for an overview of all functions for adding a 
#' multi-objective approach.
#'
#' @references
#' Williams PJ and Kendall WL (2017) A guide to multi-objective optimization 
#' for ecological problems with an application to cackling goose management.
#' _Ecological Modelling_, **343**: 54-67.
#' 
#' Schuster R, Buxton R, Hanson JO, Binley AD, Pittman J, Tulloch V, La Sorte 
#' FA, Roehrdanz PR, Verburg PH, Rodewald AD, Wilson S, Possingham HP, and 
#' Bennett JR (2023) Protected area planning to conserve biodiversity in an 
#' uncertain future. _Conservation Biology_, **37**: e14048. 
#'
#' @family approaches
#'
#' @examples
#' \dontrun{
#' # import data
#' con_cost <- get_sim_pu_raster()
#' keystone_spp <- get_sim_features()[[1:3]]
#' iconic_spp <- get_sim_features()[[4:5]]
#'
#' # set budget
#' budget <- terra::global(con_cost, "sum", na.rm = TRUE)[[1]] * 0.3
#'
#' # define individual problems
#' p1 <-
#'   problem(con_cost, keystone_spp) %>%
#'   add_min_shortfall_objective(budget) %>%
#'   add_relative_targets(0.4) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
#' p2 <-
#'   problem(con_cost, iconic_spp) %>%
#'   add_min_shortfall_objective(budget) %>%
#'   add_relative_targets(0.4) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
#'
#' # solve problems for comparison
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#'
#' # plot
#' plot(s1)
#' plot(s2)
#'
#' # now create multi-objective problem
#' mp1 <- multi_problem(
#'   obj1 = problem(con_cost, keystone_spp) %>%
#'     add_min_shortfall_objective(con_budget) %>%
#'     add_relative_targets(0.4) %>%
#'     add_binary_decisions() %>%
#'     add_default_solver(),
#'   obj2 = problem(con_cost, iconic_spp) %>%
#'     add_min_shortfall_objective(con_budget) %>%
#'     add_relative_targets(0.4) %>%
#'     add_binary_decisions() %>%
#'     add_default_solver()
#' ) %>%
#'   add_rel_constraint_approach(rel_tol = 0.1, verbose = FALSE) %>%
#'   add_gurobi_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' ms1 <- solve(mp1)
#'
#' plot(ms1)
#'
#' # create multi-objective problem using input matrix
#' mp2 <- multi_problem(
#'   obj1 = problem(con_cost, keystone_spp) %>%
#'     add_min_shortfall_objective(con_budget) %>%
#'     add_relative_targets(0.4) %>%
#'     add_binary_decisions() %>%
#'     add_default_solver(),
#'   obj2 = problem(con_cost, iconic_spp) %>%
#'     add_min_shortfall_objective(con_budget) %>%
#'     add_relative_targets(0.4) %>%
#'     add_binary_decisions() %>%
#'     add_default_solver()
#' ) %>%
#'   add_rel_constraint_approach(rel_tol = matrix(c(0.9, 0.1), nrow = 2, ncol = 1), verbose = FALSE) %>%
#'   add_gurobi_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' ms2 <- solve(mp2)
#'
#' plot(c(ms2[[1]], ms2[[2]]), main = c("High degradation", "Low degradation"), axes = FALSE)
#'
#' # create multi-objective problem using input matrix
#' rel_tol <- matrix(seq(0, 1, length.out = 40), ncol = 1)
#'
#' mp3 <- multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_rel_constraint_approach(rel_tol, verbose = TRUE) %>%
#'   add_default_solver(verbose = FALSE)
#' ms3 <- solve(mp3)
#'
#' # extract objective values and plot approximated pareto front (very few weight values)
#' obj_mat <- attributes(ms5)$objective
#' plot(obj_mat)
#' }
#'
#' @export
add_rel_constraint_approach <- function(x, priority = NULL, rel_tol = NULL, method = "gurobi", verbose = TRUE) {
  # assert arguments
  assert_required(x)
  assert_required(rel_tol)
  assert_required(method)
  assert_required(verbose)
  assert(
    is_multi_conservation_problem(x),
    is.null(rel_tol) || (is.numeric(rel_tol) && all_positive(rel_tol)),
    is.null(priority) || is.numeric(priority),
    all_positive(rel_tol),
    assertthat::is.string(method),
    is_match_of(method, c("gurobi", "manual")),
    assertthat::is.flag(verbose)
  )

  if (!is.null(rel_tol)) {
    if (length((if (is.matrix(rel_tol)) rel_tol else matrix(rel_tol, nrow = 1))[1, ]) != (number_of_problems(x) - 1)) {
      msg <- ifelse(is.matrix(rel_tol),
        cli::cli_abort(c(
          "The number of columns of {.arg rel_tol} must be one less than the number of objectives.",
          "i" = "{.arg rel_tol} has {length(as.matrix(rel_tol)[1, ])} values.",
          "x" = "{.arg rel_tol} must have {number_of_problems(x) - 1} values."
        )),
        cli::cli_abort(c(
          "The length of {.arg rel_tol} must be one less than the number of objectives.",
          "i" = "{.arg rel_tol} has {length(as.matrix(rel_tol)[1, ])} values.",
          "x" = "{.arg rel_tol} must have {number_of_problems(x) - 1} values."
        ))
      )
    }
  }

  # add approach
  x$add_approach(
    R6::R6Class(
      "HierarchicalApproach",
      inherit = MultiObjApproach,
      public = list(
        name = "hierarchical approach",
        data = list(rel_tol = rel_tol, verbose = verbose),
        run = function(x, solver) {
          rel_tol <- self$get_data("rel_tol")
          rel_tol <- if (is.matrix(rel_tol)) rel_tol else matrix(rel_tol, nrow = 1)

          sols <- vector("list", length = nrow(rel_tol)) # as many solutions as we have multiobj coefficients

          ## if needed, set up progress bar
          if (isTRUE(verbose)) {
            cli::cli_inform(paste("Generating", nrow(rel_tol), "solutions..."))
            pb <- cli::cli_progress_bar(
              "Generating solutions",
              total = nrow(rel_tol),
              .envir = parent.frame()
            )
          }

          for (j in seq_len(nrow(rel_tol))) { # loop over rel_tol rows (different degradations)

            sols[[j]] <- solver$solve_multiobj(x, priority, rel_tol[j, ])

            ## if possible, update the starting solution for the solver
            if (
              !is.null(solver$data) &&
                !is.null(sols[[j]]$x) &&
                isTRUE("start_solution" %in% names(solver$data))
            ) {
              solver$data$start_solution <- sols[[j]]$x
            }

            ## if needed, update progress bar
            if (isTRUE(verbose)) {
              cli::cli_progress_update(id = pb)
            }
          }
          ## if needed, clean up progress bar
          if (isTRUE(verbose)) {
            cli::cli_progress_done(id = pb)
          }
          sols
        }
      )
    )$new()
  )
}
