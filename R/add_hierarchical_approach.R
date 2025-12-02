#' @include internal.R MultiObjApproach-class.R
NULL

#' Add a hierarchical approach
#'
#' Add a hierarchical multi-objective optimization approach to a
#' conservation planning problem.
#'
#' @param x [multi_problem()] object.
#'
#' @param rel_tol `numeric` vector containing the relative tolerances for each
#' objective. To generate multiple solutions based on different
#' combinations of tolerances, `rel_tol` can be a `numeric` matrix where
#' each row corresponds to a different solution and each column
#' corresponds to a different objective.
#' The length/number of columns should be one less than the number of objectives.
#'
#' @param method `character` specifying the solving method.
#' Options: `"gurobi"` (default) or `"manual"`.
#'
#' @param verbose `logical` should progress on generating solutions
#' be displayed? Defaults to `TRUE`.
#'
#' @details
#' The hierarchical approach is a lexicographic multi-objective optimization method
#' that solves objectives sequentially based on a pre-defined order of priority.
#' In this method, the first objective is optimized, and the solutions
#' obtained constrain subsequent objectives using the relative tolerances (or level of degradation) specified
#' in `rel_tol`. This ensures that higher-priority objectives are satisfied
#' before lower-priority objectives are considered. For example, if we have two objectives and a `rel_tol` of 0.1, then
#' the first objective is solved, and the objective value of its solution (e.g., minimized cost), cannot
#' degrade by more than 10% (i.e., 10% more costly) in the subsequent solution
#' when the next objective is optimized and the first one is used as aconstraint.
#' 
#' Specifically, let \(y_1, y_2, \ldots, y_k\) denote the values of the
#' objectives in the hierarchy. The hierarchical approach first optimizes \(y_1\),
#' then optimizes \(y_2\) subject to
#'
#' \deqn{y_1 \le y_1^* (1 + \text{rel_tol}_1),}
#'
#' where `rel_tol_1` is the first element of `rel_tol`, and \(y_1^*\) is the optimal value of \(y_1\).
#' This process is repeated sequentially for each subsequent objective,
#' with each objective \(y_i\) being optimized subject to the constraints
#' imposed by all higher-priority objectives \(y_1, \dots, y_{i-1}\)
#' according to the corresponding relative tolerances `rel_tol_1, \dots, rel_tol_{i-1}`.
#' 
#' When `rel_tol` is a matrix, each row is interpreted as an independent
#' hierarchical configuration. These will be solved sequentially when `solve()`.
#'
#' This approach is appropriate when there is a clear priority order among
#' objectives, and when it is important that higher-priority objectives
#' are not compromised while optimizing lower-priority objectives.
#'
#' @return 
#' A modified `multi_problem()` object with the hierarchical approach
#' added. This function does not solve the problem; it only records the
#' approach so that the problem can be compiled and solved later. After calling 
#' [solve()], the output will either be an individual solution or a list of 
#' solutions if a matrix was supplied for `rel_tol`.
#'
#' @seealso
#' See [approaches] for an overview of all functions for adding a multi-objective
#' approach.
#'
#' @references
#' TODO
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
#' con_budget <- terra::global(con_cost, "sum", na.rm = TRUE)[[1]] * 0.3
#' 
#' # define individual problems
#' p1 <-
#'   problem(con_cost, keystone_spp) %>%
#'   add_min_shortfall_objective(con_budget) %>%
#'   add_relative_targets(0.4) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
#' p2 <-
#'   problem(con_cost, iconic_spp) %>%
#'   add_min_shortfall_objective(con_budget) %>%
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
#' obj1 = problem(con_cost, keystone_spp) %>%
#'   add_min_shortfall_objective(con_budget) %>%
#'   add_relative_targets(0.4) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(),
#' obj2 = problem(con_cost, iconic_spp) %>%
#'   add_min_shortfall_objective(con_budget) %>%
#'   add_relative_targets(0.4) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
#' ) %>%
#'   add_hierarchical_approach(rel_tol = 0.1, verbose = FALSE) %>%
#'   add_gurobi_solver(gap = 0, verbose = FALSE) 
#' 
#' # solve problem
#' ms1 <- solve(mp1)
#' 
#' plot(ms1)
#' 
#' # create multi-objective problem using input matrix
#' mp2 <- multi_problem(
#' obj1 = problem(con_cost, keystone_spp) %>%
#'   add_min_shortfall_objective(con_budget) %>%
#'   add_relative_targets(0.4) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(),
#' obj2 = problem(con_cost, iconic_spp) %>%
#'   add_min_shortfall_objective(con_budget) %>%
#'   add_relative_targets(0.4) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
#' ) %>%
#'   add_hierarchical_approach(rel_tol = matrix(c(0.9, 0.1), nrow = 2, ncol = 1), verbose = FALSE) %>%
#'   add_gurobi_solver(gap = 0, verbose = FALSE) 
#' 
#' # solve problem
#' ms2 <- solve(mp2)
#' 
#' plot(c(ms2[[1]], ms2[[2]]), main = c("High degradation", "Low degradation"), axes = FALSE
#' 
#' # create multi-objective problem using input matrix
#' rel_tol <- matrix(seq(0, 1, length.out = 40), ncol = 1)
#' 
#' mp3 <- multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_hierarchical_approach(rel_tol, verbose = TRUE) %>%
#'   add_default_solver(verbose = FALSE)
#' ms3 <- solve(mp3)
#' 
#' # extract objective values and plot approximated pareto front (very few weight values)
#' obj_mat <- attributes(ms5)$objective
#' plot(obj_mat)
#' }
#'
#' @export
add_hierarchical_approach <- function(x, rel_tol, method = "gurobi", verbose = TRUE) {
  # assert arguments
  assert_required(x)
  assert_required(rel_tol)
  assert_required(method)
  assert_required(verbose)
  assert(
    is_multi_conservation_problem(x),
    is.numeric(rel_tol),
    all_positive(rel_tol),
    assertthat::is.string(method),
    is_match_of(method, c("gurobi", "manual")),
    assertthat::is.flag(verbose)
  )

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
              .envir = parent.frame() # can only get progress bar to work witht this
            )
          }

          for (j in seq_len(nrow(rel_tol))) { # loop over rel_tol rows (different degradations)

            sols[[j]] <- solver$solve_multiobj(x, rel_tol[j, ])

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
