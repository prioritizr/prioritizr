#' Add a hierarchical approach
#'
#' Add a hierarchical multi-objective optimization approach to a
#' conservation planning problem.
#'
#' @param x [multi_problem()] object.
#'
#' @param rel_tol `numeric` vector containing the weights for each
#' objective. To generate multiple solutions based on different
#' combinations of weights, `rel_tol` can be a `numeric` matrix where
#' each row corresponds to a different solution and each columns
#' corresponds to a different objective.
#' The length/number of columns should be one less than the number of objectives.
#'
#' @param method `character` specifying the solving method.
#' Options: `"gurobi"` (default) or `"manual"`.
#'
#' @param verbose `logical` should progress on generating solutions
#' be displayed? Defaults to `TRUE`.
#'
#' @return An R6 object representing the hierarchical approach.
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
    length(as.matrix(rel_tol)[1,]) == (number_of_problems(x) - 1), #TODO write more elaborate error message for this
    assertthat::is.string(method),
    is_match_of(method, c("gurobi", "manual")),
    assertthat::is.flag(verbose)
  )
  
  # add approach
  x$add_approach(
    R6::R6Class(
      "HierarchicalApproach",
      inherit = MultiObjApproach,
      public = list(
        name = "hierarchical approach",
        data = list(rel_tol = rel_tol, verbose = verbose),
        run = function(x, solver) {
          
          rel_tol <- as.matrix(self$get_data("rel_tol"))
          
          sols <- vector("list", length = nrow(rel_tol)) # as many solutions as we have multiobj coefficients
          
          for (j in seq_len(nrow(rel_tol))) { # loop over rel_tol rows (different degradations)
            
            sols[[j]] <- solver$solve_multiobj(x, rel_tol[j, ]) #check here!!!!
            
          }
          sols
         }
      )
    )$new()
  )
}

