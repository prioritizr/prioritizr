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
          
          for (j in seq_len(nrow(rel_tol))) { # loop over rel_tol rows (different degradations)

            cli::cli_inform(
              c(
                "Solving problem for degradation {j}/{nrow(rel_tol)}",
                "Relative tolerance(s): {paste(rel_tol[j, ], collapse = ', ')}"
              )
            )
            
            sols[[j]] <- solver$solve_multiobj(x, rel_tol[j, ])
            
          }
          sols
        }
      )
    )$new()
  )
}
