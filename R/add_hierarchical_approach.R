#' Add a hierarchical approach
#'
#' Add a hierarchical multi-objective optimization approach to a
#' conservation planning problem.
#'
#' @param x [multi_problem()] object.
#'
#' @param rel_tol `numeric` vector of relative tolerances between objectives.
#' The length should be one less than the number of objectives.
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
    length(rel_tol) == (number_of_problems(x) - 1),
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
        data = list(rel_tol = rel_tol, method = method, verbose = verbose),
        run = function(x, solver) {
          rel_tol <- self$get_data("rel_tol")
          method <- self$get_data("method")
          verbose <- self$get_data("verbose")
          
          mobj <- x$obj
          mmodelsense <- x$modelsense
          mopt <- x$opt
          
          sols <- vector("list", length = nrow(mobj))
          
          if (identical(method, "gurobi")) {
            # build multiobj list from the problems
            multiobj <- lapply(seq_len(nrow(mobj)), function(i) {
              list(
                objn = if (mmodelsense[i] == "min") mobj[i, ] else -mobj[i, ],
                priority = nrow(mobj) - i + 1,
                weight = 1.0,
                reltol = if (i < nrow(mobj)) rel_tol[i] else NULL,
                name = paste0("Objective_", i)
              )
            })
            
            # store it in the solver data
            mopt$set_multiobj(multiobj) # something like this here
          } else {
            for (i in seq_len(nrow(mobj))) {
              if (verbose) cli::cli_alert_info("Solving objective {i}/{nrow(mobj)}")
              
              # set current objective
              mopt$set_obj(mobj[i, ])
              mopt$set_modelsense(mmodelsense[i])
              
              # solve problem directly
              sols[[i]] <- solver$solve(mopt)
              
              if (i != nrow(mobj)) {
                # calculate hierarchical constraint for next objective
                rhs <- sum(mobj[i, ] * sols[[i]]$x) *
                  ifelse(mmodelsense[i] == "min", 1 + rel_tol[i], 1 - rel_tol[i])
                sense <- ifelse(mmodelsense[i] == "min", "<=", ">=")
                
                if (verbose) cli::cli_alert_info("Adding hierarchical constraint for next objective: rhs={rhs}, sense={mmodelsense}")
                
                mopt$append_linear_constraints(
                  rhs = rhs,
                  sense = sense,
                  A = Matrix::drop0(Matrix::sparseMatrix(
                    i = rep(1, length(mobj[i, ])),
                    j = seq_along(mobj[i, ]),
                    x = mobj[i, ],
                    dims = c(1, length(mobj[i, ]))
                  )),
                  row_ids = "h"
                )
                
                # update starting solution if available
                # if (!is.null(solver$data) && "start_solution" %in% names(solver$data)) {
                #   solver$data$start_solution <- sols[[i]]$x
                # }
              }
            }
          }
          sol_final <- list(sols[[nrow(mobj)]])
          return(sol_final)
        }
      )
    )$new()
  )
}

