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
          
          solver$calculate(x = x$opt, 
                           mobj = x$obj, 
                           mmodelsense = x$modelsense) # only needed once because only rel_tol will change later and we only use that in run()
          
          sols <- vector("list", length = nrow(rel_tol)) # as many solutions as we have multiobj coefficients
          
          browser()
          for (i in seq_len(nrow(rel_tol))) {
  
            sols[[i]] <- solver$run_multiobj(rel_tol[i, ])  #either go into standard Solver-class run_multiobj or gurobi specific one
            
            ### compute and store objective values for each objective
            if (!is.null(sols[[i]]$x)) {
              sols[[i]]$objective <- stats::setNames(
                rowSums(
                  x$obj *
                    matrix(
                      sols[[i]]$x, ncol = ncol(x$obj),
                      nrow = nrow(x$obj), byrow = TRUE
                    )
                ),
                rownames(x$obj)
              )
            } 
          }
          
          sols
          
        #   rel_tol <- self$get_data("rel_tol")
        #   method <- self$get_data("method")
        #   verbose <- self$get_data("verbose")
        #   
        #   mobj <- x$obj
        #   mmodelsense <- x$modelsense
        #   mopt <- x$opt
        #   
        #   sols <- vector("list", length = nrow(mobj))
        #   
        #   if (identical(method, "gurobi")) {
        #     # build multiobj list from the problems
        #     multiobj <- lapply(seq_len(nrow(mobj)), function(i) {
        #       list(
        #         objn = if (mmodelsense[i] == "min") mobj[i, ] else -mobj[i, ],
        #         priority = i,
        #         weight = 1.0,
        #         reltol = if (i < nrow(mobj)) rel_tol[i] else NULL,
        #         name = paste0("Objective_", i)
        #       )
        #     })
        # 
        #     # use multiobj
        #     
        #     #browser()
        #     # solver$calculate(mopt)       
        #     # solver$set_multiobj(multiobj) 
        #     # solver$run()  
        #     # 
        #     solver$set_multiobj(multiobj)
        #     #solver$get_data("multiobj")
        #     sols[[nrow(mobj)]] <- solver$solve(mopt)
        # 
        #   } else {
        #     for (i in seq_len(nrow(mobj))) {
        #       if (verbose) cli::cli_alert_info("Solving objective {i}/{nrow(mobj)}")
        #       
        #       # set current objective
        #       mopt$set_obj(mobj[i, ])
        #       mopt$set_modelsense(mmodelsense[i])
        #       
        #       # solve problem directly
        #       sols[[i]] <- solver$solve(mopt)
        #       
        #       if (i != nrow(mobj)) {
        #         # calculate hierarchical constraint for next objective
        #         rhs <- sum(mobj[i, ] * sols[[i]]$x) *
        #           ifelse(mmodelsense[i] == "min", 1 + rel_tol[i], 1 - rel_tol[i])
        #         sense <- ifelse(mmodelsense[i] == "min", "<=", ">=")
        #         
        #         if (verbose) cli::cli_alert_info("Adding hierarchical constraint for next objective: rhs={rhs}, sense={mmodelsense}")
        #         
        #         mopt$append_linear_constraints(
        #           rhs = rhs,
        #           sense = sense,
        #           A = Matrix::drop0(Matrix::sparseMatrix(
        #             i = rep(1, length(mobj[i, ])),
        #             j = seq_along(mobj[i, ]),
        #             x = mobj[i, ],
        #             dims = c(1, length(mobj[i, ]))
        #           )),
        #           row_ids = "h"
        #         )
        #         
        #         # update starting solution if available
        #         # if (!is.null(solver$data) && "start_solution" %in% names(solver$data)) {
        #         #   solver$data$start_solution <- sols[[i]]$x
        #         # }
        #       }
        #     }
        #   }
        #   sol_final <- list(sols[[nrow(mobj)]])
        #   return(sol_final)
         }
      )
    )$new()
  )
}

