#' @include internal.R MultiObjApproach-class.R
NULL

#' Add a weighted sum approach
#'
#' Add a weighted sum approach for multi-objective optimization to a
#' conservation planning problem.
#'
#' @param x [multi_problem()] object.
#'
#' @param weights `numeric` vector or matrix containing the weights for each 
#' [problem()] in `x`. If `x` is a vector, a single solution will be 
#' generated. If `x`is a matrix, then multiple solutions based on different
#' combinations of weights will be generated where each row corresponds to a 
#' different solution and each column corresponds to a different problem.
#'
#' @param verbose `logical` should progress on generating solutions
#' displayed? Defaults to `TRUE`.
#'
#' @details
#' The weighted sum approach is an approach for solving 
#' multi-objective optimization problems by transforming several
#' objectives into a single objective. This is done by taking a 
#' weighted linear combination of the objective expressions from each 
#' [problem()] in the [multi_problem()] object.
#' 
#' The weights determine the relative importance of the objectives and
#' can be used to explore trade-offs among conservation, cost, and other
#' planning outcomes.  
#' 
#' When `weights` is a matrix, each row is interpreted as an independent
#' weighting scheme and columns are the individual objectives. The approach 
#' stores a separate solution for each resulting weighted-sum problem. 
#' These will be solved sequentially when `solve()`.
#' 
#' This approach is suitable when objectives can be meaningfully combined
#' after weighting, and when there is no clear priority that needs to be 
#' defined with [add_rel_constraint_approach()].
#' 
#' @section Mathematical formulation:
#' 
#' Let a set of objectives (\eqn{K}{K} indexed by \eqn{k}{k}) be defined for a
#' multi-objective optimization problem, and let \eqn{y_k}{yk} denote the value
#' of objective \eqn{k}{k}. If the user supplies a weight vector
#' \eqn{w = (w_1, \ldots, w_K)}{w = (w1, ..., wK)}, the weighted-sum approach
#' combines all objectives into a single scalar objective function.
#'
#' Specifically, the transformed optimization problem can be written as:
#'
#' \deqn{\mathit{Minimize} \space \sum_{k = 1}^{K} w_k \times y_k}{
#' Minimize sum_k^K wk * yk}
#'
#' where \eqn{w_k}{wk} denotes the weight associated with objective \eqn{k}{k},
#' and \eqn{y_k}{yk} is the corresponding objective value. The relative magnitude
#' of \eqn{w_k}{wk} determines the contribution of objective \eqn{k}{k} to the
#' overall objective function.
#'
#' This formulation enables trade-offs among objectives to be controlled
#' explicitly through the choice of weights, with all objectives optimized
#' simultaneously within a single optimization problem.
#'
#' @return
#' A modified `multi_problem()` object with the weighted-sum approach
#' added. 
#'
#' @seealso
#' See [approaches] for an overview of all functions for adding an approach.
#' 
#' @references
#' Williams PJ and Kendall WL (2017) A guide to multi-objective optimization 
#' for ecological problems with an application to cackling goose management.
#' _Ecological Modelling_, **343**: 54-67.
#'
#' @family approaches
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
#' plot(s1, main = "Keystone Species")
#' plot(s2, main = "Iconic Species")
#' 
#' # now create multi-objective problem
#' mp1 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>% 
#'   add_weighted_sum_approach(c(0.5, 0.5), verbose = TRUE) %>% 
#'   add_default_solver()
#' 
#' # solve problem
#' ms1 <- solve(mp1)
#' 
#' plot(ms1, main = "Equal weights")
#' 
#' # create multi-objective problem using input matrix
#' mp2 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_weighted_sum_approach(
#'     matrix(runif(100), ncol = 2),
#'     verbose = TRUE
#'   ) %>%
#'   add_default_solver(verbose = FALSE)
#' 
#' # solve problem
#' ms2 <- solve(mp2)
#' 
#' # extract objective values and plot approximated pareto front
#' obj_mat <- attributes(ms2)$objective
#' plot(obj_mat, main = "Approximated pareto front", 
#' xlab = "Keystone objective (shortfall)", 
#' ylab = "Iconic objective (shortfall)")
#' }
#'
#' @export
add_weighted_sum_approach <- function(x, weights, verbose = TRUE) {
  # assert arguments are valid
  assert_required(x)
  assert_required(weights)
  assert_required(verbose)
  assert(
    is_multi_conservation_problem(x),
    is.numeric(weights),
    all_finite(weights),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  if (!is.matrix(weights)) {
    assert(number_of_problems(x) == length(weights))
    weights <- matrix(weights, nrow = 1)
  }

  if (length(as.matrix(weights)[1, ]) != (number_of_problems(x))) {
    msg <- ifelse(is.matrix(weights),
      cli::cli_abort(c(
        "The number of columns of {.arg weights} must be the same as the number of objectives.",
        "i" = "{.arg weights} has {length(as.matrix(weights)[1, ])} values.",
        "x" = "{.arg weights} must have {number_of_problems(x)} values."
      )),
      cli::cli_abort(c(
        "The length of {.arg weights} must be one less than the number of objectives.",
        "i" = "{.arg weights} has {length(as.matrix(weights)[1, ])} values.",
        "x" = "{.arg weights} must have {number_of_problems(x)} values."
      ))
    )
  }

  # rescale weights
  weights <-
    weights /
      matrix(
        rowSums(weights),
        nrow = nrow(weights), ncol = ncol(weights), byrow = FALSE
      )

  # add approach
  x$add_approach(
    R6::R6Class(
      "WeightedSumApproach",
      inherit = MultiObjApproach,
      public = list(
        name = "weighted sum approach",
        data = list(weights = weights, verbose = verbose),
        run = function(x, solver) {
          ## initialization
          weights <- self$get_data("weights")
          verbose <- self$get_data("verbose")
          sols <- vector(mode = "list", length = nrow(weights))
          ## preliminary calculations
          obj_signs <- matrix(
            ifelse(x$modelsense == "min", -1, 1),
            ncol = ncol(x$obj), nrow = nrow(x$obj), byrow = FALSE
          )
          ## if needed, set up progress bar
          if (isTRUE(verbose)) {
            cli::cli_inform(paste("Generating", nrow(weights), "solutions..."))
            pb <- cli::cli_progress_bar(
              "Generating solutions",
              total = nrow(weights),
              .envir = parent.frame() # can only get progress bar to work witht this
            )
          }
          ## set modelsense to max
          x$opt$set_modelsense("max")
          ## iterate over each combination of weights
          for (i in seq_len(nrow(weights))) {
            ### set model objective coefficients
            x$opt$set_obj(
              colSums(
                x$obj *
                  obj_signs *
                  matrix(
                    weights[i, ],
                    ncol = ncol(x$obj),
                    nrow = nrow(x$obj), byrow = FALSE
                  )
              )
            )
            ### solve problem
            sols[[i]] <- solver$solve(x$opt)
            ### compute and store objective values for each objective
            if (!is.null(sols[[i]]$x)) {
              sols[[i]]$objective <- stats::setNames(
                rowSums(
                  x$obj *
                    matrix(
                      sols[[i]]$x,
                      ncol = ncol(x$obj),
                      nrow = nrow(x$obj), byrow = TRUE
                    )
                ),
                rownames(x$obj)
              )
            }
            ## if possible, update the starting solution for the solver
            if (
              !is.null(solver$data) &&
                !is.null(sols[[i]]$x) &&
                isTRUE("start_solution" %in% names(solver$data))
            ) {
              solver$data$start_solution <- sols[[i]]$x
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
          ## return solutions
          sols
        }
      )
    )$new()
  )
}
