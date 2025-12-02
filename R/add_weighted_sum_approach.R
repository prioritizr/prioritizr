#' @include internal.R MultiObjApproach-class.R
NULL

#' Add a weighted sum approach
#'
#' Add a weighted sum approach for multi-objective optimization to a
#' conservation planning problem.
#'
#' @param x [multi_problem()] object.
#'
#' @param weights `numeric` vector containing the weights for each
#' objective. To generate multiple solutions based on different
#' combinations of weights, `weights` can be a `numeric` matrix where
#' each row corresponds to a different solution and each columns
#' corresponds to a different objective.
#'
#' @param verbose `logical` should progress on generating solutions
#' displayed? Defaults to `TRUE`.
#'
#' @details
#' The weighted sum approach is a method for solving 
#' multi-objective optimization problems by transforming several
#' objectives into a single objective. This is done by taking a 
#' weighted linear combination of the objective expressions from each 
#' problem in the `multi_problem()` object.
#' 
#' Specifically, if a multi-objective problem contains \(k\) objectives
#' with values \(y_1, y_2, \ldots, y_k\) corresponding to each objective, and the
#' user supplies a weight vector \(w = (w_1, \ldots, w_k)\), then the
#' transformed optimization problem minimizes the function:
#'
#' \deqn{F = \sum_{i=1}^{k} w_i \, y_i}
#' 
#' The weights determine the relative importance of the objectives and
#' can be used to explore trade-offs among conservation, cost, and other
#' planning outcomes.  
#' 
#' When `weights` is a matrix, each row is interpreted as an independent
#' weighting scheme and columns are the individual objectives. Thehe approach 
#' stores a separate solution for each resulting weighted-sum problem. 
#' These will be solved sequentially when `solve()`.
#' 
#' This approach is suitable when objectives can be meaningfully combined
#' after weighting, and when there is no clear priority that needs to be 
#' defined with [add_hierarchical_approach()].
#'
#' @return
#' A modified `multi_problem()` object with the weighted-sum approach
#' added. This function does not solve the problem; it only records the
#' approach so that the problem can be compiled and solved later. After calling 
#' [solve()], the output will either be an individual solution or a list of 
#' solutions if a matrix was supplied for `weights`. 
#'
#' @seealso
#' See [approaches] for an overview of all functions for adding an approach.
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
#' mp1 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>% 
#'   add_weighted_sum_approach(c(0.5, 0.5), verbose = TRUE) %>% 
#'   add_default_solver()
#' 
#' # solve problem
#' ms1 <- solve(mp1)
#' 
#' plot(ms1)
#' 
#' # create multi-objective problem using input matrix
#' mp2 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_weighted_sum_approach(
#'     matrix(c( 
#'       0.5, 0.5, # balanced
#'       1.0, 0.0, # all in on keystone
#'       0.0, 1.0 # all in on iconic
#'     ), ncol = 2, byrow = TRUE),
#'     verbose = TRUE
#'   ) %>%
#'   add_default_solver(verbose = FALSE)
#' 
#' # solve problem
#' ms2 <- solve(mp2)
#' 
#' # extract objective values and plot approximated pareto front (very few weight values)
#' obj_mat <- attributes(ms2)$objective
#' plot(obj_mat)
#' 
#' # create multi-objective problem using input matrix
#' mp3 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_weighted_sum_approach(
#'     matrix(runif(50), ncol = 2),
#'     verbose = TRUE
#'   ) %>%
#'   add_default_solver(verbose = FALSE)
#' 
#' # solve problem
#' ms3 <- solve(mp3)
#' 
#' # extract objective values and plot approximated pareto front (more weight values)
#' obj_mat <- attributes(ms3)$objective
#' plot(obj_mat)
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
