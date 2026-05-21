#' @include internal.R MultiObjApproach-class.R
NULL

#' Add a weighted sum approach
#'
#' Add a weighted sum approach for multi-objective optimization to a
#' multi-objective conservation planning problem (Jaimes *et al.* 2009).
#' Broadly speaking, this approach involves combining each [problem()] in a
#' [multi_problem()] object together based on weights, wherein
#' those associated with a greater weight value exert a greater influence
#' on the optimization process.
#'
#' @inheritParams add_hier_approach
#'
#' @param weights `numeric` vector or matrix containing the weights for each
#' [problem()] in `x`. A vector
#' can be used to specify a set of values for generating a single solution,
#' wherein each value corresponds to a different [problem()] in `x`.
#' Alternatively, a matrix can be used to specify multiple sets of values for
#' generating multiple solutions, wherein each column corresponds to a different
#' [problem()] in `x` and each row corresponds to a different solution.
#' With the `weights` values, greater values indicate greater importance.
#' Also, a `weights` value of 0 means that a particular [problem()] in `x`
#' has no influence over the optimization process.
#'
#' @details
#' This multi-objective optimization approach is most useful when considering
#' a small number of objectives that have the same units (e.g., they have the
#' same objective function and similar cost and feature data)
#' (Neubert *et al.* 2025).
#' Briefly, this approach involves transforming multiple
#' objectives into a new single objective -- based on a weighted
#' linear combination -- and then generating a solution based on this new
#' objective.
#' Although this approach has widespread usage (Williams and Kendall 2017),
#' small differences in the weight values can cause unexpectedly large
#' differences to solutions (Das and Dennis 1997).
#' This is because -- when using this approach -- the overall influence that an
#' objective has on a solution depends on its weight value and also the
#' scale (in other words, range) of the metric used to evaluate how well
#' a solution achieves the objective (termed objective value).
#' For example, the minimum shortfall objective function
#' ([add_min_shortfall_objective()]) often has relatively small
#' objective values (e.g., values may range between zero and the number of
#' features), and the minimum set objective function
#' ([add_min_set_objective()] can have much higher values depending
#' on the cost data (e.g., values may range between zero and 10,000 depending
#' on the cost data). Due to these differences in scale,
#' a solution generated with these two objectives and equal weight values
#' will likely fail to balance them equally. As such, when using the weighted
#' sum approach, practitioners may need to (i) consider a large number of
#' sets of weights to obtain a diverse set of solutions and (ii) perform
#' multiple calibration procedures to manually identify weight parameter values
#' that result in different solutions.

#' @section Mathematical formulation:
#' This approach can be expressed mathematically for a set of
#' objectives associated with the [problem()] objects in `x`.
#' Let \eqn{O}{O} denote the set of objectives (indexed by \eqn{o}{o}).
#' For brevity, we will assume that all of the objectives should ideally be
#' maximized.
#' Also, let \eqn{f_o(x)}{fo(x)} denote the objective function for each
#' objective \eqn{o \in O}{o in O}, where \eqn{x} represents all the decision
#' variables for calculating the objective values (e.g., planning unit selection
#' status values).
#' Additionally, let \eqn{w_o}{wo} denote the weight (per
#' `weights`) parameter for each objective \eqn{o \in O}{o in O}.
#' Furthermore, let \eqn{S}{S} represent the set (region) of feasible
#' values for \eqn{x} based on the constraints for all of the objectives
#' (e.g., if the first problem in `x` has locked in constraints and the
#' second problem has locked out constraints, then \eqn{S}{S} would
#' account for both the locked in and locked out constraints).
#' Given this terminology, the approach involves solving the following
#' optimization problem.
#'
#' \deqn{
#' \mathit{Maximize} \space
#' \sum_{o \in O} \frac{w_o}{\sum_{o \in O} w_o} \times f_o(x) \\
#' \mathit{subject \space to \space} x \in S
#' }{
#' Maximize sum_o^O (wo / sum_o^O wo) * fo(x), subject to x in S
#' }
#'
#' By specifying the relative importance of each objective through a particular
#' choice of weights, the optimization process can identify a solution
#' that achieves multiple objectives.
#'
#' @inherit add_hier_approach return seealso
#'
#' @seealso
#' See [approaches] for an overview of all functions for adding an approach.
#' Also, see [objective_weights_matrix()] to automatically create a matrix
#' for `weights`.
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
#' Neubert S, McGowan J, Metcalfe K, Hanson JO, Buenafe KCV, Dabalà A, Dunn DC,
#' Everett JD, Possingham HP, Stelzenmüller V, Estep A, Ervin J, and
#' Richardson AJ (2025) Multiple-use spatial planning for sustainable
#' development and conservation. _Trends in Ecology and Evolution_, **40**:
#' 1126--1142.
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
#' # now create multi-objective problem with equal weights for the objectives
#' mp1 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_wtd_sum_approach(c(0.5, 0.5), verbose = TRUE) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' ms1 <- solve(mp1)
#'
#' # plot solution to multi-objective problem
#' plot(ms1, main = "Equal weights", axes = FALSE)
#'
#' # we will now generate multiple solutions based on a matrix
#' # that contains different combinations of weight values
#'
#' # create a matrix with weight values for objectives
#' obj_weights_matrix <- objective_weights_matrix(
#'  n_objectives = 2,
#'  n_per_objective = 5,
#'  include_zero = TRUE
#' )
#'
#' # preview weight matrix
#' head(obj_weights_matrix)
#'
#' # create multi-objective problem using weight matrix
#' mp2 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_wtd_sum_approach(obj_weights_matrix, verbose = FALSE) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve multi-objective problem and generate multiple solutions
#' ms2 <- solve(mp2)
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
#'
#' # we can see that there are multiple solutions (points) that have
#' # exactly the same performance for the two objectives (these appear
#' # as points with slightly thicker borders), and this is a key limitation
#' # of the weighted sum approach
#' }
#' @export
add_wtd_sum_approach <- function(x, weights, verbose = TRUE) {
  # assert arguments are valid
  assert_required(x)
  assert_required(weights)
  assert_required(verbose)
  assert(
    is_multi_conservation_problem(x),
    is.numeric(weights),
    all_positive(weights),
    all_finite(weights),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

  # additional checks for weights
  if (is.matrix(weights)) {
    ## if weights is a matrix,then perform additional checks
    assert(
      identical(number_of_problems(x), ncol(weights)),
      msg = c(
        "{.arg weights} must have a column for each problem in {.arg x}.",
        "i" = "{.arg x} has {number_of_problems(x)} problem{?s}.",
        "x" = "{.arg weights} has {ncol(weights)} column{?s}."
      )
    )
    assert(nrow(weights) > 0)
  } else {
    ## if weights is a vector,then perform additional checks
    assert(
      identical(number_of_problems(x), length(weights)),
      msg = c(
        "{.arg weights} must have a value for each problem in {.arg x}.",
        "i" = "{.arg x} has {number_of_problems(x)} problem{?s}.",
        "x" = "{.arg weights} has {length(weights)} value{?s}."
      )
    )

    ## standardize weight format
    weights <- matrix(weights, nrow = 1)
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
            pb <- cli::cli_progress_bar(
              total = nrow(weights),
              .envir = parent.frame()
            )
          }

          ## set modelsense to max
          x$opt$set_modelsense("max")

          ## iterate over each combination of weights
          for (i in seq_len(nrow(weights))) {
            ## set model objective coefficients
            x$opt$set_obj(
              colSums(
                x$obj *
                  obj_signs *
                  matrix(
                    weights[i, ], byrow = FALSE,
                    ncol = ncol(x$obj), nrow = nrow(x$obj)
                  )
              )
            )

            ## solve problem
            sols[[i]] <- solver$solve(x$opt)

            ## compute and store objective values for each objective
            if (!is.null(sols[[i]]$x)) {
              sols[[i]]$objective <- stats::setNames(
                rowSums(
                  x$obj *
                    matrix(
                      sols[[i]]$x, byrow = TRUE,
                      ncol = ncol(x$obj), nrow = nrow(x$obj)
                    )
                ),
                rownames(x$obj)
              )
            }

            ## if needed, update starting solution
            if (!identical(i, nrow(weights))) {
              ### identify starting solution for next run
              j <- which_least_different(
                weights[seq_len(i), , drop = FALSE],
                weights[i + 1, , drop = FALSE]
              )
              ### update the starting solution for the solver
              solver$set_start_solution(sols[[j]]$x, warn = FALSE)
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
