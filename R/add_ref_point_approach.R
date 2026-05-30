#' @include internal.R MultiObjApproach-class.R
NULL

#' Add a reference point approach
#'
#' Add a reference point approach for multi-objective optimization to a
#' multi-objective conservation planning problem (Wierzbicki 1980, Jaimes 2009).
#' Broadly speaking, this approach considers a set of (i) reference point
#' parameters that specify an aspirational level of achievement for each
#' objective and (ii) weight parameters that specify the relative importance
#' for reaching the reference point for each objective.
#' To ensure that solutions are not biased
#' by differences in scale among the objectives, this approach
#' also considers the best and worst possible objective values for each
#' objective.
#'
#' @inheritParams add_wtd_sum_approach
#'
#' @param weights `numeric` vector containing the weights for each
#' objective. To generate multiple solutions based on different values,
#' `weights` can be a `numeric` matrix where
#' each row corresponds to a different solution and each column
#' corresponds to a different objective.
#' Defaults to `NULL` such that weights are automatically calculated
#' to equally balance all objectives (i.e., equivalent to a `numeric`
#' vector containing a value of 1 for each objective).
#'
#' @param ref_points `numeric` vector containing values that denote the
#' reference points. These points represent aspirational goals for each
#' objective. To generate multiple solutions based on different values,
#' `ref_points` can be a `numeric` matrix where
#' each row corresponds to a different solution and each columns
#' corresponds to a different objective.
#' Note that all values must be greater than zero.
#' Defaults to `NULL` such that reference points are automatically
#' calculated based on the best possible objective value for each objective.
#'
#' @param worst `numeric` vector containing objective values that denote
#' the worst possible performance for each objective.
#' Note that  values must follow the same order as the problems in `x`.
#' Defaults to `NULL` such that these values are computed automatically.
#'
#' @param best `numeric` vector containing objective values that denote
#' the best possible performance for each objective.
#' Note that  values must follow the same order as the problems in `x`.
#' Defaults to `NULL` such that these values are computed automatically.
#'
#' @param rescale `logical` indicating if `weights` should be
#' normalized based on the best and worst objective values
#' (per `best` and `worst`, respectively). This is important
#' to ensure that the optimization process is not biased by differences
#' in scale between different objectives. Defaults to `TRUE`.
#'
#' @details
#' The reference point approach for multi-objective optimization involves
#' creating a new objective that is calculated based on multiple objectives.
#' In particular, the new objective uses weights to specify the relative
#' importance of each individual objective, and reference points to specify
#' a desirable threshold level of performance for each objective
#' (conceptually similar to target thresholds used in conservation planning).
#' Given this, the reference point approach first involves
#' calculate the weighted shortfall for each objective
#' (i.e., difference between the reference point and the objective value
#' for a candidate solution, multiplied by the weight). It then
#' involves maximizing the maximum value of the weighted shortfalls,
#' and then subsequently minimizing the sum of the weighted shortfalls.
#'
#' To describe this approach mathematically, we will define the
#' following terminology.
#' Although this approach can support both maximization and minimization
#' objectives, we will assume that all objectives should
#' be maximized for brevity.
#' Let \eqn{O} denote the set of objectives (indexed by \eqn{o}).
#' For each objective, let \eqn{w_o}{wo} denote the weight for each objective
#' \eqn{o \in O}{o in O} (per `weights`),
#' \eqn{r_o}{ro} denote the reference point for each objective
#' \eqn{o \in O}{o in O} (per `ref_points`),
#' \eqn{b_o}{bo} denote the best objective value for each objective
#' (per `best`),
#' \eqn{c_o}{co} denote the worst objective value for each objective
#' (per `worst`),
#' \eqn{s_o}{so} denote a scaling term for each objective
#' (see below for details),
#' and \eqn{v_o}{vo} denote the objective value
#' for a candidate solution as measured based on each objective
#' \eqn{o \in O}{o in O}.
#' After defining these terms, the approach
#' is formulated with the following equation.
#'
#' \deqn{
#' \mathrm{Minimize} \space \max_{o \in O} w_o \times s_o \times \max(r_o - v_o, 0), \\
#' \mathrm{Minimize} \space \sum_{o \in O} w_o \times s_o \times \max(r_o - v_o, 0)
#' }{
#' Minimize max o^O wo * so * max(ro - vo, 0)
#' Minimize sum o^O wo * so * max(ro - vo, 0)
#' }
#'
#' If the weights should be normalized (per `rescale = TRUE`), then
#' the scaling term for each objective is calculated
#' with the following equation.
#'
#' \deqn{
#' so = \frac{1}{\|bo - co\|}
#' }{
#' so = 1/(abs(bo - co))
#' }
#'
#' Conversely, if the weights should not be normalized
#' (per `rescale = FALSE`), then \eqn{s_o}{so}
#' is set to a value of 1 for each objective.
#'
#' @inherit add_wtd_sum_approach return seealso
#'
#' @family approaches
#'
#' @references
#' Jaimes AL, Saúl ZM, and Coello Coello CA (2009) *An introduction to
#' multiobjective optimization techniques* in Optimization in Polymer
#' Processing. Eds Gaspar-Cunha A and Covas JA. Nova Science Publishers Inc,
#' New York, United States.
#'
#' Wierzbicki AP (1980) *The use of reference objectives in multiobjective
#' optimization* in Multiple criteria decision making theory and application.
#' Eds Fandel G and Gal T. Lecture notes in economics and mathematical
#' systems (pp. 468--486). Springer Berlin Heidelberg.
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
#' # now create multi-objective problem with reference point approach,
#' # with settings to automatically identify an equally balanced solution
#' mp1 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_ref_point_approach(verbose = TRUE) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' ms1 <- solve(mp1)
#'
#' # plot solution to multi-objective problem
#' plot(ms1, main = "Equally balanced", axes = FALSE)
#'
#' # we will now generate multiple solutions based on a matrix
#' # that contains different combinations of weight values
#'
#' # create a matrix with weight values for objectives
#' weights_matrix <- approach_weights_matrix(
#'   n_problems = 2, n_values = 5, include_zero = TRUE
#' )
#'
#' # preview weight matrix
#' head(weights_matrix)
#'
#' # now create multi-objective problem with reference point approach,
#' # with weights to generate multiple solutions
#' mp2 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_ref_point_approach(weights = weights_matrix, verbose = TRUE) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
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
add_ref_point_approach <- function(x,
                                   weights = NULL,
                                   ref_points = NULL,
                                   best = NULL,
                                   worst = NULL,
                                   rescale = TRUE,
                                   verbose = TRUE) {
  # assert arguments are valid
  assert(
    is_multi_conservation_problem(x),
    assertthat::is.flag(rescale),
    assertthat::noNA(rescale),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  ## standardize arguments
  if (is.numeric(weights) && !is.matrix(weights)) {
    assert(is_match_of(length(weights), number_of_problems(x)))
    weights <- matrix(weights, nrow = 1)
  }
  if (is.numeric(ref_points) && !is.matrix(ref_points)) {
    assert(is_match_of(length(ref_points), number_of_problems(x)))
    ref_points <- matrix(
      ref_points,
      nrow = nrow(weights), ncol = ncol(weights), byrow = TRUE
    )
  }
  ## additional argument checks
  if (!is.null(weights)) {
    assert(
      is.matrix(weights),
      is_match_of(ncol(weights), number_of_problems(x)),
      nrow(weights) >= 1,
      all_finite(weights),
      all_positive(weights)
    )
  }
  if (!is.null(weights) && isTRUE(rescale)) {
    assert(
      all_proportion(weights),
      msg = c(
        "!" =
          "{.arg weights} must have values between {.val {0}} and {.val {1}}.",
        "x" = "This is because {.arg rescale} is {.val {TRUE}}."
      )
    )
  }
  if (!is.null(ref_points)) {
    assert(
      is.matrix(ref_points),
      is_match_of(ncol(ref_points), number_of_problems(x)),
      nrow(ref_points) >= 1,
      all_finite(ref_points)
    )
  }
  if (!is.null(ref_points) && !is.null(weights)) {
    assert(is_match_of(nrow(ref_points), nrow(weights)))
  }
  if (!is.null(best)) {
    assert(
      is.numeric(best),
      all_finite(best),
      is_match_of(length(best), number_of_problems(x))
    )
  }
  if (!is.null(worst)) {
    assert(
      is.numeric(worst),
      all_finite(worst),
      is_match_of(length(worst), number_of_problems(x))
    )
  }
  # add approach
  x$add_approach(
    R6::R6Class(
      "ReferencePointApproach",
      inherit = MultiObjApproach,
      public = list(
        name = "reference point approach",
        data = list(
          weights = weights,
          ref_points = ref_points,
          best = best,
          worst = worst,
          rescale = rescale,
          verbose = verbose
        ),
        calculate = function(x, y) {
          ## assert valid arguments
          assertthat::assert_that(
           inherits(x, "list"),
           is_multi_conservation_problem(y)
          )
          ## initialization
          n <- number_of_problems(y)
          ref_points <- self$get_data("ref_points")
          worst <- self$get_data("worst")
          best <- self$get_data("best")
          rescale <- self$get_data("rescale")
          ## if needed, calculate best objective value
          if (
            (is.null(best) && isTRUE(rescale)) ||
            (is.null(best) && is.null(ref_points))
          ) {
            best <- vapply(
              seq_len(n),
              FUN.VALUE = numeric(1),
              function(i) {
                ### set problem with i'th objective and modelsense
                x$opt$set_modelsense(x$modelsense[[i]])
                x$opt$set_obj(x$obj[i, ])
                ### generate solution
                s <- y$solver$solve(x$opt)
                assert(
                  is_valid_raw_solution(s, multiple = FALSE),
                  call = rlang::expr(solve())
                )
                ### calculate objective value
                sum(s$x * x$obj[i, ])
              }
            )
          }
          ## if needed, calculate worst objective value
          if (is.null(worst) && isTRUE(rescale)) {
            worst <- vapply(
              seq_len(n),
              FUN.VALUE = numeric(1),
              function(i) {
                ### set problem with i'th objective and opposite modelsense
                x$opt$set_modelsense(
                  ifelse(x$modelsense[[i]] == "min", "max", "min")
                )
                x$opt$set_obj(x$obj[i, ])
                ### generate solution
                s <- y$solver$solve(x$opt)
                assert(
                  is_valid_raw_solution(s, multiple = FALSE),
                  call = rlang::expr(solve())
                )
                ### calculate objective value
                sum(s$x * x$obj[i, ])
              }
            )
          }
          ## store values
          self$set_internal("worst", worst)
          self$set_internal("best", best)
        },
        run = function(x, solver) {
          ## initialization
          weights <- self$get_data("weights")
          ref_points <- self$get_data("ref_points")
          rescale <- self$get_data("rescale")
          best <- self$get_internal("best")
          worst <- self$get_internal("worst")
          verbose <- self$get_data("verbose")
          ## if weights is NULL, then set as equal weights
          if (is.null(weights)) {
            weights <- matrix(1, ncol = length(x$modelsense), nrow = 1)
          }
          ## if ref_points is NULL, then set based on best
          if (is.null(ref_points)) {
            ref_points <- matrix(
              best,
              nrow = nrow(weights), ncol = ncol(weights), byrow = TRUE
            )
          }
          ## if needed, calculate normalized weights
          if (isTRUE(rescale)) {
            weights <-
              weights *
              matrix(
                1 / abs(best - worst),
                ncol = ncol(weights), nrow = nrow(weights), byrow = TRUE
              )
          }
          ## assert that arguments are valid
          assert(
            is.matrix(ref_points),
            is.matrix(weights),
            all_finite(ref_points),
            all_finite(weights),
            ncol(ref_points) == ncol(weights),
            nrow(ref_points) == nrow(weights),
            .internal = TRUE
          )
          ## ensure that reference points are between best and worst bounds
          {for (i in seq_len(ncol(weights))) {
            if (identical(x$modelsense[[i]], "max")) {
              ref_points[, i] <- pmax(
                pmin(ref_points[, i], best[[i]]),
                worst[[i]]
              )
            } else {
              ref_points[, i] <- pmin(
                pmax(ref_points[, i], best[[i]]),
                worst[[i]]
              )
            }
          }}
          ## if needed, set up progress bar
          if (isTRUE(verbose)) {
            pb <- cli::cli_progress_bar(
              "Generating solutions", total = nrow(weights)
            )
          }
          ## calculate shortfall bounds
          sh_ub <- rep(Inf, ncol(weights))
          if (is.numeric(best) & is.numeric(worst)) {
            sh_ub <- abs(best - worst)
          }
          ## main processing
          sols <- vector(mode = "list", length = nrow(weights))
          for (i in seq_len(nrow(weights))) {
            ### convert multi-objective optimization problem to
            ### follow the reference point method
            mo <- x$opt$copy()
            mo <- rcpp_convert_ref_point_method(
              mo$ptr, x$modelsense, x$obj,
              weights[i, ], ref_points[i, ],
              sh_ub
            )
            mo$opt <- OptimizationProblem$new(ptr = mo$opt)
            ### optimize reference point objectives with
            ### hierarchical optimization
            s <- solver$solve_multiobj(
              mo, priority = c(2, 1), rel_tol = 0
            )
            ### validate solution
            assert(
              is_valid_raw_solution(s, multiple = FALSE),
              call = rlang::expr(solve())
            )
            ### compute and store objective values for each objective
            s$objective <- stats::setNames(
              rowSums(
                x$obj *
                  matrix(
                    s$x[seq_len(ncol(x$obj))], byrow = TRUE,
                    ncol = ncol(x$obj), nrow = nrow(x$obj)
                  )
              ),
              rownames(x$obj)
            )
            ### store solution
            sols[[i]] <- s
            ### if needed, update starting solution
            if (!identical(i, nrow(weights))) {
              ### identify starting solution for next run
              j <- which_least_different(
                weights[seq_len(i), , drop = FALSE],
                weights[i + 1, , drop = FALSE]
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
          ## return solutions
          sols
        }
      )
    )$new()
  )
}
