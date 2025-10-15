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
#' TODO.
#'
#' @return
#' TODO.
#'
#' @seealso
#' See [approaches] for an overview of all functions for adding an approach.
#'
#' @family approaches
#'
#' @examples
#' \dontrun{
#' # TODO
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
  assert(number_of_problems(x) == ncol(weights))

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
      "WeightedSumAppraoch",
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
              "Generating solutions", total = nrow(weights)
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
                  weights[i, ], ncol = ncol(x$obj),
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
                    sols[[i]]$x, ncol = ncol(x$obj),
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
