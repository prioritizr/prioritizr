#' @include internal.R
NULL

#' Create objective weights
#'
#' Create multiple sets of weight values to generate multiple solutions with
#' the weighted sum approach for multi-objective optimization
#' (i.e., the `weights` parameter of [add_wtd_sum_approach()]).
#'
#' @param n_objectives `integer` number of objectives.
#'
#' @param n_per_objective `integer` number of weight values to
#' to generate for each objective.
#'
#' @param include_zeros `logical` value indicating if the weight values
#' should include zeros? If `include_zeros = TRUE`, then some of the sets
#' will assign a weight of zero to some of the objectives, and so
#' solutions based on these sets will be influenced by only some of the
#' objectives (i.e., those with non-zero weight values).
#' Defaults to `TRUE`.
#'
#' @param include_extremes `logical` value indicating if
#' the sets of weight values should combinations of weight values
#' that consider only a single objective?
#' If `include_extremes = TRUE`, then some of the sets will
#' contain zeros for all objectives except a single objective.
#' Defaults to `TRUE`.
#'
#' @return
#' A `numeric` matrix with weight values. Here, rows correspond to
#' different sets of each weight values and columns correspond to different
#' objectives.
#'
#' @inherit add_wtd_sum_approach examples
#'
#' @export
objective_weights_matrix <- function(n_objectives, n_per_objective,
                                    include_zeros = TRUE,
                                    include_extremes = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.count(n_objectives),
    assertthat::noNA(n_objectives),
    assertthat::is.count(n_per_objective),
    assertthat::noNA(n_per_objective),
    assertthat::is.flag(include_zeros),
    assertthat::noNA(include_zeros),
    assertthat::is.flag(include_extremes),
    assertthat::noNA(include_extremes)
  )

  # create initial weight values
  out <- seq(0, 1, length.out = n_per_objective + as.double(include_zeros))
  if (!isTRUE(include_zeros)) {
    out <- out[-1]
  }
  out <- list(out)[rep(1, n_objectives)]

  # generate matrix with all combinations of weight values
  out <- as.matrix(do.call(expand.grid, args = out))

  # remove rows where all weight values are the same
  keep <- apply(out, 1, function(z) length(unique(z)) >= 2)
  out <- out[keep, , drop = FALSE]

  # remove rows where all weight values are zero except one
  keep <- rowSums(out > 1e-6) >= 2
  out <- out[keep, , drop = FALSE]

  # manually add rows for extreme points
  if (isTRUE(include_extremes)) {
    out <- rbind(diag(n_objectives), out)
  }

  # manually add in a row where each objective is assigned equal weighting
  out <- rbind(matrix(1, nrow = 1, ncol = n_objectives), out)

  # return result
  out
}
