#' @include internal.R
NULL

#' Create objective relative tolerance values
#'
#' Create multiple sets of relative tolerance values to generate multiple
#' solutions with the hierachical approach for multi-objective optimization
#' (i.e., the `rel_tol` parameter of [add_hier_approach()]).
#'
#' @inheritParams objective_weights_matrix
#'
#' @param n_per_objective `integer` number of relative tolerance values to
#' to generate for each objective.
#'
#' @param include_zeros `logical` value indicating if the relative tolerance
#' values should include zeros? If `include_zeros = TRUE`, then some of the sets
#' will assign a value of zero to some of the objectives, and so
#' solutions based on these sets will ensure optimality for the
#' objectives with zero values. Defaults to `TRUE`.
#'
#' @param max_rel_tol `numeric` positive value denoting the maximum relative
#' tolerance value. For example, a value of 0.2 means that
#' some of the resulting solutions could perform up to 20% worse than optimality
#' for particular objectives. Similarity, a value of 1.5 means that
#' some of the resulting solutions could perform up to 150% worse than
#' optimality for particular objectives.
#'
#' @return
#' A `numeric` matrix. Here, rows correspond to
#' different sets of each relative tolerance values and columns correspond to
#' different objectives.
#'
#' @inherit add_hier_approach examples
#'
#' @export
objective_rel_tol_matrix <- function(n_objectives, n_per_objective,
                                     max_rel_tol,
                                     include_zeros = TRUE) {
  # assert arguments are valid
  assert_required(n_objectives)
  assert_required(n_per_objective)
  assert_required(max_rel_tol)
  assert(
    assertthat::is.count(n_objectives),
    assertthat::noNA(n_objectives),
    n_objectives >= 2,
    assertthat::is.count(n_per_objective),
    assertthat::noNA(n_per_objective),
    assertthat::is.flag(include_zeros),
    assertthat::noNA(include_zeros),
    assertthat::is.number(max_rel_tol),
    assertthat::noNA(max_rel_tol),
    all_positive(max_rel_tol)
  )

  # create initial rel_tol values
  out <- lapply(
    rep(max_rel_tol, n_objectives - 1),
    seq, from = 0, length.out = n_per_objective + as.double(!include_zeros)
  )
  if (!isTRUE(include_zeros)) {
    out <- lapply(out, `[`, -1)
  }

  # generate matrix with all combinations of weight values
  out <- as.matrix(do.call(expand.grid, args = out))
  colnames(out) <- NULL

  # return result
  out
}
