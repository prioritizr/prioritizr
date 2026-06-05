#' @include internal.R
NULL

#' Create relative tolerance values for a multi-objective approach
#'
#' Create multiple sets of relative tolerance values to generate multiple
#' solutions with the hierarchical approach for multi-objective optimization
#' (i.e., the `rel_tol` parameter of [add_hier_approach()]).
#'
#' @inheritParams approach_weights_matrix
#'
#' @param n_values `integer` denoting the number of relative tolerance values to
#' to generate for each [problem()] object (per `n_problems`), except for the
#' last [problem()].
#'
#' @param include_zeros `logical` value indicating if the relative tolerance
#' values should include zeros? If `include_zeros = TRUE`, then some of the sets
#' will assign a value of zero to some of the objectives, and so
#' solutions based on these sets will ensure optimality for the
#' objectives with zero values. Defaults to `TRUE`.
#'
#' @param max `numeric` positive value denoting the maximum relative
#' tolerance value. For example, a value of 0.2 means that
#' some of the resulting solutions could perform up to 20% worse than optimality
#' for particular objectives. Similarity, a value of 1.5 means that
#' some of the resulting solutions could perform up to 150% worse than
#' optimality for particular objectives.
#'
#' @param order `logical` value indicating if each set
#' returned relative tolerance values should only contain values in descending
#' order of priority. For example, if considering three problems, then a set of
#' `rel_tol = c(0.8, 0.2)` would have values in descending order of priority
#' and a set of `rel_tol = c(0.2, 0.8)` would not. If you want to generate
#' relative tolerance values to explore trade-offs assuming a particular order
#' of priority when using the hierarchical approach,
#' then we recommend using `order = TRUE`.
#' Defaults to `TRUE`.
#'
#' @return
#' A `numeric` matrix. Here, rows correspond to
#' different sets of each relative tolerance values and columns correspond to
#' different objectives.
#'
#' @inherit add_hier_approach examples
#'
#' @export
approach_rel_tol_matrix <- function(n_problems, n_values, max,
                                    include_zeros = TRUE,
                                    order = TRUE) {
  # assert arguments are valid
  assert_required(n_problems)
  assert_required(n_values)
  assert_required(max)
  assert_required(order)
  assert(
    assertthat::is.count(n_problems),
    assertthat::noNA(n_problems),
    n_problems >= 2,
    assertthat::is.count(n_values),
    assertthat::noNA(n_values),
    assertthat::is.flag(include_zeros),
    assertthat::noNA(include_zeros),
    assertthat::is.number(max),
    assertthat::noNA(max),
    all_positive(max),
    assertthat::is.flag(order),
    assertthat::noNA(order)
  )

  # create initial rel_tol values
  out <- lapply(
    rep(max, n_problems - 1),
    seq, from = 0, length.out = n_values + as.double(!include_zeros)
  )
  if (!isTRUE(include_zeros)) {
    out <- lapply(out, `[`, -1)
  }

  # generate matrix with all combinations of rel_tol values
  out <- as.matrix(do.call(expand.grid, args = out))
  colnames(out) <- NULL

  # if needed, exclude sets that are not in descending order of priority
  if (isTRUE(order) && (ncol(out) > 1L)) {
    idx <- rep(TRUE, nrow(out))
    for (i in seq(2, ncol(out))) {
      idx <- idx & (out[, i - 1] >= out[, i])
    }
    out <- out[idx, , drop = FALSE]
  }

  # return result
  out
}
