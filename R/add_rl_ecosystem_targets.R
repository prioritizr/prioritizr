#' @include internal.R ConservationProblem-class.R
NULL

#' Add targets based on the IUCN Red List of Ecosystems
#'
#' Add targets to a conservation planning problem based on criteria from the
#' International Union for the Conservation of Nature (IUCN) Red List of
#' Ecosystems (IUCN 2024).
#' Briefly, this method can be used to set targets based on
#' criteria pertaining to current size of geographic distribution
#' (criterion B) and reduction in geographic distribution (criterion A).
#' To help prevent widespread features from obscuring priorities for
#' rare features, targets are capped following Butchart *et al.* (2015).
#' This method may be suitable for ecosystem protection at global and
#' and national scales.
#'
#' @inheritParams add_manual_targets
#' @inheritParams rl_ecosystem_targets
#'
#' @inheritSection rl_ecosystem_targets Mathematical formulation
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit rl_ecosystem_targets details references
#' @inherit add_relative_targets return seealso
#'
#' @family targets
#'
#' @examples
#' #TODO
#'
#' @name add_rl_ecosystem_targets
NULL

#' @rdname add_rl_ecosystem_targets
#' @export
add_rl_ecosystem_targets <- function(x, status, criterion_a, criterion_b,
                                     prop_uplift = 0, method = "max",
                                     cap_threshold = 1000000) {
  # add targets
  internal_add_auto_targets.Method(
    x,
    method = rl_ecosystem_targets(
      status = status,
      criterion_a = criterion_a,
      criterion_b = criterion_b,
      prop_uplift = prop_uplift,
      method = method,
      cap_threshold = cap_threshold
    )
  )
}
