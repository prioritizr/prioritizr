#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Add targets following Jung *et al.* (2021)
#'
#' Add targets to a conservation planning problem
#' following Jung *et al.* (2021).
#' Briefly, this method involves setting targets based the criteria
#' for recognizing Vulnerable species by the International Union for the
#' Conservation of Nature (IUCN) Red List of Threatened Species (IUCN 2025).
#' To help prevent widespread features from obscuring priorities,
#' targets are capped following Butchart *et al.* (2015).
#' This method was designed for global-scale prioritizations.
#'
#' @inheritParams add_manual_targets
#' @inheritParams jung_targets
#'
#' @inheritSection jung_targets Mathematical formulation
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit jung_targets details references
#' @inherit add_relative_targets return seealso
#'
#' @family targets
#'
#' @examples
#' #TODO
#'
#' @name add_jung_targets
NULL

#' @rdname add_jung_targets
#' @export
add_jung_targets <- function(x, status = "VU", prop_uplift = 0.1,
                             cap_threshold = 1000000) {
  # add targets
  internal_add_auto_targets.Method(
    x,
    method = jung_targets(
      status = status,
      prop_uplift = prop_uplift,
      cap_threshold = cap_threshold
    )
  )
}
