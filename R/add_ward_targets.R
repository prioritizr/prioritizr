#' @include internal.R ConservationProblem-class.R
NULL

#' Add targets following Ward *et al.* (2025)
#'
#' Add targets to a conservation planning problem
#' following Ward *et al.* (2025).
#' Briefly, this method involves setting targets based the criteria
#' for recognizing Critically Endangered species by the International Union for
#' the Conservation of Nature (IUCN) Red List of Threatened Species (IUCN 2025).
#' To help prevent widespread features from obscuring priorities,
#' targets are capped following Butchart *et al.* (2015).
#' This method was designed for species protection at national-scales.
#'
#' @inheritParams add_manual_targets
#' @inheritParams add_ward_targets
#'
#' @inheritSection ward_targets Mathematical formulation
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit ward_targets details references
#' @inherit add_relative_targets return seealso
#'
#' @family targets
#'
#' @examples
#' #TODO
#'
#' @name add_ward_targets
NULL

#' @rdname add_ward_targets
#' @export
add_ward_targets <- function(x, status = "CR", cap_threshold = 1000000) {
  # return method
  method(
    name = "Ward et al. (2025) targets",
    type = "absolute",
    fun = internal_rl_species_targets,
    args = list(
      status = status,
      criterion_a = "A2",
      criterion_b = "B1",
      prop_uplift = 0,
      method = "max",
      cap_threshold = cap_threshold
    )
  )
}
