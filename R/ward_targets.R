#' Add targets following Ward *et al.* (2025)
#'
#' Specify targets following Ward *et al.* (2025).
#' This function is designed to be used within `add_auto_targets()` and
#' `add_group_targets()`.
#'
#' @param status `character` value denoting the IUCN Red List threat
#' status used for target setting.
#' Available options include `"CR"` (Critically Endangered) ,
#' `"EN"` (Endangered), and "VU"` (Vulnerable).
#' Defaults to `"CR"`.
#'
#' @inheritParams rodrigues_targets
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on assessment
#' criteria from the IUCN Red List (IUCN 2025).
#' It is based on the idea that a protected area system should ideally
#' safeguard enough of a species' distribution to ensure that a species
#' would, at worst, be classified under a particular threat status.
#' In particular, this method considers criteria related to
#' the size of a species' spatial distribution (i.e., Criterion B)
#' and population size reduction (i.e., Criterion A).
#' By default, it considers criteria for the Critically Endangered threat status
#' and involves setting the target threshold for a species
#' as 100,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} (per subcriterion B1)
#' or 20% (per subcriterion A2) of its spatial distribution
#' (which ever value is larger).
#' Additionally, following Butchart *et al.* (2015), a cap of 1,000,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} is applied to target
#' thresholds.
#' By helping to ensure that species would -- at a minimum -- meet criteria
#' for being recognized as Critically Endangered, this method aims to reduce
#' chances that species will become extinct.
#'
#' @details
#' This target setting method was designed to protect species in
#' a national-scale prioritizations (Ward *et al.* 2025).
#' Since it was designed for national-scale prioritizations,
#' it may fail to identify meaningful priorities for
#' prioritizations conducted at smaller geographic scales
#' (e.g., national, state-level or county scales).
#' For example, if this method is applied to
#' smaller geographic scales, then the resulting prioritizations
#' may select an overly large percentage of the study area,
#' or be biased towards over-representing common and widespread species.
#' As such, if you are working at smaller scales, it is recommended to set
#' thresholds based on that criteria are appropriate to the spatial extent
#' of the planning region.
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#' @inherit jung_targets seealso return
#'
#' @family methods
#'
#' @references
#' Butchart SHM, Clarke M, Smith RJ, Sykes RE, Scharlemann JPW, Harfoot M,
#' Buchanan GM, Angulo A, Balmford A, Bertzky B, Brooks TM, Carpenter KE,
#' Comeros‐Raynal MT, Cornell J, Ficetola GF, Fishpool LDC, Fuller RA,
#' Geldmann J, Harwell H, Hilton‐Taylor C, Hoffmann M, Joolia A, Joppa L,
#' Kingston N, May I, Milam A, Polidoro B, Ralph G, Richman N, Rondinini C,
#' Segan DB, Skolnik B, Spalding MD, Stuart SN, Symes A, Taylor J, Visconti P,
#' Watson JEM, Wood L, Burgess ND (2015) Shortfalls and solutions for meeting
#' national and global conservation area targets. *Conservation Letters*,
#' 8: 329--337.
#'
#' Carwardine J, Klein CJ, Wilson KA, Pressey RL, Possingham HP (2009) Hitting
#' the target and missing the point: target‐based conservation planning in
#' context. *Conservation Letters*, 2: 4--11.
#'
#' IUCN (2025) The IUCN Red List of Threatened Species. Version 2025-1.
#' Available at <https://www.iucnredlist.org>. Accessed on 23 July 2025.
#'
#' Ward M, Possingham HP, Wintle BA, Woinarski JCZ, Marsh JR, Chapple DG,
#' Lintermans M, Scheele BC, Whiterod NS, Hoskin CJ, Aska B, Yong C, Tulloch A,
#' Stewart R, Watson JEM (2025) The estimated cost of preventing extinction and
#' progressing recovery for Australia's priority threatened species.
#' *Proceedings of the National Academy of Sciences*, 122: e2414985122.
#'
#' @examples
#' #TODO
#'
#' @family method
#'
#' @examples
#' #TODO
#'
#' @export
ward_targets <- function(status = "CR", cap_area_target = 1000000,
                         area_units = "km^2", ...) {
  UseMethod("ward_targets")
}

#' @export
ward_targets.default <- function(status = "CR",
                                 cap_area_target = 1000000,
                                 area_units = "km^2", ...) {
  # return method
  new_method(
    name = "Ward et al. (2025) targets",
    type = "relative",
    fun = internal_rl_species_targets,
    args = list(
      status = status,
      criterion_a = "A2",
      criterion_b = "B1",
      prop_uplift = 0,
      method = "max",
      cap_area_target = cap_area_target,
      area_units = area_units
    )
  )
}

#' @export
ward_targets.ConservationProblem <- function(status, ...) {
  target_problem_error("add_ward_targets")
}
