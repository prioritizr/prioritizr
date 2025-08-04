#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets following Polak *et al.* (2015)
#'
#' Specify targets following Polak *et al.* (2015).
#' This function is designed to be used within `add_auto_targets()` and
#' `add_group_targets()`.
#'
#' @inheritParams rodrigues_targets
#'
#' @param common_area_threshold `numeric` value indicating the threshold
#' area for identifying common features.
#' Defaults to 10000
#' (i.e., 10,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on the spatial
#' extent of the features.
#' By default, this method identifies rare features as those with a
#' spatial distribution smaller than 1,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} (per `rare_threshold`)
#' and common features as those with a spatial distribution
#' larger than 10,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} (per `common_threshold`).
#' Given this, rare features are assigned a target threshold
#' of 100% (per `rare_relative_target`), common features
#' are assigned a target threshold of 10% (per `common_relative_target`),
#' and features with a spatial distribution that is between
#' the area-based thresholds used to identify rare and common features are
#' assigned a target threshold through linear interpolation.
#' Additionally, following Butchart *et al.* (2015), a cap of 1,000,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} is applied to target
#' thresholds.
#'
#' @details
#' This target setting method was designed to protect species in national-
#' scale prioritizations (Polak *et al.* 2015).
#' Although it has been successfully applied to to national-scales
#' (e.g., Polak *et al.* 2016; Clements *et al.* 2018; ),
#' it may fail to identify meaningful priorities for
#' prioritizations conducted at smaller or larger geographic scales
#' (e.g., local or global scales).
#' For example, if this method is applied to
#' smaller geographic scales, then the resulting prioritizations
#' may select an overly large percentage of the study area,
#' or be biased towards over-representing common and widespread species.
#' This is because the thresholds for defining rare and common
#' features (i.e., `rare_threshold` and `cap_threshold`)
#' were originally developed based on criteria for national-scales.
#' As such, if you working at a different scale, you may need to calibrate
#' these thresholds based on the spatial extent of the planning region
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#' @inherit jung_targets return seealso
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
#' Clements HS, Kearney SG, Cook CN (2018) Moving from representation to
#' persistence: The capacity of Australia's National Reserve System to support
#' viable populations of mammals. *Diversity and Distributions*,
#' 24: 1231--1241.
#'
#' Polak T, Watson JEM, Fuller RA, Joseph LN, Martin TG, Possingham HP,
#' Venter O, Carwardine J (2015) Efficient expansion of global protected areas
#' requires simultaneous planning for species and ecosystems.
#' *Royal Society Open Science*, 2: 150107.
#'
#' Polak T, Watson JEM, Bennett JR, Possingham HP, Fuller RA, Carwardine J
#' (2016) Balancing ecosystem and threatened species representation in
#' protected areas and implications for nations achieving global conservation
#' goals. *Conservation Letters*, 9:438--445.
#'
#' UNEP-WCMC and IUCN (2025) Protected Planet Report 2024.
#' Cambridge, UK: UNEP-WCMC and IUCN. Available at <www.protectedplanet.net>.
#'
#' @examples
#' #TODO
#'
#' @export
polak_targets <- function(rare_area_threshold,
                          rare_relative_target = 1,
                          common_area_threshold = 10000,
                          common_relative_target = 0.1,
                          cap_area_target = 1000000,
                          area_units = "km^2", ...) {
  UseMethod("polak_targets")
}

#' @export
polak_targets.default <- function(rare_area_threshold = 1000,
                                  rare_relative_target = 1,
                                  common_area_threshold = 10000,
                                  common_relative_target = 0.1,
                                  cap_area_target = 1000000,
                                  area_units = "km^2", ...) {
  # assert no dots
  rlang::check_dots_empty()
  # return method
  new_method(
    name = "Polak et al. (2015) targets",
    type = "relative",
    fun = internal_interpolated_area_targets,
    args = list(
      rare_area_threshold = rare_area_threshold,
      rare_relative_target = rare_relative_target,
      rare_area_target = NA_real_,
      rare_method = "max", # has no effect
      common_area_threshold = common_area_threshold,
      common_relative_target = common_relative_target,
      common_area_target = NA_real_,
      common_method = "max", # has no effect
      cap_area_target = cap_area_target,
      interp_method = "linear",
      area_units = area_units
    )
  )
}

#' @export
polak_targets.ConservationProblem <- function(rare_area_threshold, ...) {
  target_problem_error("add_polak_targets")
}
